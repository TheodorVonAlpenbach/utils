import Gio from 'gi://Gio';
import GLib from 'gi://GLib';
import Meta from 'gi://Meta';
import Shell from 'gi://Shell';
import {Extension} from 'resource:///org/gnome/shell/extensions/extension.js';

import {
    LAPTOP_TERMINAL_TITLE,
    planPrimaryMonitor,
    planWindowMoves,
    resolveTarget,
} from './planner.mjs';

const BUS_NAME = 'org.gnome.Shell.Extensions.WindowMover';
const OBJECT_PATH = '/org/gnome/Shell/Extensions/WindowMover';
const INTERFACE_NAME = 'org.gnome.Shell.Extensions.WindowMover';
const DISPLAY_CONFIG_NAME = 'org.gnome.Mutter.DisplayConfig';
const DISPLAY_CONFIG_PATH = '/org/gnome/Mutter/DisplayConfig';
const DISPLAY_CONFIG_INTERFACE = 'org.gnome.Mutter.DisplayConfig';

const DBUS_XML = `
<node>
  <interface name="${INTERFACE_NAME}">
    <method name="ListWindows">
      <arg name="monitor_override" type="s" direction="in"/>
      <arg name="lines" type="as" direction="out"/>
    </method>
    <method name="MoveWindows">
      <arg name="monitor_override" type="s" direction="in"/>
      <arg name="dry_run" type="b" direction="in"/>
      <arg name="lines" type="as" direction="out"/>
    </method>
  </interface>
</node>`;

export default class WindowMoverExtension extends Extension {
    enable() {
        this._enabled = true;
        this._taggedWindows = new WeakSet();
        this._titleWatchers = new Map();
        this._windowCreatedId = global.display.connect('window-created',
            (_display, window) => this._watchWindowTitle(window));
        for (const window of global.display.list_all_windows())
            this._watchWindowTitle(window);

        this._connection = Gio.bus_get_sync(Gio.BusType.SESSION, null);
        this._interfaceInfo = Gio.DBusNodeInfo.new_for_xml(DBUS_XML).interfaces[0];
        this._registrationId = this._connection.register_object(
            OBJECT_PATH,
            this._interfaceInfo,
            (_connection, _sender, _path, _interface, methodName,
                parameters, invocation) => {
                void this._handleMethod(methodName, parameters, invocation);
            },
            null,
            null);
        this._nameOwnerId = Gio.bus_own_name_on_connection(
            this._connection,
            BUS_NAME,
            Gio.BusNameOwnerFlags.NONE,
            null,
            null);
    }

    disable() {
        this._enabled = false;
        if (this._nameOwnerId) {
            Gio.bus_unown_name(this._nameOwnerId);
            this._nameOwnerId = 0;
        }

        if (this._registrationId) {
            this._connection.unregister_object(this._registrationId);
            this._registrationId = 0;
        }

        if (this._connection) {
            this._connection = null;
        }

        if (this._windowCreatedId) {
            global.display.disconnect(this._windowCreatedId);
            this._windowCreatedId = 0;
        }

        for (const [window, ids] of this._titleWatchers) {
            window.disconnect(ids.title);
            window.disconnect(ids.unmanaged);
        }
        this._titleWatchers.clear();
        this._taggedWindows = null;
    }

    async _handleMethod(methodName, parameters, invocation) {
        try {
            const args = parameters.deep_unpack();
            let lines;
            if (methodName === 'ListWindows')
                lines = await this._listWindows(args[0]);
            else if (methodName === 'MoveWindows')
                lines = await this._moveWindows(args[0], args[1]);
            else
                throw new Error(`Unsupported method: ${methodName}`);

            invocation.return_value(new GLib.Variant('(as)', [lines]));
        } catch (error) {
            invocation.return_dbus_error(
                `${INTERFACE_NAME}.Error.Failed`,
                error.message ?? String(error));
        }
    }

    _watchWindowTitle(window) {
        const tagIfMatching = () => {
            const title = window.get_title() ?? '';
            if (!title.startsWith(LAPTOP_TERMINAL_TITLE))
                return false;

            this._taggedWindows.add(window);
            return true;
        };

        if (tagIfMatching())
            return;

        let titleId = 0;
        let unmanagedId = 0;
        const cleanup = () => {
            if (titleId)
                window.disconnect(titleId);
            if (unmanagedId)
                window.disconnect(unmanagedId);
            this._titleWatchers.delete(window);
        };

        titleId = window.connect('notify::title', () => {
            if (tagIfMatching())
                cleanup();
        });
        unmanagedId = window.connect('unmanaged', cleanup);
        this._titleWatchers.set(window, {title: titleId, unmanaged: unmanagedId});
    }

    async _callDisplayConfig(method, parameters, replyType) {
        // Mutter shares Shell's main loop, so a synchronous call would block its reply.
        const reply = await new Promise((resolve, reject) => {
            Gio.DBus.session.call(
                DISPLAY_CONFIG_NAME,
                DISPLAY_CONFIG_PATH,
                DISPLAY_CONFIG_INTERFACE,
                method,
                parameters,
                replyType,
                Gio.DBusCallFlags.NONE,
                5000,
                null,
                (connection, result) => {
                    try {
                        resolve(connection.call_finish(result));
                    } catch (error) {
                        reject(error);
                    }
                });
        });
        if (!this._enabled)
            throw new Error('Window-mover extension was disabled during the request.');
        return reply.recursiveUnpack();
    }

    async _displayState() {
        return this._callDisplayConfig('GetCurrentState', null, new GLib.VariantType(
            '(ua((ssss)a(siiddada{sv})a{sv})a(iiduba(ssss)a{sv})a{sv})'));
    }

    async _connectedMonitors(state = null) {
        state ??= await this._displayState();
        const monitorManager = global.backend.get_monitor_manager();

        return state[1].map(([spec, _modes, properties]) => ({
            connector: spec[0],
            displayName: properties['display-name'] ?? spec[0],
            isBuiltin: properties['is-builtin'] === true,
            isPrimary: state[2].some(logical =>
                logical[4] && logical[5].some(candidate => candidate[0] === spec[0])),
            monitorIndex: monitorManager.get_monitor_for_connector(spec[0]),
        }));
    }

    async _applyPrimaryMonitor(configuration) {
        const variantProperties = properties => Object.fromEntries(
            Object.entries(properties).map(([key, value]) => [
                key,
                new GLib.Variant(key === 'underscanning' ? 'b'
                    : key === 'monitors-for-lease' ? 'a(ssss)' : 'u', value),
            ]));
        const logicalMonitors = configuration.logicalMonitors.map(logical => [
            ...logical.slice(0, 5),
            logical[5].map(([connector, mode, properties]) => [
                connector, mode, variantProperties(properties),
            ]),
        ]);
        const parameters = new GLib.Variant(
            '(uua(iiduba(ssa{sv}))a{sv})',
            [configuration.serial, 2, logicalMonitors, variantProperties(configuration.properties)]);
        await this._callDisplayConfig('ApplyMonitorsConfig', parameters, new GLib.VariantType('()'));
    }

    _windows() {
        const tracker = Shell.WindowTracker.get_default();
        return global.display.list_all_windows()
            .filter(window => window.get_window_type() === Meta.WindowType.NORMAL &&
                !window.is_override_redirect() && !window.skip_taskbar)
            .map(window => {
                const app = tracker.get_window_app(window);
                const frame = window.get_frame_rect();
                const title = window.get_title() ?? '';
                return {
                    window,
                    id: window.get_stable_sequence(),
                    title,
                    appId: app?.get_id() ?? window.get_wm_class() ?? 'unknown',
                    minimized: window.minimized,
                    monitorIndex: window.get_monitor(),
                    frame: {x: frame.x, y: frame.y, width: frame.width, height: frame.height},
                    isLaptopTerminal: this._taggedWindows.has(window) ||
                        title.startsWith(LAPTOP_TERMINAL_TITLE),
                };
            });
    }

    _monitorLabel(monitors, index) {
        const names = monitors
            .filter(monitor => monitor.monitorIndex === index)
            .map(monitor => monitor.connector);
        return names.length > 0 ? names.join('+') : `monitor ${index}`;
    }

    async _listWindows(connector) {
        const monitors = await this._connectedMonitors();
        let target;
        let targetIssue = '';
        if (connector) {
            target = resolveTarget(monitors, connector);
        } else {
            try {
                target = resolveTarget(monitors);
            } catch (error) {
                targetIssue = error.message;
            }
        }

        const lines = monitors.map(monitor =>
            `MONITOR ${monitor.connector} (${monitor.displayName}) ${monitor.isBuiltin ? 'built-in' : 'external'} ` +
            `${monitor.monitorIndex < 0 ? 'inactive' : `logical ${monitor.monitorIndex}`}` +
            `${monitor.isPrimary ? ' primary' : ''}`);

        const targetLabel = connector ? 'REQUESTED TARGET' : 'AUTOMATIC TARGET';
        if (targetIssue)
            lines.push(`AUTOMATIC TARGET: ${targetIssue}`);
        else if (target.monitorIndex === null)
            lines.push(`${targetLabel}: no active external monitor`);
        else
            lines.push(`${targetLabel}: ${target.connector} (logical ${target.monitorIndex})`);

        for (const window of this._windows()) {
            const title = JSON.stringify(window.title);
            const tags = [
                window.minimized ? 'minimized' : 'normal',
                window.isLaptopTerminal ? 'LaptopTerminal' : '',
            ].filter(Boolean).join(',');
            lines.push(
                `WINDOW ${window.id} ${window.appId} [${tags}] ` +
                `${this._monitorLabel(monitors, window.monitorIndex)} ${title}`);
        }

        return lines;
    }

    async _moveWindows(connector, dryRun) {
        let state = await this._displayState();
        let monitors = await this._connectedMonitors(state);
        let target = resolveTarget(monitors, connector);
        if (target.monitorIndex === null)
            return ['No active external monitor; nothing changed.'];

        const windowPlan = () => {
            const workArea = target.laptopMonitorIndex === null
                ? null
                : global.workspace_manager.get_active_workspace()
                    .get_work_area_for_monitor(target.laptopMonitorIndex);
            return planWindowMoves(this._windows(), target, workArea);
        };
        let plan = windowPlan();
        const primaryConfiguration = planPrimaryMonitor(state, target.connector);
        const lines = [];
        if (primaryConfiguration) {
            const selectedConnector = target.connector;
            if (dryRun) {
                lines.push(`Would make ${selectedConnector} the primary display (saved configuration).`);
            } else {
                await this._applyPrimaryMonitor(primaryConfiguration);
                state = await this._displayState();
                if (planPrimaryMonitor(state, selectedConnector))
                    throw new Error(`Monitor "${selectedConnector}" did not become primary.`);

                // Changing primary can change monitor indices and the panel's work area.
                monitors = await this._connectedMonitors(state);
                target = resolveTarget(monitors, selectedConnector);
                plan = windowPlan();
                lines.push(`Made ${selectedConnector} primary; accept GNOME's display confirmation to keep it.`);
            }
        }
        if (plan.length === 0) {
            lines.push('All windows are already in their requested positions.');
            return lines;
        }

        for (const item of plan) {
            const destination = item.action === 'position'
                ? `${this._monitorLabel(monitors, item.destinationMonitorIndex)} at ${item.position.x},${item.position.y}`
                : this._monitorLabel(monitors, item.destinationMonitorIndex);
            lines.push(`${dryRun ? 'Would move' : 'Moving'} window ${item.id} (${item.appId}) to ${destination}`);
        }

        if (!dryRun) {
            for (const item of plan) {
                if (item.window.get_monitor() !== item.destinationMonitorIndex)
                    item.window.move_to_monitor(item.destinationMonitorIndex);
                if (item.position)
                    item.window.move_frame(true, item.position.x, item.position.y);
            }
            lines.push(`Moved ${plan.length} window(s).`);
        } else {
            lines.push(`Dry run: ${plan.length} window(s) would move; no changes made.`);
        }

        return lines;
    }
}
