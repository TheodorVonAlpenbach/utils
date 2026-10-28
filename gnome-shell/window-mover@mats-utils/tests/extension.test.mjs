import assert from 'node:assert/strict';
import {registerHooks} from 'node:module';
import test from 'node:test';

registerHooks({
    resolve(specifier, context, nextResolve) {
        if (specifier.startsWith('gi://') || specifier.startsWith('resource://'))
            return {url: `mock:${specifier}`, shortCircuit: true};
        return nextResolve(specifier, context);
    },
    load(url, context, nextLoad) {
        if (!url.startsWith('mock:'))
            return nextLoad(url, context);
        let source = 'export default {};';
        if (url === 'mock:gi://GLib') {
            source = `
                class Variant {
                    constructor(type, value) { this.type = type; this.value = value; }
                }
                class VariantType {
                    constructor(type) { this.type = type; }
                }
                export default {Variant, VariantType};
            `;
        } else if (url === 'mock:gi://Meta') {
            source = 'export default {WindowType: {NORMAL: 0}};';
        } else if (url === 'mock:gi://Shell') {
            source = `export default {WindowTracker: {
                get_default: () => ({get_window_app: () => ({
                    get_id: () => 'org.gnome.Terminal.desktop',
                })}),
            }};`;
        } else if (url.startsWith('mock:resource://')) {
            source = 'export class Extension {}';
        }
        return {format: 'module', source, shortCircuit: true};
    },
});

const {default: WindowMoverExtension} = await import('../extension.js');

function terminalWindow(title) {
    let nextSignal = 1;
    const signals = new Map();
    return {
        title,
        get_title() { return this.title; },
        get_window_type: () => 0,
        is_override_redirect: () => false,
        get_frame_rect: () => ({x: 30, y: 40, width: 800, height: 600}),
        get_stable_sequence: () => 1,
        get_monitor: () => 0,
        connect(signal, callback) {
            const id = nextSignal++;
            signals.set(id, {signal, callback});
            return id;
        },
        disconnect(id) { signals.delete(id); },
        changeTitle(value) {
            this.title = value;
            for (const {signal, callback} of [...signals.values()]) {
                if (signal === 'notify::title')
                    callback();
            }
        },
    };
}

test('acknowledges the terminal only after its unique marker is visible and retains identity', () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    const first = terminalWindow('ordinary terminal');
    const second = terminalWindow('launcher2.sh');
    global.display = {list_all_windows: () => [first, second]};
    assert.equal(extension._registerLaptopTerminal('LaptopTerminal-123'), false);
    second.changeTitle('LaptopTerminal-123');
    assert.equal(extension._registerLaptopTerminal('LaptopTerminal-123'), true);
    second.changeTitle('top');
    assert.deepEqual(extension._windows().map(window => window.isLaptopTerminal), [false, true]);
    second.changeTitle('mats@laptop: ~/ada');
    assert.equal(extension._windows()[1].isLaptopTerminal, true);
});

test('remembers the registered terminal across disable/enable, as on screen lock', async () => {
    const {default: Gio} = await import('gi://Gio');
    Object.assign(Gio, {
        BusType: {SESSION: 0},
        BusNameOwnerFlags: {NONE: 0},
        DBusNodeInfo: {new_for_xml: () => ({interfaces: [{}]})},
        bus_get_sync: () => ({register_object: () => 1, unregister_object() {}}),
        bus_own_name_on_connection: () => 1,
        bus_unown_name() {},
    });
    const first = terminalWindow('ordinary terminal');
    const second = terminalWindow('LaptopTerminal-456');
    first.get_stable_sequence = () => 1001;
    second.get_stable_sequence = () => 1002;
    global.display = {
        list_all_windows: () => [first, second],
        connect: () => 1,
        disconnect() {},
    };
    const before = new WindowMoverExtension();
    before.enable();
    assert.equal(before._registerLaptopTerminal('LaptopTerminal-456'), true);
    second.changeTitle('mats@laptop: ~/ada');
    before.disable();

    const after = new WindowMoverExtension();
    after.enable();
    assert.deepEqual(after._windows().map(window => window.isLaptopTerminal), [false, true]);
    after.disable();
});

test('registration rejects invalid or ambiguous titles and does not tag other applications', () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    extension._windows = () => [
        {title: 'LaptopTerminal-123', appId: 'editor.desktop', window: {}},
    ];
    assert.throws(() => extension._registerLaptopTerminal(''), /unique LaptopTerminal title/);
    assert.throws(() => extension._registerLaptopTerminal('LaptopTerminal'), /unique LaptopTerminal title/);
    assert.equal(extension._registerLaptopTerminal('LaptopTerminal-123'), false);
    extension._windows = () => [1, 2].map(() => ({
        title: 'LaptopTerminal-123', appId: 'org.gnome.Terminal.desktop', window: {},
    }));
    assert.throws(() => extension._registerLaptopTerminal('LaptopTerminal-123'), /refusing to guess/);
});

test('registration D-Bus method returns a typed acknowledgement', async () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    const window = {};
    extension._windows = () => [{
        title: 'LaptopTerminal-123', appId: 'org.gnome.Terminal.desktop', window,
    }];
    let reply;
    await extension._handleMethod('RegisterLaptopTerminal', {
        deep_unpack: () => ['LaptopTerminal-123'],
    }, {
        return_value(value) { reply = value; },
        return_dbus_error(name, message) { assert.fail(`${name}: ${message}`); },
    });
    assert.equal(reply.type, '(b)');
    assert.deepEqual(reply.value, [true]);
    assert.equal(extension._taggedWindows.has(window), true);
});

test('title watcher still remembers a marker after tabs change the title', () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    extension._titleWatchers = new Map();
    const window = terminalWindow('ordinary shell');
    extension._watchWindowTitle(window);
    window.changeTitle('LaptopTerminal-123');
    window.changeTitle('top');
    assert.equal(extension._taggedWindows.has(window), true);
    assert.equal(extension._titleWatchers.size, 0);
});

test('launch handshake moves only the second terminal before acknowledging registration', async () => {
    const {extension, calls, moves} = setup({primary: true});
    extension._taggedWindows = new WeakSet();
    extension._windows = WindowMoverExtension.prototype._windows.bind(extension);
    const first = terminalWindow('first terminal');
    const second = terminalWindow('LaptopTerminal-123');
    second.get_monitor = () => 1;
    second.get_workspace = () => ({
        get_work_area_for_monitor(index) {
            assert.equal(index, 0);
            return {x: 100, y: 50, width: 1920, height: 1080};
        },
    });
    first.move_to_monitor = () => assert.fail('Must not move the first terminal');
    second.move_to_monitor = index => moves.push(['monitor', index]);
    second.move_frame = (_userOp, x, y) => moves.push(['frame', x, y]);
    global.display = {list_all_windows: () => [first, second]};
    await extension._handleMethod('RegisterAndPlaceLaptopTerminal', {
        deep_unpack: () => ['LaptopTerminal-123'],
    }, {
        return_value(reply) {
            assert.equal(reply.type, '(b)');
            assert.deepEqual(reply.value, [true]);
            assert.deepEqual(moves, [['monitor', 0], ['frame', 600, 70]]);
        },
        return_dbus_error(name, message) { assert.fail(`${name}: ${message}`); },
    });
    assert.deepEqual(calls.map(call => call.method), ['GetCurrentState']);
    second.changeTitle('top');
    assert.equal(extension._windows()[1].isLaptopTerminal, true);
});

test('launch placement works without external monitors and leaves an already positioned window alone', async () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    const terminal = terminalWindow('LaptopTerminal-123');
    terminal.get_frame_rect = () => ({x: 500, y: 20, width: 800, height: 600});
    terminal.get_workspace = () => ({
        get_work_area_for_monitor: () => ({x: 0, y: 0, width: 1920, height: 1080}),
    });
    terminal.move_to_monitor = () => assert.fail('Already on laptop');
    terminal.move_frame = () => assert.fail('Already positioned');
    global.display = {list_all_windows: () => [terminal]};
    extension._connectedMonitors = async () => [
        {connector: 'eDP-1', isBuiltin: true, monitorIndex: 0},
    ];
    assert.equal(await extension._registerAndPlaceLaptopTerminal('LaptopTerminal-123'), true);
});

test('launch placement waits for a title match and fails explicitly without a built-in display', async () => {
    const extension = new WindowMoverExtension();
    extension._taggedWindows = new WeakSet();
    extension._windows = () => [];
    extension._connectedMonitors = async () => {
        assert.fail('Should not query displays before finding the terminal');
    };
    assert.equal(await extension._registerAndPlaceLaptopTerminal('LaptopTerminal-123'), false);
    extension._windows = () => [{
        title: 'LaptopTerminal-123', appId: 'org.gnome.Terminal.desktop', window: {},
    }];
    extension._connectedMonitors = async () => [
        {connector: 'DP-7', isBuiltin: false, monitorIndex: 0},
    ];
    await assert.rejects(extension._registerAndPlaceLaptopTerminal('LaptopTerminal-123'),
        /no built-in monitor is active/);
});

function setup({primary = false, windows = []} = {}) {
    const builtin = ['eDP-1', 'vendor', 'laptop', '1'];
    const external = ['DP-7', 'vendor', 'external', '2'];
    const state = [
        42,
        [
            [builtin, [['laptop-mode', 1920, 1080, 60, 1, [1], {'is-current': true}]],
                {'is-builtin': true}],
            [external, [['external-mode', 3440, 1440, 60, 1, [1], {'is-current': true}]], {}],
        ],
        [
            [0, 0, 1, 0, !primary, [builtin], {}],
            [1920, 0, 1, 0, primary, [external], {}],
        ],
        {},
    ];
    const index = connector => state[2].findIndex(logical =>
        logical[5].some(spec => spec[0] === connector));
    global.backend = {get_monitor_manager: () => ({get_monitor_for_connector: index})};
    global.workspace_manager = {get_active_workspace: () => ({
        get_work_area_for_monitor: monitorIndex => {
            assert.equal(monitorIndex, index('eDP-1'));
            return {x: 0, y: primary ? 0 : 24, width: 1920, height: 1056};
        },
    })};
    const extension = new WindowMoverExtension();
    const calls = [];
    const moves = [];
    extension._windows = () => windows.map((item, id) => ({
        id,
        appId: 'test-app',
        frame: {x: 1950, y: 40, width: 800, height: 600},
        ...item,
        monitorIndex: index(item.connector),
        window: {
            get_monitor: () => index(item.connector),
            move_to_monitor: destination => moves.push([id, 'monitor', destination]),
            move_frame: (_userOp, x, y) => moves.push([id, 'frame', x, y]),
        },
    }));
    extension._callDisplayConfig = async (method, parameters) => {
        calls.push({method, parameters});
        if (method === 'GetCurrentState')
            return state;
        assert.equal(method, 'ApplyMonitorsConfig');
        // Simulate Mutter renumbering monitors after the primary change.
        state[2][0][4] = false;
        state[2][1][4] = true;
        state[2].reverse();
        primary = true;
        return [];
    };
    return {extension, calls, moves};
}

test('registered second terminal moves to the laptop after its title changes; first moves external', async () => {
    const {extension, moves} = setup({primary: true});
    extension._taggedWindows = new WeakSet();
    extension._windows = WindowMoverExtension.prototype._windows.bind(extension);
    const first = terminalWindow('first terminal');
    const second = terminalWindow('LaptopTerminal-123');
    first.get_monitor = () => 0;
    second.get_monitor = () => 1;
    first.get_stable_sequence = () => 1;
    second.get_stable_sequence = () => 2;
    for (const [name, window] of [['first', first], ['second', second]]) {
        window.move_to_monitor = index => moves.push([name, 'monitor', index]);
        window.move_frame = (_userOp, x, y) => moves.push([name, 'frame', x, y]);
    }
    global.display = {list_all_windows: () => [first, second]};
    assert.equal(extension._registerLaptopTerminal('LaptopTerminal-123'), true);
    second.changeTitle('mats@laptop: ~/ada');
    await extension._moveWindows('', false);
    assert.deepEqual(moves, [
        ['first', 'monitor', 1],
        ['second', 'monitor', 0],
        ['second', 'frame', 500, 20],
    ]);
});

test('dry-run reports a primary change even with no windows and never applies settings', async () => {
    const {extension, calls, moves} = setup();
    const lines = await extension._moveWindows('', true);
    assert.match(lines.join('\n'), /Would make DP-7 the primary display/);
    assert.deepEqual(calls.map(call => call.method), ['GetCurrentState']);
    assert.deepEqual(moves, []);
});

test('list reports the current primary without changing display settings', async () => {
    const {extension, calls, moves} = setup();
    const lines = await extension._listWindows('');
    assert.match(lines[0], /eDP-1.*primary/);
    assert.doesNotMatch(lines[1], /primary/);
    assert.deepEqual(calls.map(call => call.method), ['GetCurrentState']);
    assert.deepEqual(moves, []);
});

test('saves the external primary even when there are no window moves', async () => {
    const {extension, calls} = setup();
    const lines = await extension._moveWindows('', false);
    assert.match(lines.join('\n'), /Made DP-7 primary/);
    assert.deepEqual(calls.map(call => call.method), [
        'GetCurrentState', 'ApplyMonitorsConfig', 'GetCurrentState',
    ]);
    const parameters = calls[1].parameters;
    assert.equal(parameters.type, '(uua(iiduba(ssa{sv}))a{sv})');
    assert.equal(parameters.value[0], 42);
    assert.equal(parameters.value[1], 2);
    assert.deepEqual(parameters.value[2].map(logical => logical[4]), [false, true]);
});

test('replans window destinations and laptop work area after changing primary', async () => {
    const {extension, moves} = setup({windows: [
        {connector: 'eDP-1'},
        {connector: 'DP-7', isLaptopTerminal: true},
    ]});
    await extension._moveWindows('', false);
    assert.deepEqual(moves, [
        [0, 'monitor', 0],
        [1, 'monitor', 1],
        [1, 'frame', 500, 20],
    ]);
});

test('does not reapply configuration when the external monitor is already primary', async () => {
    const {extension, calls} = setup({primary: true});
    await extension._moveWindows('', false);
    assert.deepEqual(calls.map(call => call.method), ['GetCurrentState']);
});

test('propagates display configuration errors without moving windows', async () => {
    const {extension, moves} = setup({windows: [{connector: 'eDP-1'}]});
    const call = extension._callDisplayConfig;
    extension._callDisplayConfig = async (method, parameters) => {
        if (method === 'ApplyMonitorsConfig')
            throw new Error('Display configuration denied');
        return call(method, parameters);
    };
    await assert.rejects(extension._moveWindows('', false), /Display configuration denied/);
    assert.deepEqual(moves, []);
});
