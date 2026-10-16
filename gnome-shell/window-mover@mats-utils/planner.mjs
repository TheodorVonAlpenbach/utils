export const LAPTOP_TERMINAL_TITLE = 'LaptopTerminal';

export function planPrimaryMonitor(state, connector) {
    const [serial, monitors, logicalMonitors, properties] = state;
    const target = logicalMonitors.find(logical =>
        logical[5].some(spec => spec[0] === connector));
    if (!target)
        throw new Error(`Monitor "${connector}" is not connected and active.`);
    if (target[4])
        return null;

    const configuration = logicalMonitors.map(([x, y, scale, transform, , specs]) => [
        x, y, scale, transform, specs.some(spec => spec[0] === connector),
        specs.map(spec => {
            const monitor = monitors.find(([candidate]) => candidate[0] === spec[0]);
            const mode = monitor?.[1].find(candidate => candidate[6]['is-current']);
            if (!mode)
                throw new Error(`Cannot determine the current mode for monitor "${spec[0]}".`);

            const current = monitor[2];
            const settings = {};
            if ('is-underscanning' in current)
                settings.underscanning = current['is-underscanning'];
            for (const key of ['color-mode', 'rgb-range']) {
                if (key in current)
                    settings[key] = current[key];
            }
            return [spec[0], mode[0], settings];
        }),
    ]);
    const settings = {};
    if (properties['supports-changing-layout-mode'] && 'layout-mode' in properties)
        settings['layout-mode'] = properties['layout-mode'];
    const leased = monitors.filter(monitor => monitor[2]['is-for-lease'])
        .map(monitor => monitor[0]);
    if (leased.length > 0)
        settings['monitors-for-lease'] = leased;

    return {serial, logicalMonitors: configuration, properties: settings};
}

export function resolveTarget(monitors, connector = '') {
    const active = monitors.filter(monitor => monitor.monitorIndex >= 0);
    const builtInIndices = [...new Set(active
        .filter(monitor => monitor.isBuiltin)
        .map(monitor => monitor.monitorIndex))];

    if (builtInIndices.length > 1)
        throw new Error('More than one built-in logical monitor is active; refusing to guess the laptop display.');

    const external = active.filter(monitor =>
        !monitor.isBuiltin && !builtInIndices.includes(monitor.monitorIndex));

    if (connector) {
        const target = active.find(monitor => monitor.connector === connector);
        if (!target)
            throw new Error(`External monitor "${connector}" is not connected and active.`);
        if (target.isBuiltin)
            throw new Error(`Monitor "${connector}" is built-in, not an external target.`);
        if (builtInIndices.includes(target.monitorIndex))
            throw new Error(`External monitor "${connector}" is mirrored with the built-in display.`);

        return {
            connector: target.connector,
            monitorIndex: target.monitorIndex,
            laptopMonitorIndex: builtInIndices[0] ?? null,
        };
    }

    const byLogicalMonitor = new Map();
    for (const monitor of external) {
        if (!byLogicalMonitor.has(monitor.monitorIndex))
            byLogicalMonitor.set(monitor.monitorIndex, monitor);
    }

    if (byLogicalMonitor.size === 0)
        return {connector: '', monitorIndex: null, laptopMonitorIndex: builtInIndices[0] ?? null};

    if (byLogicalMonitor.size > 1) {
        const connectors = [...byLogicalMonitor.values()]
            .map(monitor => monitor.connector)
            .join(', ');
        throw new Error(`More than one external monitor is active (${connectors}); specify --monitor CONNECTOR.`);
    }

    const target = byLogicalMonitor.values().next().value;
    return {
        connector: target.connector,
        monitorIndex: target.monitorIndex,
        laptopMonitorIndex: builtInIndices[0] ?? null,
    };
}

export function laptopPosition(frame, workArea) {
    const maxX = Math.max(workArea.x, workArea.x + workArea.width - frame.width);
    const maxY = Math.max(workArea.y, workArea.y + workArea.height - frame.height);

    return {
        x: Math.min(Math.max(workArea.x + 500, workArea.x), maxX),
        y: Math.min(Math.max(workArea.y + 20, workArea.y), maxY),
    };
}

export function planWindowMoves(windows, target, workArea) {
    if (target.monitorIndex === null)
        return [];

    const plan = [];
    for (const window of windows) {
        if (window.isLaptopTerminal) {
            if (target.laptopMonitorIndex === null)
                throw new Error('A LaptopTerminal window exists, but no built-in monitor is active.');

            const position = laptopPosition(window.frame, workArea);
            if (window.monitorIndex !== target.laptopMonitorIndex ||
                window.frame.x !== position.x || window.frame.y !== position.y) {
                plan.push({
                    ...window,
                    destinationMonitorIndex: target.laptopMonitorIndex,
                    position,
                    action: 'position',
                });
            }
        } else if (window.monitorIndex !== target.monitorIndex) {
            plan.push({
                ...window,
                destinationMonitorIndex: target.monitorIndex,
                position: null,
                action: 'move',
            });
        }
    }

    return plan;
}
