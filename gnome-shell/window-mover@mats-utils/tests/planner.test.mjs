import assert from 'node:assert/strict';
import test from 'node:test';

import {
    laptopPosition,
    planPrimaryMonitor,
    planWindowMoves,
    resolveTarget,
} from '../planner.mjs';

const monitors = [
    {connector: 'eDP-1', isBuiltin: true, monitorIndex: 0},
    {connector: 'DP-7', isBuiltin: false, monitorIndex: 1},
];

function displayState() {
    const builtin = ['eDP-1', 'vendor', 'laptop', '1'];
    const external = ['DP-7', 'vendor', 'external', '2'];
    return [
        42,
        [
            [builtin, [
                ['preferred', 1920, 1080, 60, 1, [1], {'is-preferred': true}],
                ['laptop-mode', 1920, 1080, 60, 1, [1], {'is-current': true}],
            ], {'is-builtin': true, 'color-mode': 0, 'rgb-range': 1}],
            [external, [
                ['external-mode', 3440, 1440, 60, 1, [1], {'is-current': true}],
            ], {'is-underscanning': false, 'color-mode': 1, 'rgb-range': 2}],
        ],
        [
            [0, 0, 1, 0, true, [builtin], {}],
            [1920, 0, 1.25, 1, false, [external], {}],
        ],
        {'layout-mode': 1, 'supports-changing-layout-mode': true},
    ];
}

test('makes only the selected display primary while preserving the current configuration', () => {
    const state = displayState();
    const original = structuredClone(state);
    assert.deepEqual(planPrimaryMonitor(state, 'DP-7'), {
        serial: 42,
        logicalMonitors: [
            [0, 0, 1, 0, false, [['eDP-1', 'laptop-mode', {'color-mode': 0, 'rgb-range': 1}]]],
            [1920, 0, 1.25, 1, true, [
                ['DP-7', 'external-mode', {underscanning: false, 'color-mode': 1, 'rgb-range': 2}],
            ]],
        ],
        properties: {'layout-mode': 1},
    });
    assert.deepEqual(state, original);
});

test('does not reapply display settings when the external display is already primary', () => {
    const state = displayState();
    state[2][0][4] = false;
    state[2][1][4] = true;
    assert.equal(planPrimaryMonitor(state, 'DP-7'), null);
});

test('requires an active connector and a known current mode before changing display settings', () => {
    const state = displayState();
    assert.throws(() => planPrimaryMonitor(state, 'HDMI-1'), /not connected and active/);
    state[1][0][1][1][6] = {};
    assert.throws(() => planPrimaryMonitor(state, 'DP-7'), /current mode.*eDP-1/);
});

test('preserves mirrored external connectors and leaves other logical monitors non-primary', () => {
    const state = displayState();
    const mirror = ['HDMI-1', 'vendor', 'mirror', '3'];
    state[1].push([mirror, [
        ['mirror-mode', 3440, 1440, 60, 1, [1], {'is-current': true}],
    ], {}]);
    state[2][1][5].push(mirror);
    const plan = planPrimaryMonitor(state, 'HDMI-1');
    assert.deepEqual(plan.logicalMonitors.map(logical => logical[4]), [false, true]);
    assert.deepEqual(plan.logicalMonitors[1][5][1], ['HDMI-1', 'mirror-mode', {}]);
});

test('preserves leased monitors and does not enable inactive monitors', () => {
    const state = displayState();
    const leased = ['DP-8', 'vendor', 'leased', '4'];
    state[1].push([leased, [], {'is-for-lease': true}]);
    state[1].push([['HDMI-1', 'vendor', 'inactive', '5'], [], {}]);
    const plan = planPrimaryMonitor(state, 'DP-7');
    assert.equal(plan.logicalMonitors.length, 2);
    assert.deepEqual(plan.properties['monitors-for-lease'], [leased]);
});

test('omits layout-mode when changing it is unsupported', () => {
    const state = displayState();
    delete state[3]['supports-changing-layout-mode'];
    assert.deepEqual(planPrimaryMonitor(state, 'DP-7').properties, {});
});

test('automatically chooses the only active external logical monitor', () => {
    assert.deepEqual(resolveTarget(monitors), {
        connector: 'DP-7',
        monitorIndex: 1,
        laptopMonitorIndex: 0,
    });
});

test('returns a no-op target when no external monitor is active', () => {
    assert.deepEqual(resolveTarget([monitors[0]]), {
        connector: '',
        monitorIndex: null,
        laptopMonitorIndex: 0,
    });
});

test('requires an override when multiple external monitors are active', () => {
    assert.throws(() => resolveTarget([
        ...monitors,
        {connector: 'HDMI-1', isBuiltin: false, monitorIndex: 2},
    ]), /specify --monitor CONNECTOR/);
});

test('deduplicates external connectors mirrored onto one logical monitor', () => {
    assert.equal(resolveTarget([
        ...monitors,
        {connector: 'HDMI-1', isBuiltin: false, monitorIndex: 1},
    ]).monitorIndex, 1);
});

test('accepts only an active external connector override', () => {
    assert.equal(resolveTarget(monitors, 'DP-7').monitorIndex, 1);
    assert.throws(() => resolveTarget(monitors, 'HDMI-1'), /not connected and active/);
    assert.throws(() => resolveTarget(monitors, 'eDP-1'), /built-in, not an external target/);
});

test('rejects mirrored external targets that are the built-in logical monitor', () => {
    assert.throws(() => resolveTarget([
        {connector: 'eDP-1', isBuiltin: true, monitorIndex: 0},
        {connector: 'HDMI-1', isBuiltin: false, monitorIndex: 0},
    ], 'HDMI-1'), /mirrored with the built-in display/);
});

test('moves windows to the external display and positions the tagged terminal on laptop', () => {
    const plan = planWindowMoves([
        {id: 1, monitorIndex: 0, frame: {x: 10, y: 40, width: 800, height: 600}},
        {
            id: 2,
            isLaptopTerminal: true,
            monitorIndex: 1,
            frame: {x: 1950, y: 40, width: 1000, height: 700},
        },
        {id: 3, monitorIndex: 1, frame: {x: 2000, y: 50, width: 800, height: 600}},
    ], {monitorIndex: 1, laptopMonitorIndex: 0}, {
        x: 0, y: 24, width: 1920, height: 1056,
    });

    assert.deepEqual(plan.map(item => [item.id, item.action, item.position]), [
        [1, 'move', null],
        [2, 'position', {x: 500, y: 44}],
    ]);
});

test('does not plan changes for windows already at their destinations', () => {
    assert.deepEqual(planWindowMoves([
        {monitorIndex: 0, frame: {x: 500, y: 44, width: 1000, height: 700}, isLaptopTerminal: true},
        {monitorIndex: 1, frame: {x: 2000, y: 50, width: 800, height: 600}},
    ], {monitorIndex: 1, laptopMonitorIndex: 0}, {
        x: 0, y: 24, width: 1920, height: 1056,
    }), []);
});

test('keeps only the second terminal on the laptop regardless of monitor index ordering', () => {
    const target = resolveTarget([
        {connector: 'DP-7', isBuiltin: false, monitorIndex: 0},
        {connector: 'eDP-1', isBuiltin: true, monitorIndex: 1},
    ]);
    const frame = {x: 30, y: 40, width: 800, height: 600};
    const plan = planWindowMoves([
        {id: 'first-terminal', monitorIndex: 1, frame},
        {id: 'second-terminal', isLaptopTerminal: true, monitorIndex: 0, frame},
        {id: 'browser', monitorIndex: 1, frame},
        {id: 'editor', monitorIndex: 1, frame},
    ], target, {x: 3440, y: 0, width: 1920, height: 1080});
    assert.deepEqual(plan.map(item => [item.id, item.destinationMonitorIndex]), [
        ['first-terminal', 0],
        ['second-terminal', 1],
        ['browser', 0],
        ['editor', 0],
    ]);
});

test('does not move any windows when no external display is active', () => {
    assert.deepEqual(planWindowMoves([
        {monitorIndex: 0, frame: {x: 10, y: 40, width: 800, height: 600}},
        {monitorIndex: 0, isLaptopTerminal: true, frame: {x: 20, y: 50, width: 800, height: 600}},
    ], resolveTarget([monitors[0]]), {x: 0, y: 24, width: 1920, height: 1056}), []);
});

test('clamps a large tagged terminal within the laptop work area', () => {
    assert.deepEqual(laptopPosition({width: 2100, height: 1200}, {
        x: 0, y: 24, width: 1920, height: 1056,
    }), {x: 0, y: 24});
});

test('requires an active built-in monitor when a tagged terminal needs positioning', () => {
    assert.throws(() => planWindowMoves([{
        isLaptopTerminal: true,
        monitorIndex: 2,
        frame: {x: 0, y: 0, width: 800, height: 600},
    }], {monitorIndex: 1, laptopMonitorIndex: null}, null), /no built-in monitor is active/);
});
