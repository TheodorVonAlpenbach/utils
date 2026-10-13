import assert from 'node:assert/strict';
import test from 'node:test';

import {
    laptopPosition,
    planWindowMoves,
    resolveTarget,
} from '../planner.mjs';

const monitors = [
    {connector: 'eDP-1', isBuiltin: true, monitorIndex: 0},
    {connector: 'DP-7', isBuiltin: false, monitorIndex: 1},
];

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
