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
        } else if (url.startsWith('mock:resource://')) {
            source = 'export class Extension {}';
        }
        return {format: 'module', source, shortCircuit: true};
    },
});

const {default: WindowMoverExtension} = await import('../extension.js');

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
