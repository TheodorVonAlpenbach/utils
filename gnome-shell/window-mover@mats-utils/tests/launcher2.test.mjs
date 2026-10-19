import assert from 'node:assert/strict';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import test from 'node:test';

const launcher = fileURLToPath(new URL('../../../bin/launcher2.sh', import.meta.url));

function runLauncher(registration) {
    const harness = `
        gdbus() {
            ${registration}
        }
        gnome-terminal() { printf 'LAUNCHED TAB\\n'; }
        sleep() { ready=true; }
        export -f gdbus gnome-terminal sleep
        exec 3>&1
        bash --noprofile --norc "$1"
    `;
    // A PTY is required because the launcher deliberately refuses non-terminal output.
    return spawnSync('script', [
        '--quiet', '--return', '--command',
        'bash --noprofile --norc -c "$LAUNCHER_TEST_HARNESS" bash "$LAUNCHER_TEST_PATH"',
        '/dev/null',
    ], {
        encoding: 'utf8',
        env: {...process.env, LAUNCHER_TEST_HARNESS: harness, LAUNCHER_TEST_PATH: launcher},
        timeout: 10000,
    });
}

test('launcher requests placement before opening its five tabs', () => {
    const result = runLauncher(`
        [[ "$*" == *".RegisterAndPlaceLaptopTerminal LaptopTerminal-"* ]] || return 1
        printf 'ACKNOWLEDGED\\n' >&3
        printf '(true,)\\n'
    `);
    assert.equal(result.status, 0, result.stdout + result.stderr);
    assert.match(result.stdout, /\x1b\]0;LaptopTerminal-[0-9]+\x07/);
    assert.equal((result.stdout.match(/LAUNCHED TAB/g) ?? []).length, 5);
    assert.ok(result.stdout.indexOf('ACKNOWLEDGED') < result.stdout.indexOf('LAUNCHED TAB'));
});

test('launcher retries while the compositor has not yet seen the title', () => {
    const result = runLauncher(`
        if [[ "\${ready:-false}" == true ]]; then
            printf 'ACKNOWLEDGED\\n' >&3
            printf '(true,)\\n'
        else
            printf 'WAITING FOR TITLE\\n' >&3
            printf '(false,)\\n'
        fi
    `);
    assert.equal(result.status, 0, result.stdout + result.stderr);
    assert.ok(result.stdout.indexOf('WAITING FOR TITLE') < result.stdout.indexOf('ACKNOWLEDGED'));
    assert.ok(result.stdout.indexOf('ACKNOWLEDGED') < result.stdout.indexOf('LAUNCHED TAB'));
    assert.equal((result.stdout.match(/LAUNCHED TAB/g) ?? []).length, 5);
});

test('launcher explicitly times out without starting tabs if Shell never sees the marker', () => {
    const result = runLauncher("printf '(false,)\\n'");
    assert.equal(result.status, 1, result.stdout + result.stderr);
    assert.match(result.stdout, /Timed out waiting for GNOME Shell/);
    assert.doesNotMatch(result.stdout, /LAUNCHED TAB/);
});

test('launcher reports an unavailable or outdated extension without starting tabs', () => {
    const result = runLauncher("printf 'UnknownMethod\\n' >&2; return 1");
    assert.equal(result.status, 1, result.stdout + result.stderr);
    assert.match(result.stdout, /UnknownMethod/);
    assert.match(result.stdout, /Install\/enable or reload/);
    assert.doesNotMatch(result.stdout, /LAUNCHED TAB/);
});

test('launcher rejects an unexpected registration reply without starting tabs', () => {
    const result = runLauncher("printf 'unexpected\\n'");
    assert.equal(result.status, 1, result.stdout + result.stderr);
    assert.match(result.stdout, /Unexpected laptop-terminal registration reply/);
    assert.doesNotMatch(result.stdout, /LAUNCHED TAB/);
});

test('launcher refuses to mark a window without terminal output', () => {
    const result = spawnSync('bash', [launcher], {encoding: 'utf8'});
    assert.equal(result.status, 1);
    assert.match(result.stderr, /must run inside the second GNOME Terminal window/);
});
