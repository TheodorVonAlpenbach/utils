---
applyTo: "gnome-shell/**/*.js, gnome-shell/**/*.mjs"
---

# GNOME Shell extension standards

These extensions exist to reach GNOME Shell/Mutter APIs (window management, monitor
configuration, etc.) that are not accessible to ordinary scripts under Wayland. See
`.github/instructions/shell-scripting.instructions.md` for why this is needed.

## Structure

- Expose only a narrow, purpose-built D-Bus interface from `enable()` — a small number of
  named methods with typed arguments, not a generic "eval arbitrary JS" method. Any session
  process can call an exported D-Bus method, so keep the surface area small and side-effect
  behavior explicit (e.g. a dedicated `dry_run` argument rather than inferring intent).
- Always clean up what `enable()` registered — D-Bus name ownership, object registration,
  signal connections — in `disable()`. Extensions can be disabled and re-enabled within a
  session without a restart.
- Keep pure decision logic (choosing a target monitor, planning window moves, computing a
  position) in plain `.mjs` modules with no GNOME/GJS imports, separate from the `extension.js`
  glue code that talks to `Meta`/`Shell`/`Gio`. This is what makes the logic testable with
  plain `node --test`, without needing a running GNOME Shell.

## GJS/D-Bus specifics

- Never use a synchronous, blocking D-Bus call (e.g. `Gio.DBus.session.call_sync`) from
  inside GNOME Shell's own process — GNOME Shell and Mutter share one main loop, so a
  blocking call stalls the compositor. Use the asynchronous `call()`/`call_finish()` pair
  (or `Gio._promisify`) and `await` it instead.
- Use `GLib.Variant.recursiveUnpack()` (not `deep_unpack()`) when a reply contains nested
  variants (e.g. an `a{sv}` properties dictionary), or nested values will come back as
  `GLib.Variant` objects instead of plain JS values.
- Verify the exact Mutter/Shell API surface (method names, signatures, enum values) against
  the GNOME Shell version actually installed (`gnome-shell --version`), e.g. with
  `gjs -c 'print(typeof Meta.Window.prototype.someMethod)'`, or against the upstream Mutter
  docs for that version. Do not assume an API from older GNOME versions or blog posts still
  applies.
- Filter `global.display.list_all_windows()` to normal, managed windows before acting on them
  (exclude override-redirect windows, and typically also windows that skip the taskbar).

## Testing

- Add or update `node --test` coverage in the extension's `tests/` directory for any change
  to its pure logic modules (target selection, window planning, position math, etc.).
- Live behavior inside GNOME Shell (actually moving a window, enabling/disabling the
  extension) cannot be verified by automated tests; call this out explicitly when such
  verification is still pending, and ask before installing, enabling, or disabling an
  extension, or moving real windows.
