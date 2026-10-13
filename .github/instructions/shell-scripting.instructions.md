---
applyTo: "bin/**/*.sh"
---

# Shell scripting standards

## Wayland and desktop automation

GNOME on Wayland does not let ordinary processes query or move arbitrary windows belonging to
other applications. Tools that relied on X11 (`wmctrl`, `xdotool`, `xprop`, direct `xrandr`
geometry math) only see XWayland windows and will silently miss or mishandle native Wayland
windows. For any script that needs to enumerate, move, resize, or otherwise manipulate
windows across applications:

- Prefer a narrow GNOME Shell extension exposing a session D-Bus API, as done in
  `gnome-shell/`, over X11 tools. See
  `.github/instructions/gnome-shell-extensions.instructions.md` for how to build one.
- Do not hardcode monitor offsets, resolutions, or connector names (e.g. a fixed
  `OFFSET_X=1920`). Resolve monitors dynamically (e.g. via
  `org.gnome.Mutter.DisplayConfig.GetCurrentState` or the extension's own API) and fail
  explicitly, rather than guessing, when the target is ambiguous.
- Identifying windows by `WM_CLASS`, `--class`, `--name`, or `--role` is unreliable on
  Wayland for GTK/Electron apps that launch a single shared server process (e.g.
  `gnome-terminal`). Use a title marker, a dedicated `--app-id`/instance mechanism, or the
  extension's own window tracking instead, and verify the chosen mechanism actually works
  under the installed GNOME/app version before relying on it.

## General script conventions

- Use `#!/bin/bash` as the shebang for new or substantially rewritten scripts.
- Prefer `set -euo pipefail` in new or substantially rewritten scripts, unless the script
  intentionally continues past failing commands (and if so, say why in a comment).
- Keep adjustable values (paths, connector names, excluded classes, etc.) as clearly named
  variables near the top of the script rather than inline magic values.
- Scripts that call into a GNOME Shell extension (see `gnome-shell/`) should fail with an
  actionable message (e.g. pointing at the extension's README) when the extension is not
  installed or enabled, rather than failing with a raw D-Bus error.
- Run `shellcheck` on new or substantially rewritten scripts where it is available.
