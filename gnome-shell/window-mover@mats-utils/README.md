# Window Mover (GNOME Shell 50)

This extension supplies a small, session-local D-Bus API used by
`bin/windows-move-to-external-display.sh`. Unlike the old X11 helper, it uses
GNOME Shell's window and monitor APIs and works with Wayland windows.

The helper moves normal windows from every workspace, including minimized
windows, to the only active external logical monitor. If multiple external
monitors are active, pass `--monitor CONNECTOR` (for example `DP-7`) to choose
one. Mirrored connectors that represent the same logical monitor count as one
destination. If no external monitor is active, a move is a no-op.

The laptop terminal is identified by the unique initial `LaptopTerminal` title
set in `bin/setup-terminals.sh`. GNOME Shell remembers windows as they receive
that title, so later shell-title changes do not change their identity. Such a
window stays on the built-in monitor and is placed at approximately 500,20
within its work area. Windows already at their destination are left alone.
Maximized state and workspace membership are not changed.
The remembered identity lasts only while the extension is enabled. If it is
disabled and re-enabled after the terminal has changed its title, open the
special terminal again through `setup-terminals.sh`. Existing terminals
launched by the old setup script also need to be recreated.

## Install and enable

These are manual instructions; the helper does not install or enable the
extension automatically.

```sh
mkdir -p ~/.local/share/gnome-shell/extensions
ln -s /home/mats/git/utils/gnome-shell/window-mover@mats-utils \
  ~/.local/share/gnome-shell/extensions/window-mover@mats-utils
```

On Wayland, log out and back in so GNOME Shell discovers the new extension,
then enable it:

```sh
gsettings set org.gnome.shell disable-user-extensions false
gnome-extensions enable window-mover@mats-utils
```

The extension requires GNOME Shell 50 and the `gdbus` command.
Enable the extension before the next run of `bin/setup-terminals.sh` so the
extension can remember the laptop terminal window when it receives its
initial title. The `gsettings` command above allows user extensions; it does
not individually enable other extensions.

## Use

```sh
bin/windows-move-to-external-display.sh --list
bin/windows-move-to-external-display.sh --dry-run
bin/windows-move-to-external-display.sh --monitor DP-7
```

With no options, the helper moves windows when the target is unambiguous.
`--list` prints active monitor mappings and normal windows. `--dry-run` prints
the planned moves without changing any windows. `--monitor` is also accepted
with either inspection option. This helper does not launch terminals or
browsers; `bin/setup-terminals.sh`, the two launcher scripts, and
`bin/chrome-all.sh` retain their launch behavior.

## Checks

The display-moving operation has not been exercised live; enabling the
extension and moving existing windows are intentionally left to the user.
Pure target-selection and movement planning tests can be run with:

```sh
node --test gnome-shell/window-mover@mats-utils/tests/planner.test.mjs
```
