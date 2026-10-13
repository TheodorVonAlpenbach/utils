#!/bin/bash

set -euo pipefail

extension_id="window-mover@mats-utils"
destination="org.gnome.Shell.Extensions.WindowMover"
object_path="/org/gnome/Shell/Extensions/WindowMover"
interface="org.gnome.Shell.Extensions.WindowMover"
operation="move"
monitor=""

usage() {
  cat <<'EOF'
Usage: windows-move-to-external-display.sh [--list | --dry-run] [--monitor CONNECTOR]

Move normal windows to the single active external monitor. If more than one
external monitor is active, choose one with --monitor. LaptopTerminal windows
stay on the built-in display. --list and --dry-run do not move any windows.
EOF
}

while (($#)); do
  case "$1" in
    --list)
      if [[ "$operation" != "move" ]]; then
        echo "--list and --dry-run cannot be combined." >&2
        exit 2
      fi
      operation="list"
      ;;
    --dry-run)
      if [[ "$operation" != "move" ]]; then
        echo "--list and --dry-run cannot be combined." >&2
        exit 2
      fi
      operation="dry-run"
      ;;
    --monitor)
      if (($# < 2)) || [[ -z "$2" || "$2" == --* ]]; then
        echo "--monitor requires a connected monitor connector, such as DP-7." >&2
        exit 2
      fi
      monitor="$2"
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown option: $1" >&2
      usage >&2
      exit 2
      ;;
  esac
  shift
done

if ! command -v gdbus >/dev/null 2>&1; then
  echo "gdbus is required to contact the GNOME Shell window-mover extension." >&2
  exit 1
fi

if [[ "$operation" == "list" ]]; then
  method="ListWindows"
  arguments=("$monitor")
else
  method="MoveWindows"
  dry_run=false
  [[ "$operation" == "dry-run" ]] && dry_run=true
  arguments=("$monitor" "$dry_run")
fi

if output=$(gdbus call --session \
  --dest "$destination" \
  --object-path "$object_path" \
  --method "$interface.$method" \
  "${arguments[@]}" 2>&1); then
  printf '%s\n' "$output"
else
  status=$?
  if [[ "$output" == *"ServiceUnknown"* ||
        "$output" == *"NameHasNoOwner"* ||
        "$output" == *"UnknownObject"* ]]; then
    printf 'Window-mover extension %s is not available. Install and enable it; see gnome-shell/window-mover@mats-utils/README.md.\n' \
      "$extension_id" >&2
  else
    printf '%s\n' "$output" >&2
  fi
  exit "$status"
fi
