#!/bin/bash

set -euo pipefail

adadir=/home/mats/ada
destination=org.gnome.Shell.Extensions.WindowMover
object_path=/org/gnome/Shell/Extensions/WindowMover
marker="LaptopTerminal-$$"
registration_attempts=50
registration_interval=0.1

if [[ ! -t 1 ]]; then
  echo "launcher2.sh must run inside the second GNOME Terminal window." >&2
  exit 1
fi
if ! command -v gdbus >/dev/null 2>&1; then
  echo "gdbus is required to identify the laptop terminal to the window-mover extension." >&2
  exit 1
fi

# Wait for Shell to identify and place this window before tabs replace its title.
printf '\033]0;%s\007' "$marker"
registered=false
for ((attempt = 0; attempt < registration_attempts; attempt++)); do
  if ! reply=$(gdbus call --session \
    --dest "$destination" \
    --object-path "$object_path" \
    --method "$destination.RegisterAndPlaceLaptopTerminal" "$marker" 2>&1); then
    printf 'Cannot register/place the laptop terminal: %s\nCheck that the built-in display is active. Install/enable or reload the window-mover extension; see gnome-shell/window-mover@mats-utils/README.md.\n' \
      "$reply" >&2
    exit 1
  fi
  case "$reply" in
    '(true,)')
      registered=true
      break
      ;;
    '(false,)')
      sleep "$registration_interval"
      ;;
    *)
      printf 'Unexpected laptop-terminal registration reply: %s\n' "$reply" >&2
      exit 1
      ;;
  esac
done
if [[ "$registered" != true ]]; then
  echo "Timed out waiting for GNOME Shell to recognize the laptop terminal; no tabs were launched." >&2
  exit 1
fi

# ; sudo openconnect --no-external-auth --csd-wrapper=/usr/libexec/openconnect/csd-wrapper.sh --user=mats.bergstrom@cappelendamm.no --useragent AnyConnect vpn.intility.no & echo 'Job finished'

gnome-terminal --tab -- bash -c "cd $adadir; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/java/pu-message-consumer; ../gradlew bootRun & echo 'Job finished.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir/java/portal-api; ../gradlew bootRun & echo 'Job finished.'; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir; top; exec bash"
gnome-terminal --tab -- bash -c "cd $adadir; watch free -h; exec bash"
