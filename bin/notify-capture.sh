#! /bin/sh

last=""

dbus-monitor "type='method_call',interface='org.freedesktop.Notifications'" \
| grep --line-buffered --color=never -oP 'ode \K[0-9]+(?= for Microsoft)|PASSCODE: \K[a-z0-9]+' \
| while IFS= read -r id; do
    if [ "$id" != "$last" ]; then
	printf "%s" "$id" | xclip -selection clipboard
        echo "[COPIED] $id"
        last="$id"
    fi
done
