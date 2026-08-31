#!/bin/bash

# Move all visible windows to the external monitor (adjust x-offset to
# your setup)
OFFSET_X=1920  # your laptop screen width
EXCLUDE_CLASS="LaptopTerminal" # see setup-terminals.sh

# echo $EXCLUDE_CLASS

for wid in $(wmctrl -l | awk '{print $1}'); do
  CLASS=$(xprop -id "$wid" WM_CLASS 2>/dev/null | grep -o '"[^"]*"' | tail -1 | tr -d '"')
  # echo "CLASS: $CLASS"


  if [[ "$CLASS" == "$EXCLUDE_CLASS" ]]; then
      xdotool windowmove "$wid" 500 20
    continue
  fi

  eval $(xdotool getwindowgeometry --shell "$wid")
  # echo "X: $X"
  if [ "$X" -lt "$OFFSET_X" ]; then
    xdotool windowmove "$wid" $((X + OFFSET_X)) "$Y"
  fi
done
