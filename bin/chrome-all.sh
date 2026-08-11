#!/bin/bash

google-chrome-stable \
  --user-data-dir="$HOME/.config/chrome-mats" \
  --restore-last-session \
  --class=chrome-mats &

google-chrome-stable \
  --user-data-dir="$HOME/.config/chrome-bouvet" \
  --restore-last-session \
  --class=chrome-bouvet &

google-chrome-stable \
  --user-data-dir="$HOME/.config/chrome-cappelendamm" \
  --restore-last-session \
  --class=chrome-cappelendamm &

google-chrome-stable \
  --user-data-dir="$HOME/.config/chrome-pu" \
  --restore-last-session \
  --class=chrome-pu &

#google-chrome-stable --profile-directory="Profile 3" --class=chrome-pu &
