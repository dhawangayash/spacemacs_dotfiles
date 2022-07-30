#!/bin/bash
# This script sets sticky keys
# Shortcut for this is <C-M-S s>

#URL: https://wiki.ubuntu.com/Accessibility/doc/Guide/Mobility#example
#URL: https://askubuntu.com/questions/120010/how-do-i-turn-on-sticky-keys-from-the-cli
xkbset exp -bell -sticky -twokey -latchlock -accessx -feedback -stickybeep -led 9999

# If you would prefer pressing Shift twice to mean "hold down shift" then change '-latchlock' above to 'latchlock'

xkbset bell sticky -twokey -latchlock feedback led stickybeep
