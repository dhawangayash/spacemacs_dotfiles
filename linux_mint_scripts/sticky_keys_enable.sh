#!/bin/bash
# This script sets sticky keys
# Shortcut for this is <C-M-S s>

xkbset exp -bell -sticky -twokey -latchlock -accessx -feedback -stickybeep -led 9999

# If you would prefer pressing Shift twice to mean "hold down shift" then change '-latchlock' above to 'latchlock'

xkbset bell sticky -twokey -latchlock feedback led stickybeep
