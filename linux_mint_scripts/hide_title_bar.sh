#!/bin/bash
# hides the title bar
# Shortcut Keys: Shift+Alt+W
xprop -f _MOTIF_WM_HINTS 32c -set _MOTIF_WM_HINTS "0x2, 0x0, 0x2, 0x0, 0x0"
