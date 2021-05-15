#!/bin/bash
# Enable touch-pad in laptop
POINTER=2; xinput reattach `xinput | grep -i touchpad |  awk '{split($0,a,"id="); print substr(a[2],1,2)}'` ${POINTER}
