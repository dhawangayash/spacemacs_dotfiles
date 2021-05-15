#!/bin/bash
# disable touch-pad in laptop
xinput float `xinput | grep -i touchpad |  awk '{split($0,a,"id="); print substr(a[2],1,2)}'`
