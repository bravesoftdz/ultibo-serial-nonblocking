#!/bin/bash
set -ex

# on raspbian, build the program and reboot to it

ULTIBO=$HOME/ultibo/core
ULTIBOBIN=$ULTIBO/fpc/bin
LPR=BLETest.lpr
export PATH=$ULTIBOBIN:$PATH
rm -rf lib/

fpc -B -O2 -Tultibo -Parm -CpARMV7a -WpRPI3B -Fi$ULTIBO/source/rtl/ultibo/core @$ULTIBOBIN/RPI3.CFG $LPR # >& errors.log

sudo cp BCM43430A1.hcd /boot
sudo cp kernel7.img /boot/test-kernel7.img
sudo cp test-config.txt test-cmdline.txt /boot
sudo cp /boot/test-config.txt /boot/config.txt
sudo reboot
