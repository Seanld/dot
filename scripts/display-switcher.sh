#!/usr/bin/env bash

# Facilitates quick-switching of display input source, for
# use with GPU-passthrough VMs in libvirt. No need for a
# KVM switch baby, anymore!

# Display 1:
# 0x01: VGA (unused)
# 0x11: HDMI (host)

# Display 2:
# 0x01: VGA (host)
# 0x11: HDMI (VM)

if [[ "$1" == "toggle" ]]; then
    if [[ $(ddcutil -d 2 getvcp 0x60 | grep -Po -m1 "(?<=sl=)0x\d+") == "0x11" ]]; then
        ddcutil -d 2 setvcp 0x60 0x01
    else
        ddcutil -d 2 setvcp 0x60 0x11
    fi
elif [[ "$1" == "set" ]]; then
    if [[ "$2" == "host" ]]; then
        ddcutil -d 2 setvcp 0x60 0x01
    elif [[ "$2" == "vm" ]]; then
        ddcutil -d 2 setvcp 0x60 0x11
    fi
fi
