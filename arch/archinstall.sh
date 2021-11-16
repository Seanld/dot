#!/bin/bash

# Download this script into the live-booted Arch (archiso)
# installer session. This script assumes you've made it to
# the point where you've just mounted the newly-made part-
# ition to `/mnt`. Run this once you have gotten to that 
# exactly to that point.

# Install the fundamental packages.
pacstrap /mnt base linux linux-firmware wget;

genfstab -U /mnt >> /mnt/etc/fstab;
echo "Chrooting into the new installation -- use the next script inside it.";
arch-chroot /mnt;
