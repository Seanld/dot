***NOTE: My setup scripts (`newbuntu.sh` and the ones for Arch) are
currently broken. Until I fix them, you'll have to manually copy
the config files to their correct destinations.***

My personal configuration files for the various Linux-based OSs
that I predominantly use.

To set up a **new Ubuntu 20.04** system, run `sudo bash newbuntu.sh`
in the terminal in a fresh install of Ubuntu 20.04. Should also work
for Ubuntu-based OSs as well, like Xubuntu and Lubuntu, as there is
no GNOME-specific code in the script.

To set up a **new Arch** system, boot live from the USB (archiso),
and run `cd` to get into root's home directory. Then, clone this
repository. Then, run `bash archinstall.sh` in the `dot/arch` directory.
Once that has completed, and you're chrooted into the new system,
run `cd` again, and then clone the repo there as well, and run
`bash newarch.sh` in the `dot/arch` directory. DON'T MESS UP on the
prompts, because there's no error checking yet.
