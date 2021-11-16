# A script that sets up as much of a new Ubuntu machine to my liking, as
# possible. Some things must be done manually afterwards.
#
# This script is intended to be run from `~`. Running elsewhere will cause
# half the script to fail.
#
# DO NOT run with super-user privileges. It will ask for escalation when
# necessary, so pay attention to the terminal when running. Running with
# `sudo` will result in commands having wrong paths, and installing
# incorrectly!



# Get Wine to emulate Windows stuff for work.
sudo apt install wine64 winetricks;

# Update Firefox to the latest version (the default that comes
# with a new Ubuntu installation is usually behind).
sudo apt install firefox --upgrade;

# Install Gnome Tweaks so that we can change the GTK theme. Preferably
# to Dracula. And then download the Dracula theme, and move it to proper
# location, to be installed through Tweaks.
sudo apt install gnome-tweaks;
mkdir /usr/share/themes;
wget "https://github.com/dracula/gtk/archive/master.zip";
unzip master.zip;
mv gtk-master /usr/share/themes/dracula;

# Open a few tabs in Firefox to remember to sign in to them.
firefox --new-tab "https://gitlab.com";
firefox --new-tab "https://drive.google.com";
firefox --new-tab "https://addons.mozilla.org/en-US/firefox/addon/darkreader/";
firefox --new-tab "https://addons.mozilla.org/en-US/firefox/addon/gnome-shell-integration/";

# Install Snap, so that we can install Alacritty, and then
# install Alacritty (terminal emulator), and GNU Screen.
sudo apt install snap;
sudo snap install alacritty --classic;
sudo apt install screen;

# Download, and install, Cascadia Code/Mono fonts.
wget "https://github.com/microsoft/cascadia-code/releases/download/v2105.24/CascadiaCode-2105.24.zip";
if [[ "$?" != 0 ]]; then
    echo "URL for Cascadia Code/Mono fonts is incorrect. Skipping.";
else
    echo "Downloaded Cascadia Code/Mono fonts.";
fi
unzip CascadiaCode-2105.24.zip;
mkdir ~/.local/share/fonts;
mv ttf/*.ttf ~/.local/share/fonts;
# Clean up files.
rm -rf ttf woff2 otf;
rm CascadiaCode*;

# Download, compile, and build Emacs from source. Ensure Git is installed.
sudo apt install git make;
cd ~;
git clone -b master git://git.sv.gnu.org/emacs.git;
cd emacs;
# Install packages for Emacs configuration.
sudo apt install libxpm-dev libjpeg-dev libgnutls28-dev libgif-dev libtiff-dev libacl1-dev libgtk-3-dev libwebkit2gtk-4.0-dev librsvg2-dev libmagickcore-dev libmagick++-dev libgpm-dev \
 libselinux1-dev libm17n-dev libotf-dev libsystemd-dev texinfo libncurses-dev;
./autogen.sh;
./configure --prefix=/home/seanld/prg/emacs --disable-silent-rules \
 --with-modules --with-file-notification=inotify --with-mailutils \
 --with-x=yes --with-x-toolkit=gtk3 --with-xwidgets --with-lcms2 \
 --with-imagemagick --with-harfbuzz;
sudo make bootstrap;
sudo make install;
# Link the newly-built Emacs binary to the bin folder, so that it can be called like a command on the terminal.
sudo ln src/emacs /usr/local/bin/emacs;
sudo ln ~/prg/emacs/bin/emacsclient /usr/local/bin/emacsclient;

# Download, and install, Spacemacs.
rm -rf ~/.emacs.d;
git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d;

cd ~/dot;
mkdir ~/.config/alacritty;
mv alacritty.yml ~/.config/alacritty/alacritty.yml;
mv screenrc ~/.screenrc;
mv spacemacs ~/.spacemacs;
rm ~/.emacs.d/init.el;
mv init.el ~/.emacs.d/init.el;

# Get the `ligature.el` package that allows Emacs to display ligatures.
wget "https://raw.githubusercontent.com/mickeynp/ligature.el/master/ligature.el";
mkdir ~/.emacs.d/lisp;
mv ligature.el ~/.emacs.d/lisp;

# Download, and install, DBeaver.
wget "https://dbeaver.io/files/21.1.2/dbeaver-ce_21.1.2_amd64.deb";
sudo apt install ./dbeaver-ce_21.1.2_amd64.deb;

# Install extra utilities / software.
sudo apt install htop glances smbclient smb-nat cifs-utils;

# Install NPM and update it.
sudo apt install npm;
sudo npm install -g npm;

# Update Node.
sudo npm install -g node;

# Install Pyright language server.
sudo npm install -g pyright;

# Install Midnight Commander (awesome file manager).
sudo apt install mc;
# Install Dracula theme for MC, then clean up download.
git clone https://github.com/dracula/midnight-commander.git dracula-mc;
mkdir -p ~/.local/share/mc/skins;
mv dracula-mc/skins/dracula.ini ~/.local/share/mc/skins;
mv dracula-mc/skins/dracula256.ini ~/.local/share/mc/skins;
rm -rf dracula-mc;

# Install Zsh, oh-my-zsh, and Git.
sudo apt install zsh;
sh -c "$(wget https://raw.github.com/ohmyzsh/ohmyzsh/master/tools/install.sh -O -)";

echo "Setup script has completed!";
