# Set up a new installation of Arch Linux, with packages I use,
# and the personalizations I prefer.
#
# NOTE: Run this after `archinstall.sh` has chrooted you into the
# new installation.

echo "Setting time zone to MST...";
ln -sf /usr/share/zoneinfo/America/Denver /etc/localtime;
hwclock --systohc;

echo "Setting up NTP...";
timedatectl set-ntp 1;
echo;

echo "Writing & generating US UTF-8 locale...";
echo "en_US.UTF-8 UTF-8" >> /etc/locale.gen;
echo "LANG=en_US.UTF-8" >> locale.conf; # May be extraneous.
locale-gen;

printf "Give your computer a name: ";
read -r compname;
echo $compname >> /etc/hostname;
echo;

echo "Creating hosts file...";
echo "127.0.0.1 localhost" >> /etc/hosts;
echo "::1       localhost" >> /etc/hosts;
echo "127.0.0.1 $compname.localdomain $compname" >> /etc/hosts;
echo;

echo "Please set a new root password...";
passwd;
echo;

echo "Creating new user account...";
printf "Your username: ";
read -r newusername;
useradd -m $newusername -g wheel;
echo "Set a password for your new account!";
passwd $newusername;
echo;

echo "Enabling DHCPCD, to gain ethernet connection..."
pacman -S dhcpcd;
systemctl enable dhcpcd.service;
echo;

echo "Enabling multilib repository...";
echo "[multilib]\nInclude = /etc/pacman.d/mirrorlist" >> /etc/pacman.conf;
echo;

echo "Updating and syncing repositories...";
pacman -Syu;
echo;

echo "Installing important packages...";
pacman -S base-devel git eog gnome-backgrounds gnome-characters gnome-color-manager gnome-control-center gnome-font-viewer \
       gnome-keyring gnome-menus gnome-screenshot gnome-session gnome-settings-daemon gnome-shell gnome-shell-extensions \
       gnome-terminal gnome-themes-extra totem xdg-user-dirs-gtk;
echo;

echo "Installing Yay (AUR helper tool)/...";
git clone https://aur.archlinux.org/yay.git;
cd yay;
makepkg -si;
cd ..;
echo;

echo "Installing LibreWolf...";
git clone https://aur.archlinux.org/librewolf-bin.git;
cd librewolf-bin;
makepkg -si;
cd ..;
echo;

echo "Downloading, building, and installing Ly display manager...";
git clone https://github.com/nullgemm/ly;
cd ly;
make github;
make;
make install;
systemctl enable ly.service;
cd;
echo;

# Ensure we are in a directory that's not important
# (root home or user home).
cd;

echo "Downloading & installing Flatery icon theme...";
wget "https://dl2.pling.com/api/files/download/j/eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpZCI6IjE2MjY5NTk5NzgiLCJ1IjpudWxsLCJsdCI6ImRvd25sb2FkIiwicyI6IjdkNTc4NGVhNDQxODg5MTI2ODg5M2FjNWQ3YzJhZWQ4OWY1NzdhNGY2YWNjYzZhODVlZTg0ZmI4MDNjNWUzY2QzM2JiM2UxNmI1Njk3ZGEwYjJmYjVhNjJlODRmZGJmM2M5YWQ2OTY3NTgzYTE3MTg4M2NlNTczZDlkNWQ2ZjRlIiwidCI6MTYyODE5MTU0MSwic3RmcCI6ImNmYWI0MDMwMzAwODMwZjEwZTg0YWUzYTkxZTE0Yzk0Iiwic3RpcCI6IjE2Ni43MC4yMDcuMTcifQ.Kr-PYgyFyKQ_sb-hUtFSiY6z_9hHWoIW2JarxGSfRRY/Flatery-Dark.tar.gz";
tar -xvf Flatery-Dark.tar.gz;
mkdir -p /usr/share/icons; mkdir -p /usr/share/themes; # Create in case it does not exist.
mv Flatery-Dark /usr/share/icons;
chown /usr/share/icons/Flatery-Dark $newusername;
echo;

echo "MAKE SURE TO DOWNLOAD 'USER THEMES' EXTENSION FROM GNOME WEBSITE!\nENABLE ICON AND GTK THEMES IN GNOME TWEAKS!";
sleep 3s;
echo;

echo "Installing user software...";
pacman -S python alacritty tmux htop unzip ncdu gparted wine
          rofi btop neovim zsh qalc;
echo;

echo "Downloading, building, and installing the latest Emacs...";
git clone -b master git://git.sv.gnu.org/emacs.git;
# Emacs shouldn't require anything not including with the Arch setup
# thus far, so don't need to install more packages like with the Ubuntu
# setup script.
cd emacs;
./autogen.sh;
./configure --with-mailutils --with-pop --with-sound --with-x-toolkit=gtk3 \
            --with-imagemagick --with-tree-sitter --with-json --with-xwidgets --with-file-notification \
            --with-cairo --with-modules --with-gnutls --with-xml2 --with-xft --with-xpm --with-native-compilation \
            CFLAGS="-O2 -mtune=native -march=native -fomit-frame-pointer";
make bootstrap;
make install;
echo;

echo "Installing config files...";
cd; cd dot;
mkdir -p /home/$newusername/.config/alacritty;
cp alacritty /home/$newusername/.config/alacritty/alacritty.yml;
chown /home/$newusername/.config -R $newusername:wheel;
cp zshrc /home/$newusername/.zshrc;
chown /home/$newusername/.zshrc $newusername:wheel;
echo;

echo "FINISHED CONFIGURATION/SETUP SCRIPT!";
