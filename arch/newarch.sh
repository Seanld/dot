# Set up a new installation of Arch Linux, with packages I use,
# and the personalizations I prefer.
#
# NOTE: Run this after `archinstall.sh` has chrooted you into the
# new installation.

echo "Setting time zone to MST...";
ln -sf /usr/share/zoneinfo/America/Denver /etc/localtime;
hwclock --systohc;

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
useradd -m $newusername;
echo "Set a password for your new account!";
passwd $newusername;
echo;

echo "Enabling DHCPCD, to gain ethernet..."
pacman -S dhcpcd;
systemctl enable dhcpcd.service;
echo;

echo "Installing doas and giving new user permissions...";
pacman -S doas;
echo "permit persist :wheel" >> /etc/doas.conf;
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

echo "Downloading & installing Dracula GTK theme...";
git clone https://github.com/dracula/gtk /usr/share/themes/Dracula;
chown /usr/share/themes/Dracula $newusername;
echo;

echo "MAKE SURE TO DOWNLOAD 'USER THEMES' EXTENSION FROM GNOME WEBSITE!\nENABLE ICON AND GTK THEMES IN GNOME TWEAKS!";
sleep 3s;
echo;

echo "Installing user software...";
pacman -S firefox python3 alacritty screen htop mc amfora unzip httpie ncdu gparted wine npm;
echo;

echo "Downloading and installing Cascadia Code font...";
wget "https://github.com/microsoft/cascadia-code/releases/download/v2105.24/CascadiaCode-2105.24.zip";
if [[ "$?" != 0 ]]; then
    echo "URL for Cascadia Code/Mono fonts is incorrect. Skipping.";
else
    echo "Downloaded Cascadia Code/Mono fonts.";
fi
unzip CascadiaCode-2105.24.zip;
mkdir /home/$newusername/.local/share/fonts;
chown /home/$newusername/.local/share/fonts $newusername;
mv ttf/*.ttf ~/.local/share/fonts;
chown /home/$newusername/.local/share/fonts/*.ttf $newusername;
# Clean up files.
rm -rf ttf woff2 otf;
rm CascadiaCode*;
echo;

echo "Downloading, building, and installing the latest Emacs...";
git clone -b master git://git.sv.gnu.org/emacs.git;
# Emacs shouldn't require anything not including with the Arch setup
# thus far, so don't need to install more packages like with the Ubuntu
# setup script.
cd emacs;
./autogen.sh;
./configure --with-mailutils --with-sound=yes --with-x-toolkit=gtk3
            --with-imagemagick --with-json --with-xwidgets --with-file-notification=yes
            --with-cairo --with-modules --with-gnutls --with-xml2 --with-xft --with-xpm
            CFLAGS="-02 -mtune=native -march=native -fomit-frame-pointer";
make bootstrap;
make install;
echo;

echo "Installing Spacemacs...";
git clone https://github.com/syl20bnr/spacemacs /home/$newusername/.emacs.d;
chown /home/$newusername/.emacs.d $newusername;
echo;

echo "Installing config files...";
cd; cd dot;
mkdir -p /home/$newusername/.config/alacritty;
cp alacritty /home/$newusername/.config/alacritty/alacritty.yml;
chown /home/$newusername/.config -R $newusername;
cp screenrc /home/$newusername/.screenrc;
chown /home/$newusername/.screenrc $newusername;
cp spacemacs /home/$newusername/.spacemacs;
chown /home/$newusername/.spacemacs $newusername;
cp zshrc /home/$newusername/.zshrc;
chown /home/$newusername/.zshrc $newusername;
echo;

echo "FINISHED CONFIGURATION/SETUP SCRIPT!";
