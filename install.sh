#! /usr/bin/env bash
# Name:    Install Script
# Author:  Ian Kollipara <ian.kollipara@gmail.com>
# Created: 2026-05-09

# Colors
blue_fg=$(tput setaf 4)
green_fg=$(tput setaf 2)
white_fg=$(tput setaf 7)
black_fg=$(tput setaf 0)
blue_bg=$(tput setab 4)
green_bg=$(tput setab 2)
white_bg=$(tput setab 7)
black_bg=$(tput setab 0)
reset_fg=$(tput setaf 7)
reset_bg=$(tput setab 7)
bold=$(tput bold)

# Vars
pkgs=(fd-find ripgrep emacs emacsclient enchant2 enchant2-devel tmux fish nix nix-daemon mise gh zed ghostty helix gnome-tweaks)

log() {
    local msg=$1
    local fg=$2
    local bg=$3

    echo -n $bg$fg
    echo -n $msg
    echo $reset_fg
}

fedora_version=$(rpm -E %fedora)

tput smso; echo "Dotfiles Install"; tput rmso; tput sgr0
log "Stage 1: Updating System" $green_fg

sudo dnf update -y

log ""

log "- Downloading Dotfiles" $green_fg
git clone https://github.com/ikollipara/dotfiles $HOME/dotfiles

log "Stage 2: Installing RPMs" $green_fg
log "- Installing RPM Fusion" $blue_fg
sudo dnf config-manager setopt fedora-cisco-openh264.enabled=1
sudo dnf install https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-${fedora_version}.noarch.rpm
sudo dnf install https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-${fedora_version}.noarch.rpm

log "- Installing Dropbox" $blue_fg
sudo dnf install https://www.dropbox.com/download?dl=packages/fedora/nautilus-dropbox-2026.01.15-1.fc43.x86_64.rpm

log "- Installing Terra Repos (for Zed)" $blue_fg
sudo dnf install --nogpgcheck --repofrompath 'terra,https://repos.fyralabs.com/terra$releasever' terra-release

log ""

log "Stage 3: Enabling Coprs" $green_fg
log "- Enabling Mise" $blue_fg
sudo dnf copr enable jdxcode/mise
log ""
log "- Enabling Ghostty" $blue_fg
sudo dnf copr enable scottames/ghostty

log "Stage 4: Installing Packages" $green_fg
log "- Installing System Packages" $blue_fg
sudo dnf install -y $pkgs
sudo systemctl enable --now nix-daemon
flatpak install -y flathub com.valvesoftware.Steam com.discordapp.Discord com.mattjakeman.ExtensionManager

log "- Installing Dev Tools" $blue_bg
mise use -g uv@
mise use -g fnox@
mise use -g age@
mise use -g usage@
mise use -g mkcert@
mise use -g node@
mise use -g npm@
mise use -g ghcup@
nix-env --install --attr devenv -f "https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable"
mise exec -- uv python install 3.13
mise exec -- uv python install 3.14
mise exec -- uv tool install pgcli cookiecutter ruff djade "python-lsp-server[rope]" gnome-extensions-cli
mise exec -- npm i -g some-sass-language-server vscode-langservers-extended
mise exec -- ghcup install ghc@9.10.3
mise exec -- ghcup install stack@3.7.1
mise exec -- ghcup install hls@2.14.0.0
mise exec -- ghcup install cabal@3.14.2.0

log "- Installing Helium" $blue_bg
curl -o $HOME/.local/bin/helium "https://github.com/imputnet/helium-linux/releases/download/0.12.1.1/helium-0.12.1.1-x86_64.AppImage"
chmod +x $HOME/.local/bin/helium
ln -sfn $HOME/dotfiles/applications/helium.desktop $HOME/.local/share/applications/helium.desktop
ln -sfn $HOME/dotfiles/.icons $HOME/.icons

log "- Setting up Dropbox" $blue_fg
log "-- Please start and finish the dropbox app" $blue_fg
read

log "- Setting up Dropbox as Desktop" $blue_fg
rm -rf $HOME/Documents $HOME/Pictures $HOME/Projects $HOME/Music $HOME/Videos
ln -sfn $HOME/Dropbox/Documents $HOME/Documents
ln -sfn $HOME/Dropbox/Pictures $HOME/Pictures
ln -sfn $HOME/Dropbox/Projects $HOME/Projects
ln -sfn $HOME/Dropbox/Music $HOME/Music
ln -sfn $HOME/Dropbox/Videos $HOME/Videos

log ""

log "Stage 5: Configuring System" $green_fg
log "- Setting up dev services" $blue_fg
gh auth login
nix-env --install --attr devenv -f "https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable"
ln -sfn $HOME/dotfiles/helix $HOME/.config/helix
ln -sfn $HOME/dotfiles/tmux $HOME/.config/tmux
ln -sfn $HOME/dotfiles/ghostty $HOME/.config/ghostty
git clone https://github.com/ikollipara/emacs-helm $HOME/.config/emacs
git clone https://github.com/tmux-plugins/tpm $HOME/.config/tmux/plugins/tpm
sudo chsh -s /usr/bin/fish $USER

log "- Setting system wallpaper" $blue_fg
gsettings set org.gnome.desktop.background picture-uri-dark "file:///home/ian/Dropbox/Pictures/Wallpapers/Hudson River Valley.jpg"

log "- Setting up keybinds" $blue_fg
gsettings set org.gnome.settings-daemon.plugins.media-keys custom-keybindings "['/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/']"
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ name "Open Terminal"
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ command "ghostty"
gsettings set org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom0/ binding "<Super><Enter>"

gsettings set org.gnome.settings-daemon.plugins.media-keys control-center "['<Super>i']"
gsettings set org.gnome.settings-daemon.plugins.media-keys home "['<Super>e']"
gsettings set org.gnome.desktops.input-sources xkb-options "['ctrl:nocaps']"

log "- Setting Favorite" $blue_fg
gsettings set org.gnome.shell favorite-apps"['helium.desktop', 'emacs.desktop', 'org.gnome.Calendar.desktop']"

log "- Installing Extensions" $blue_fg
mise exec -- gnome-extensions-cli install blur-my-shell@aunetx
