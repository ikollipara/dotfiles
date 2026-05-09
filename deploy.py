# Project:     Dotfiles
# Name:        dotfiles/deploy.py
# Author:      Ian Kollipara <ian.kollipara@gmail.com>
# Date:        2025-06-13
# Description: My PyInfra Deployment


from pyinfra import config, host, logger
from pyinfra.facts import server as facts_server
from pyinfra.operations import dnf, files, flatpak, git, npm, server, systemd, uv

server.hostname(
    name="Setting hostname to Workstation.",
    hostname="Workstation",
)

fedora_version = host.get_fact(facts_server.Command, "rpm -E %fedora")

dnf.rpm(
    name="Installing RPM Fusion Free",
    src=f"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-{fedora_version}.noarch.rpm",
    _sudo=True,
)

dnf.rpm(
    name="Installing RPM Fusion Non-Free",
    src=f"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-{fedora_version}.noarch.rpm",
    _sudo=True,
)

dnf.packages(
    name="Installing dnf5-plugins",
    packages="dnf5-plugins",
)

server.shell(
    name="Enable GH Cli Repo",
    commands="dnf config-manager addrepo --from-repofile=https://cli.github.com/packages/rpm/gh-cli.repo --overwrite",
    _sudo=True,
)

server.shell(
    name="Enable Mise Repo", commands="dnf copr enable jdxcode/mise", _sudo=True
)

server.shell(
    name="Enable openh264 library",
    commands="dnf config-manager setopt fedora-cisco-openh264.enabled=1",
    _sudo=True,
)

files.download(
    src="https://www.dropbox.com/download?dl=packages/fedora/nautilus-dropbox-2026.01.15-1.fc44.x86_64.rpm",
    dest="/tmp",
)


server.packages(
    name="Installing Packages...",
    _sudo=True,
    packages=[
        "fd-find",
        "ripgrep",
        "emacs-gtk+x11",
        "emacs-common",
        "emacs-filesystem",
        "emacsclient",
        "enchant2",
        "enchant2-devel",
        "cmake",
        "libvterm",
        "tmux",
        "fish",
        "nix",
        "nix-daemon",
        "mise",
    ],
)

dnf.rpm(
    name="Installing Dropbox",
    src="/tmp/nautilus-dropbox-2026.01.15-1.fc43.x86_64.rpm",
    _sudo=True,
)

server.shell(
    commands="dnf install gh --repo gh-cli -y",
    _sudo=True,
)

server.shell(
    name="Installing Dev Environment",
    commands="; ".join(
        (
            "mise use -g uv@latest",
            "mise use -g fnox@",
            "mise use -g age@",
            "mise use -g usage@",
            "mise use -g mkcert@",
            "mise use -g node@",
            "mise use -g npm@",
            "nix-env --install --attr devenv -f https://github.com/NixOS/nixpkgs/tarball/nixpkgs-unstable",
        )
    ),
)

uv.pythons(
    name="Install Python (3.13, 3.14)",
    versions=["3.14", "3.13"],
)

uv.tools(
    name="Installing pgcli, cookiecutter, ruff",
    tools=["pgcli", "cookiecutter", "ruff", "djade", '"python-lsp-server[rope]"'],
)

npm.packages(
    name="Installing language servers",
    packages=["some-sass-language-server", "vscode-langservers-extracted"],
)

systemd.service(
    name="Enabling Nix Daemon",
    service="nix-daemon",
    enabled=True,
    _sudo=True,
)

server.shell(
    name="Setting Wallpaper",
    commands='gsettings set org.gnome.desktop.background picture-uri-dark "file:///home/ian/Dropbox/Pictures/Wallpapers/Hudson River Valley.jpg"',
)
