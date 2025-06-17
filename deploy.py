"""
Project:     Dotfiles
Name:        dotfiles/deploy.py
Author:      Ian Kollipara <ian.kollipara@gmail.com>
Date:        2025-06-13
Description: My PyInfra Deployment
"""

from pyinfra.operations import server
from pyinfra.operations import dnf
from pyinfra.operations import git
from pyinfra import config
from pyinfra import logger
from pyinfra import host
from pyinfra.facts import server as facts_server

server.hostname(
    name="Setting hostname to Workstation.",
    hostname="Workstation",
)

server.locale(
    name="Setting en_US.UTF-8",
    local="en_US.UTF-8",
)

fedora_version = host.get_fact(facts_server.Command, "rpm -E %fedora")

dnf.rpm(
    name="Installing RPM Fusion Free",
    src=f"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-{fedora_version}.noarch.rpm"
)

dnf.rpm(
    name="Installing RPM Fusion Non-Free",
    src=f"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-{fedora_version}.noarch.rpm"
)

dnf.package(
    name="Installing dnf5-plugins",
    packages="dnf5-plugins",
)

server.shell(
    name="Enable GH Cli Repo",
    commands="dnf config-manager addrepo --from-repofile=https://cli.github.com/packages/rpm/gh-cli.repo",
    _sudo=True
)

server.shell(
    name="Enable openh264 library",
    commands="dnf config-manager setopt fedora-cisco-openh264.enabled=1",
    _sudo=True,
)

user = server.user(
    name="Create user 'ian'",
    user="ian",
    present=True,
    create_home=True,
    ensure_home=True,
    group="wheel",
    shell="/usr/bin/zsh",
)

server.packages(
    name="Installing Packages...",
    packages=[
        "fd-find",
        "zsh",
        "ripgrep",
        "emacs-gtk+x11",
        "emacs-common",
        "emacs-filesystem",
        "emacsclient",
        "enchant2",
        "enchant2-devel",
        "cmake",
        "libvterm"
    ]
)

server.shell(
    commands="dnf install gh --repo gh-cli",
    _sudo=True
)

flatpak.package(
    name="Installing Flatpaks...",
    packages=[
        "com.discordapp.Discord",
        "com.github.finefindus.eyedropper",
        "com.mattjakeman.ExtensionManager",
        "com.microsoft.Edge",
        "org.freedesktop.Platform",
        "org.freedesktop.Platform.GL.default",
        "org.freedesktop.Platform.VAAPI.Intel",
        "org.freedesktop.Platform.openh264",
        "org.gnome.Platform",
        "org.zotero.Zotero",
        "us.zoom.Zoom",
    ]
)

git.repo(
    name="Cloning Dotfiles...",
    src="https://github.com/ikollipara/dotfiles",
    dest="/home/ian/dotfiles",
    user=user,
)
