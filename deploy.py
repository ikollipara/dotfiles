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
from pyinfra.operations import flatpak
from pyinfra import config
from pyinfra import logger
from pyinfra import host
from pyinfra.facts import server as facts_server

server.hostname(
    name="Setting hostname to Workstation.",
    hostname="Workstation",
)

fedora_version = host.get_fact(facts_server.Command, "rpm -E %fedora")

dnf.rpm(
    name="Installing RPM Fusion Free",
    src=f"https://mirrors.rpmfusion.org/free/fedora/rpmfusion-free-release-{fedora_version}.noarch.rpm",
    _sudo=True
)

dnf.rpm(
    name="Installing RPM Fusion Non-Free",
    src=f"https://mirrors.rpmfusion.org/nonfree/fedora/rpmfusion-nonfree-release-{fedora_version}.noarch.rpm",
    _sudo=True
)

dnf.packages(
    name="Installing dnf5-plugins",
    packages="dnf5-plugins",
)


server.shell(
    name="Enable GH Cli Repo",
    commands="dnf config-manager addrepo --from-repofile=https://cli.github.com/packages/rpm/gh-cli.repo --overwrite",
    _sudo=True
)

server.shell(
    name="Enable openh264 library",
    commands="dnf config-manager setopt fedora-cisco-openh264.enabled=1",
    _sudo=True,
)

server.packages(
    name="Installing Packages...",
    _sudo=True,
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
        "libvterm",
    ]
)

server.shell(
    commands="dnf install gh --repo gh-cli -y",
    _sudo=True
)

flatpak.packages(
    name="Installing Flatpaks...",
    packages=[
        "com.github.finefindus.eyedropper",
        "com.mattjakeman.ExtensionManager",
        "com.microsoft.Edge",
        "org.freedesktop.Platform/x86_64/25.08",
        "org.freedesktop.Platform.GL.default/x86_64/25.08",
        "org.freedesktop.Platform.VAAPI.Intel/x86_64/25.08",
        #"org.freedesktop.Platform.openh264/x86_64/25.08",
        "org.gnome.Platform/x86_64/48",
        "org.zotero.Zotero",
	"md.obsidian.Obsidian",
        "us.zoom.Zoom",
    ]
)
