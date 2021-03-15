#!/bin/bash

# install all dependencies used by me

DEV_PACKAGES="git emacs bear node"

USER_PACKAGES="qbittorrent vlc okular chromium"

PACKAGES="$DEV_PACKAGES $USER_PACKAGES"

APT_PACKAGES="$PACKAGES silversearcher-ag"

DNF_PACKAGES="$PACKAGES the_silver_searcher java-11-openjdk"

haveProg() {
    [ -x "$(which $1 2> /dev/null)" ]
}

aptInstall() {
    sudo apt install $APT_PACKAGES
}

dnfInstall() {
    sudo dnf install $DNF_PACKAGES
}

if haveProg apt; then aptInstall
elif haveProg dnf; then dnfInstall
else
    echo "No package manager found! install your stuff manually"
    exit 2
fi
