#!/bin/bash

# this script is used to install ctags on Debian-based systems (including Ubuntu).
# reference,
# https://github.com/universal-ctags/ctags/blob/master/docs/autotools.rst

sudo apt install -y \
    gcc make \
    pkg-config autoconf automake \
    python3-docutils \
    libseccomp-dev \
    libjansson-dev \
    libyaml-dev \
    libxml2-dev
sudo git clone https://github.com/universal-ctags/ctags.git
cd ctags
sudo ./autogen.sh
sudo ./configure
sudo make
sudo make install
