#!/usr/bin/env sh

apk add git make autoconf gcc g++ texinfo gnutls-dev ncurses-dev gawk

git clone -b master git://git.sv.gnu.org/emacs.git --depth 1
cd /root/emacs/
./autogen.sh
./configure --with-mailutils
make -j1
make install
