#!/bin/bash

echo 'Rebuilding emacs'

cd ~/src-code/emacs/

git clean -fdx

git pull

./autogen.sh

$(cat ~/.emacs.d/configure-options)

make -j 8

sudo make install
