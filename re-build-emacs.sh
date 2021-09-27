#!/bin/bash

set -e

echo 'Rebuilding emacs'

cd ~/src-code/emacs/

git clean -fdx

git pull

./autogen.sh

$(cat ~/.emacs.d/configure-options)

make -j $(nproc)

sudo make install
