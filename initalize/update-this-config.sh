#!/bin/bash

# run this file via a cron job so the .emacs.d/initalize files are always recent


for file in $(ls -A ./static-files/); do
    cp $HOME/$file ./static-files/$file
done
