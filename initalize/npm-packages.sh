#!/bin/bash

cat ./static-files/.eslintrc.js > $HOME/.eslintrc.js
cat ./static-files/.prettierrc.js > $HOME/.prettierrc.js

if [ $NPM_PACKAGES ]; then
    echo "lol"
    npm i -g npm eslint eslint-plugin-import eslint-config-airbnb-base eslint-plugin-json tern
else
    echo "could not install npm packages. install them yourself"
fi
