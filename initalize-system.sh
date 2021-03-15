#!/bin/bash

# this script should (mostly) fully initialize a bare linux setup

bash ./initalize/install-dependencies.sh
bash ./initalize/setup-bash.sh
bash ./initalize/npm-packages.sh
