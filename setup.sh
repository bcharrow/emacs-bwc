#!/bin/bash
set -o nounset
set -o errexit

# Get submodules
git submodule init
git submodule update

# Install Droid fonts on Linux
if [[ `uname` == "Linux" ]]; then
    sudo apt-get install ttf-droid
fi
