#!/bin/bash

# Run setup emacs on a machine.

cd rope && python setup.py build
cd ropemacs && python setup.py build

sudo apt-get install pyflakes