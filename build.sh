#!/usr/bin/env bash

stack clean
stack build --extra-include-dirs=/usr/local/include  --extra-lib-dirs=/usr/local/lib --color always