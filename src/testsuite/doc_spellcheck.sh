#!/bin/sh
#
# Manually check HTML document files
# Ken O. Burtch
# January 2025

cd ../../doc

aspell --mode=html --home-dir=../src/testsuite --personal='aspell_dict.pws' check "$1"

