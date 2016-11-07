#!/bin/bash

echo "Installing Hapoid on your machine..."

command -v ghc >/dev/null 2>&1 || {
  echo ""
  echo >&2 "It seems GHC not installed on your machine."
  echo "Aborting..."
  echo ""
  exit 1
}

echo ""

ghc -O hapoid.hs
rm hapoid.hi
rm hapoid.o

echo ""
echo "Hapoid will be placed at /usr/bin/. So, it's need root privilege."

sudo mv hapoid /usr/bin/hapoid

echo ""
echo "Done!"
echo "Type \"hapoid version\" for verify it."
