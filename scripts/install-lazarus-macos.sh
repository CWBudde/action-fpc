#!/bin/bash
set -e

# Install Lazarus LCL on macOS
# This script compiles the Lazarus Component Library (LCL) from source for Cocoa
# Usage: ./install-lazarus-macos.sh [lazarus_version]

LAZARUS_VERSION="${1:-lazarus_3_0}"
INSTALL_DIR="${2:-/usr/local/share/lazarus}"

echo "==> Installing Lazarus LCL for macOS..."
echo "    Version: $LAZARUS_VERSION"
echo "    Install directory: $INSTALL_DIR"

# Detect architecture for proper paths
ARCH=$(uname -m)
if [ "$ARCH" = "arm64" ]; then
  DARWIN_ARCH="aarch64-darwin"
else
  DARWIN_ARCH="x86_64-darwin"
fi
echo "    Detected architecture: $ARCH (using $DARWIN_ARCH)"

# Find FPC installation and units
FPC_DIR=$(dirname $(dirname $(which fpc)))
FPC_VERSION=$(fpc -iV)
FPC_UNITS_DIR="$FPC_DIR/lib/fpc/$FPC_VERSION/units/$DARWIN_ARCH"
echo "    FPC installation: $FPC_DIR"
echo "    FPC version: $FPC_VERSION"

# Clone Lazarus sources from GitLab
echo "==> Cloning Lazarus sources..."
git clone --depth 1 --branch "$LAZARUS_VERSION" https://gitlab.com/freepascal.org/lazarus/lazarus.git /tmp/lazarus

# Build lazutils (required by LCL)
echo "==> Building lazutils..."
cd /tmp/lazarus/components/lazutils
mkdir -p lib/$DARWIN_ARCH

# List of essential units to compile (in dependency order)
UNITS="lazutilsstrconsts lazutilities graphtype graphmath lazutf8 fileutil lconvencoding laztracer lazloggerbase lazlogger lazmethodlist lazfileutils lazversion lazconfigstorage dynqueue integerlist utf8process lazsysutils maps textstrings extendedstrings uitypes dynamicarray laz2_xmlcfg lcsvutils"

for unit in $UNITS; do
  if [ -f "$unit.pas" ]; then
    fpc -FUlib/$DARWIN_ARCH -Fulib/$DARWIN_ARCH $unit.pas > /dev/null 2>&1 || true
  elif [ -f "$unit.pp" ]; then
    fpc -FUlib/$DARWIN_ARCH -Fulib/$DARWIN_ARCH $unit.pp > /dev/null 2>&1 || true
  fi
done

# Build packager registration components
echo "==> Building packager registration..."
cd /tmp/lazarus/packager/registration
mkdir -p units/$DARWIN_ARCH
fpc -FUunits/$DARWIN_ARCH \
  -Fuunits/$DARWIN_ARCH \
  -Fu../../components/lazutils/lib/$DARWIN_ARCH \
  lazaruspackageintf.pas > /dev/null 2>&1 || true

# Build LCL for Cocoa
echo "==> Building LCL for Cocoa..."
cd /tmp/lazarus/lcl
export FPCOPT="-Fu/tmp/lazarus/packager/registration/units/$DARWIN_ARCH -Fu/tmp/lazarus/components/lazutils/lib/$DARWIN_ARCH"
make LCL_PLATFORM=cocoa PP=$(which fpc) OPT="$FPCOPT" > /dev/null 2>&1

# Install to system location
echo "==> Installing to $INSTALL_DIR..."
sudo mkdir -p "$INSTALL_DIR"
sudo cp -R /tmp/lazarus/lcl "$INSTALL_DIR/"
sudo cp -R /tmp/lazarus/components "$INSTALL_DIR/"
sudo cp -R /tmp/lazarus/packager "$INSTALL_DIR/"

echo "==> Lazarus LCL installation complete!"
echo "    LCL units: $INSTALL_DIR/lcl/units/$DARWIN_ARCH"
echo "    Lazutils: $INSTALL_DIR/components/lazutils/lib/$DARWIN_ARCH"
