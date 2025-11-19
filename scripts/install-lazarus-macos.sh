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

# Check for Xcode command-line tools (required for building)
echo "==> Checking for Xcode command-line tools..."
if ! xcode-select -p &>/dev/null; then
  echo "    ✗ Xcode command-line tools not found"
  echo "    Installing Xcode command-line tools..."
  echo "    (This may take a few minutes)"

  # Touch a temporary file to trigger the installation
  touch /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

  # Find the latest command-line tools update
  PROD=$(softwareupdate -l 2>/dev/null | grep "\*.*Command Line" | tail -n 1 | sed 's/^[^C]* //')

  if [ -n "$PROD" ]; then
    softwareupdate -i "$PROD" --verbose
  else
    echo "    Could not find Command Line Tools in software updates"
    echo "    Attempting direct installation..."
    xcode-select --install
    echo "    Please complete the installation in the GUI and re-run this script"
    exit 1
  fi

  rm -f /tmp/.com.apple.dt.CommandLineTools.installondemand.in-progress

  # Verify installation
  if xcode-select -p &>/dev/null; then
    echo "    ✓ Xcode command-line tools installed successfully"
  else
    echo "    ✗ Failed to install Xcode command-line tools"
    echo "    Please install manually with: xcode-select --install"
    exit 1
  fi
else
  echo "    ✓ Xcode command-line tools found at: $(xcode-select -p)"
fi

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

# Verify required build tools
echo "==> Verifying build tools..."
MISSING_TOOLS=()
for tool in make git; do
  if ! command -v $tool &>/dev/null; then
    MISSING_TOOLS+=("$tool")
    echo "    ✗ $tool not found"
  else
    echo "    ✓ $tool: $(which $tool)"
  fi
done

if [ ${#MISSING_TOOLS[@]} -gt 0 ]; then
  echo "    ERROR: Missing required build tools: ${MISSING_TOOLS[*]}"
  echo "    These should be installed with Xcode command-line tools"
  exit 1
fi

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
    fpc -FUlib/$DARWIN_ARCH -Fulib/$DARWIN_ARCH $unit.pas || true
  elif [ -f "$unit.pp" ]; then
    fpc -FUlib/$DARWIN_ARCH -Fulib/$DARWIN_ARCH $unit.pp || true
  fi
done

# Build packager registration components
echo "==> Building packager registration..."
cd /tmp/lazarus/packager/registration
mkdir -p units/$DARWIN_ARCH
fpc -FUunits/$DARWIN_ARCH \
  -Fuunits/$DARWIN_ARCH \
  -Fu../../components/lazutils/lib/$DARWIN_ARCH \
  lazaruspackageintf.pas || true

# Build LCL for Cocoa
echo "==> Building LCL for Cocoa..."
cd /tmp/lazarus/lcl

# Debug: Show environment info
echo "    FPC compiler: $(which fpc)"
echo "    FPC version: $(fpc -iV)"
echo "    PWD: $(pwd)"

# Check if Makefile exists
if [ ! -f "Makefile" ]; then
  echo "ERROR: Makefile not found in $(pwd)"
  exit 1
fi

# Set compiler options
export FPCOPT="-Fu/tmp/lazarus/packager/registration/units/$DARWIN_ARCH -Fu/tmp/lazarus/components/lazutils/lib/$DARWIN_ARCH"
echo "    Build options: $FPCOPT"

# Build with verbose output
if ! make LCL_PLATFORM=cocoa PP=$(which fpc) OPT="$FPCOPT"; then
  echo ""
  echo "ERROR: Failed to build LCL for Cocoa"
  echo "This might be due to:"
  echo "  - Missing Xcode command-line tools (run: xcode-select --install)"
  echo "  - Incompatible FPC version"
  echo "  - Build system issues"
  echo ""
  echo "Debug info:"
  echo "  FPC: $(which fpc)"
  echo "  FPC version: $(fpc -iV)"
  echo "  Architecture: $DARWIN_ARCH"
  echo "  Lazarus source: /tmp/lazarus"
  exit 1
fi

# Install to system location
echo "==> Installing to $INSTALL_DIR..."
sudo mkdir -p "$INSTALL_DIR"
sudo cp -R /tmp/lazarus/lcl "$INSTALL_DIR/"
sudo cp -R /tmp/lazarus/components "$INSTALL_DIR/"
sudo cp -R /tmp/lazarus/packager "$INSTALL_DIR/"

echo "==> Lazarus LCL installation complete!"
echo "    LCL units: $INSTALL_DIR/lcl/units/$DARWIN_ARCH"
echo "    Lazutils: $INSTALL_DIR/components/lazutils/lib/$DARWIN_ARCH"
