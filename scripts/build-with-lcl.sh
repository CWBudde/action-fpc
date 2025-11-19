#!/bin/bash
set -e

# Build a Free Pascal project with LCL support
# Usage: ./build-with-lcl.sh <project-file> [additional-fpc-options]

if [ $# -eq 0 ]; then
  echo "Error: Project file required"
  echo "Usage: $0 <project-file> [additional-fpc-options]"
  echo "Example: $0 MyApp.dpr -Fu../Source"
  exit 1
fi

PROJECT_FILE="$1"
shift
ADDITIONAL_OPTIONS="$@"

if [ ! -f "$PROJECT_FILE" ]; then
  echo "Error: Project file not found: $PROJECT_FILE"
  exit 1
fi

echo "==> Building $PROJECT_FILE..."

# Detect OS and architecture
OS=$(uname -s)
ARCH=$(uname -m)

if [ "$OS" = "Linux" ]; then
  # Linux build
  fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
      -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
      -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
      -Fi/usr/lib/lazarus/3.0/lcl/include \
      -dLCL -dLCLgtk2 \
      $ADDITIONAL_OPTIONS \
      "$PROJECT_FILE"

elif [ "$OS" = "Darwin" ]; then
  # macOS build
  if [ "$ARCH" = "arm64" ]; then
    DARWIN_ARCH="aarch64-darwin"
  else
    DARWIN_ARCH="x86_64-darwin"
  fi

  FPC_DIR=$(dirname $(dirname $(which fpc)))
  FPC_VERSION=$(fpc -iV)
  FPC_UNITS_DIR="$FPC_DIR/lib/fpc/$FPC_VERSION/units/$DARWIN_ARCH"
  LAZARUS_DIR="${LAZARUS_DIR:-/usr/local/share/lazarus}"

  fpc -Fu"$LAZARUS_DIR/lcl/units/$DARWIN_ARCH" \
      -Fu"$LAZARUS_DIR/lcl/units/$DARWIN_ARCH/cocoa" \
      -Fu"$LAZARUS_DIR/components/lazutils/lib/$DARWIN_ARCH" \
      -Fu"$FPC_UNITS_DIR/fcl-image" \
      -Fu"$FPC_UNITS_DIR/fcl-base" \
      -Fu"$FPC_UNITS_DIR/rtl-objc" \
      -Fi"$LAZARUS_DIR/lcl/include" \
      -dLCL -dLCLcocoa \
      $ADDITIONAL_OPTIONS \
      "$PROJECT_FILE"

else
  echo "Error: Unsupported operating system: $OS"
  exit 1
fi

echo "==> Build complete!"
