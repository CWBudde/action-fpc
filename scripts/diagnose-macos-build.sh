#!/bin/bash
# Diagnostic script for macOS Lazarus build issues
# Run this on macOS to gather information about the build environment

echo "=========================================="
echo "macOS Build Environment Diagnostic"
echo "=========================================="
echo ""

# System information
echo "==> System Information"
echo "    OS Version: $(sw_vers -productVersion 2>/dev/null || echo 'N/A')"
echo "    Architecture: $(uname -m)"
echo "    Kernel: $(uname -r)"
echo ""

# Xcode and command-line tools
echo "==> Xcode Command-line Tools"
if xcode-select -p &>/dev/null; then
  echo "    ✓ Installed at: $(xcode-select -p)"
  echo "    Version: $(pkgutil --pkg-info=com.apple.pkg.CLTools_Executables 2>/dev/null | grep version || echo 'Unknown')"
else
  echo "    ✗ NOT INSTALLED - This is likely the problem!"
  echo "    Install with: xcode-select --install"
fi
echo ""

# Homebrew
echo "==> Homebrew"
if command -v brew &>/dev/null; then
  echo "    ✓ Installed: $(brew --version | head -1)"
  echo "    Prefix: $(brew --prefix)"
else
  echo "    ✗ Not installed"
fi
echo ""

# FPC installation
echo "==> Free Pascal Compiler"
if command -v fpc &>/dev/null; then
  echo "    ✓ Installed: $(which fpc)"
  echo "    Version: $(fpc -iV)"
  echo "    Target CPU: $(fpc -iTP)"
  echo "    Target OS: $(fpc -iTO)"

  FPC_DIR=$(dirname $(dirname $(which fpc)))
  FPC_VERSION=$(fpc -iV)
  ARCH=$(uname -m)
  if [ "$ARCH" = "arm64" ]; then
    DARWIN_ARCH="aarch64-darwin"
  else
    DARWIN_ARCH="x86_64-darwin"
  fi

  FPC_UNITS_DIR="$FPC_DIR/lib/fpc/$FPC_VERSION/units/$DARWIN_ARCH"
  echo "    Units directory: $FPC_UNITS_DIR"

  if [ -d "$FPC_UNITS_DIR" ]; then
    echo "    ✓ Units directory exists"
    echo "    Available units: $(ls -1 "$FPC_UNITS_DIR" 2>/dev/null | wc -l) directories"
  else
    echo "    ✗ Units directory not found"
  fi
else
  echo "    ✗ Not installed"
fi
echo ""

# Build tools
echo "==> Build Tools"
for tool in make git clang cc; do
  if command -v $tool &>/dev/null; then
    echo "    ✓ $tool: $(which $tool)"
    if [ "$tool" = "make" ]; then
      echo "      Version: $(make --version 2>&1 | head -1)"
    fi
  else
    echo "    ✗ $tool: NOT FOUND"
  fi
done
echo ""

# Test if we can clone Lazarus
echo "==> Testing Lazarus Git Repository Access"
if git ls-remote --exit-code --heads https://gitlab.com/freepascal.org/lazarus/lazarus.git lazarus_3_0 &>/dev/null; then
  echo "    ✓ Can access Lazarus repository"
  echo "    ✓ Branch 'lazarus_3_0' exists"
else
  echo "    ✗ Cannot access Lazarus repository or branch not found"
fi
echo ""

# Check if we can compile a simple Pascal program
echo "==> Testing FPC Compilation"
cat > /tmp/test_fpc.pas << 'EOF'
program TestFPC;
begin
  writeln('FPC works!');
end.
EOF

if fpc /tmp/test_fpc.pas -o/tmp/test_fpc &>/dev/null; then
  echo "    ✓ FPC can compile programs"
  rm -f /tmp/test_fpc /tmp/test_fpc.pas /tmp/test_fpc.o
else
  echo "    ✗ FPC compilation failed"
  echo "    Running with verbose output:"
  fpc /tmp/test_fpc.pas -o/tmp/test_fpc
fi
echo ""

echo "=========================================="
echo "Diagnostic Complete"
echo "=========================================="
echo ""
echo "If Xcode command-line tools are missing, install with:"
echo "  xcode-select --install"
echo ""
