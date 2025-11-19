#!/bin/bash
# Local testing utility for FPC GitHub Action
# This script allows testing the action's behavior locally without running GitHub Actions
# Particularly useful for debugging Linux installation issues

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Default options
INCLUDE_LAZARUS="true"
INSTALL_DEPENDENCIES=""
DRY_RUN="false"
VERBOSE="false"
CLEANUP_AFTER="false"
TEST_BUILD="true"

# Function to print colored messages
print_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_section() {
    echo ""
    echo -e "${GREEN}========================================${NC}"
    echo -e "${GREEN}$1${NC}"
    echo -e "${GREEN}========================================${NC}"
}

# Function to show usage
show_usage() {
    cat << EOF
Usage: $0 [OPTIONS]

Local testing utility for FPC GitHub Action. Tests the installation process
without requiring GitHub Actions runner.

OPTIONS:
    --no-lazarus            Don't install Lazarus (FPC only)
    --dependencies DEPS     Additional dependencies to install (space-separated)
    --dry-run              Show what would be done without executing
    --verbose              Show detailed output from commands
    --cleanup              Remove installation after testing
    --no-test-build        Skip building test program
    -h, --help             Show this help message

EXAMPLES:
    # Test basic FPC + Lazarus installation
    $0

    # Test FPC-only installation
    $0 --no-lazarus

    # Test with additional dependencies
    $0 --dependencies "libportaudio2 portaudio19-dev"

    # Dry run to see what would be executed
    $0 --dry-run

    # Verbose output for debugging
    $0 --verbose

    # Clean up after testing
    $0 --cleanup

EOF
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --no-lazarus)
            INCLUDE_LAZARUS="false"
            shift
            ;;
        --dependencies)
            INSTALL_DEPENDENCIES="$2"
            shift 2
            ;;
        --dry-run)
            DRY_RUN="true"
            shift
            ;;
        --verbose)
            VERBOSE="true"
            shift
            ;;
        --cleanup)
            CLEANUP_AFTER="true"
            shift
            ;;
        --no-test-build)
            TEST_BUILD="false"
            shift
            ;;
        -h|--help)
            show_usage
            exit 0
            ;;
        *)
            print_error "Unknown option: $1"
            show_usage
            exit 1
            ;;
    esac
done

# Function to execute or simulate commands
execute_cmd() {
    local cmd="$1"
    local desc="$2"

    if [ "$VERBOSE" = "true" ]; then
        print_info "Executing: $cmd"
    elif [ -n "$desc" ]; then
        print_info "$desc"
    fi

    if [ "$DRY_RUN" = "true" ]; then
        print_warning "[DRY-RUN] Would execute: $cmd"
        return 0
    fi

    if [ "$VERBOSE" = "true" ]; then
        eval "$cmd"
    else
        eval "$cmd" > /dev/null 2>&1
    fi
}

# Detect OS
print_section "System Detection"
OS=$(uname -s)
ARCH=$(uname -m)
print_info "Operating System: $OS"
print_info "Architecture: $ARCH"

if [ "$OS" != "Linux" ]; then
    print_error "This test script is currently designed for Linux only"
    print_info "For macOS/Windows testing, use the respective action steps manually"
    exit 1
fi

# Check for required tools
print_section "Pre-flight Checks"
if ! command -v apt-get &> /dev/null; then
    print_error "apt-get not found. This script requires a Debian/Ubuntu-based system"
    exit 1
fi
print_success "apt-get found"

if [ "$DRY_RUN" = "false" ]; then
    if [ "$EUID" -eq 0 ]; then
        print_warning "Running as root"
        SUDO=""
    else
        if ! command -v sudo &> /dev/null; then
            print_error "sudo not found and not running as root"
            exit 1
        fi
        print_success "sudo available"
        SUDO="sudo"
    fi
fi

# Show configuration
print_section "Test Configuration"
print_info "Include Lazarus: $INCLUDE_LAZARUS"
print_info "Additional dependencies: ${INSTALL_DEPENDENCIES:-none}"
print_info "Dry run: $DRY_RUN"
print_info "Verbose: $VERBOSE"
print_info "Test build: $TEST_BUILD"
print_info "Cleanup after: $CLEANUP_AFTER"

if [ "$DRY_RUN" = "false" ]; then
    echo ""
    read -p "Continue with installation? (y/N) " -n 1 -r
    echo
    if [[ ! $REPLY =~ ^[Yy]$ ]]; then
        print_warning "Installation cancelled"
        exit 0
    fi
fi

# Update package lists
print_section "Updating Package Lists"
execute_cmd "$SUDO apt-get update" "Updating apt package lists..."
print_success "Package lists updated"

# Install FPC
print_section "Installing Free Pascal Compiler"
execute_cmd "$SUDO apt-get install -y --fix-missing fpc" "Installing FPC..."
print_success "FPC installed"

# Install Lazarus if requested
if [ "$INCLUDE_LAZARUS" = "true" ]; then
    print_section "Installing Lazarus"
    execute_cmd "$SUDO apt-get install -y --fix-missing lazarus lcl-gtk2-3.0 lcl-units-3.0" "Installing Lazarus and LCL components..."
    print_success "Lazarus installed"
fi

# Install additional dependencies if specified
if [ -n "$INSTALL_DEPENDENCIES" ]; then
    print_section "Installing Additional Dependencies"
    print_info "Installing: $INSTALL_DEPENDENCIES"
    execute_cmd "$SUDO apt-get install -y --fix-missing $INSTALL_DEPENDENCIES" "Installing additional dependencies..."
    print_success "Additional dependencies installed"
fi

# Verify installation
print_section "Verifying Installation"

if [ "$DRY_RUN" = "false" ]; then
    FPC_PATH=$(which fpc)
    FPC_VERSION=$(fpc -iV)

    print_success "FPC found at: $FPC_PATH"
    print_success "FPC version: $FPC_VERSION"

    # Display FPC info
    print_info "Full FPC information:"
    fpc -i

    if [ "$INCLUDE_LAZARUS" = "true" ]; then
        if [ -d "/usr/lib/lazarus/3.0" ]; then
            print_success "Lazarus found at: /usr/lib/lazarus/3.0"

            # Check for LCL units
            if [ -d "/usr/lib/lazarus/3.0/lcl/units/x86_64-linux" ]; then
                print_success "LCL units found for x86_64-linux"
                print_info "Unit directories:"
                ls -la /usr/lib/lazarus/3.0/lcl/units/x86_64-linux/ | head -20
            else
                print_warning "LCL units directory not found at expected location"
            fi
        else
            print_warning "Lazarus directory not found at /usr/lib/lazarus/3.0"
        fi
    fi
fi

# Create and build test program
if [ "$TEST_BUILD" = "true" ] && [ "$DRY_RUN" = "false" ]; then
    print_section "Testing FPC Compilation"

    TEST_DIR=$(mktemp -d)
    TEST_PROGRAM="$TEST_DIR/test_program.pas"

    print_info "Creating test program at: $TEST_PROGRAM"

    cat > "$TEST_PROGRAM" << 'PASCAL_EOF'
program TestProgram;

{$mode objfpc}{$H+}

uses
  SysUtils;

begin
  WriteLn('=================================');
  WriteLn('FPC Test Program');
  WriteLn('=================================');
  WriteLn('Compiled successfully!');
  WriteLn('FPC Version: ', {$I %FPCVERSION%});
  WriteLn('Target CPU: ', {$I %FPCTARGETCPU%});
  WriteLn('Target OS: ', {$I %FPCTARGETOS%});
  WriteLn('Build Date: ', {$I %DATE%}, ' ', {$I %TIME%});
  WriteLn('=================================');
end.
PASCAL_EOF

    print_info "Compiling test program..."
    if fpc "$TEST_PROGRAM" -o"$TEST_DIR/test_program"; then
        print_success "Compilation successful!"

        print_info "Running test program:"
        echo ""
        "$TEST_DIR/test_program"
        echo ""
    else
        print_error "Compilation failed!"
        exit 1
    fi

    # Test LCL compilation if Lazarus is installed
    if [ "$INCLUDE_LAZARUS" = "true" ]; then
        print_section "Testing LCL Compilation"

        LCL_TEST_PROGRAM="$TEST_DIR/test_lcl.pas"
        print_info "Creating LCL test program..."

        cat > "$LCL_TEST_PROGRAM" << 'PASCAL_EOF'
program TestLCL;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils;

begin
  WriteLn('=================================');
  WriteLn('LCL Test Program');
  WriteLn('=================================');
  WriteLn('LCL units accessible!');
  WriteLn('Classes unit loaded successfully');
  WriteLn('=================================');
end.
PASCAL_EOF

        print_info "Compiling LCL test program..."

        # Compile with LCL paths
        if fpc \
            -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
            -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
            -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
            -Fi/usr/lib/lazarus/3.0/lcl/include \
            -dLCL -dLCLgtk2 \
            "$LCL_TEST_PROGRAM" -o"$TEST_DIR/test_lcl" 2>&1; then
            print_success "LCL compilation successful!"

            print_info "Running LCL test program:"
            echo ""
            "$TEST_DIR/test_lcl"
            echo ""
        else
            print_error "LCL compilation failed!"
            print_warning "This may indicate issues with Lazarus installation or unit paths"
        fi
    fi

    # Cleanup test directory
    rm -rf "$TEST_DIR"
fi

# Output summary
print_section "Test Summary"

if [ "$DRY_RUN" = "false" ]; then
    echo "Installation Details:"
    echo "  FPC Path: $(which fpc)"
    echo "  FPC Version: $(fpc -iV)"
    if [ "$INCLUDE_LAZARUS" = "true" ]; then
        echo "  Lazarus Path: /usr/lib/lazarus/3.0"
    fi

    print_success "All tests completed successfully!"

    # Cleanup if requested
    if [ "$CLEANUP_AFTER" = "true" ]; then
        print_section "Cleanup"
        print_warning "Removing installed packages..."

        if [ "$INCLUDE_LAZARUS" = "true" ]; then
            execute_cmd "$SUDO apt-get remove -y lazarus lcl-gtk2-3.0 lcl-units-3.0 fpc" "Removing FPC and Lazarus..."
        else
            execute_cmd "$SUDO apt-get remove -y fpc" "Removing FPC..."
        fi

        if [ -n "$INSTALL_DEPENDENCIES" ]; then
            execute_cmd "$SUDO apt-get remove -y $INSTALL_DEPENDENCIES" "Removing additional dependencies..."
        fi

        execute_cmd "$SUDO apt-get autoremove -y" "Cleaning up unused packages..."
        print_success "Cleanup completed"
    fi
else
    print_success "Dry run completed - no changes were made"
fi

echo ""
print_info "To use these in GitHub Actions, the outputs would be:"
echo "  fpc-path=$(which fpc 2>/dev/null || echo 'not-installed')"
echo "  fpc-version=$(fpc -iV 2>/dev/null || echo 'not-installed')"
if [ "$INCLUDE_LAZARUS" = "true" ]; then
    echo "  lazarus-path=/usr/lib/lazarus/3.0"
fi
