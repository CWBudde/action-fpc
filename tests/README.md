# Testing Utilities

This directory contains testing utilities to verify the FPC and Lazarus installation without running the full GitHub Action.

## Local Testing Script

The `test-action-locally.sh` script (located in `../scripts/`) allows you to test the action's installation process locally, particularly useful for debugging Linux installation issues.

### Usage

```bash
# Basic test with FPC + Lazarus
./scripts/test-action-locally.sh

# Test FPC only (no Lazarus)
./scripts/test-action-locally.sh --no-lazarus

# Test with additional dependencies
./scripts/test-action-locally.sh --dependencies "libportaudio2 portaudio19-dev"

# Dry run (see what would be done without executing)
./scripts/test-action-locally.sh --dry-run

# Verbose output for debugging
./scripts/test-action-locally.sh --verbose

# Clean up after testing
./scripts/test-action-locally.sh --cleanup

# Skip the build test
./scripts/test-action-locally.sh --no-test-build
```

### Options

- `--no-lazarus` - Install FPC only, skip Lazarus
- `--dependencies DEPS` - Additional dependencies to install (space-separated)
- `--dry-run` - Show what would be done without executing
- `--verbose` - Show detailed output from commands
- `--cleanup` - Remove installation after testing
- `--no-test-build` - Skip building test programs
- `-h, --help` - Show help message

### What It Tests

1. **System Detection** - Verifies OS and architecture
2. **Package Installation** - Tests apt-get installation of FPC and Lazarus
3. **FPC Verification** - Checks FPC is properly installed and accessible
4. **Lazarus Verification** - Verifies Lazarus installation and LCL unit paths
5. **Compilation Test** - Compiles and runs test programs
6. **LCL Compilation Test** - Tests LCL-based compilation (if Lazarus installed)

## Verification Programs

### verify-installation.pas

Tests basic FPC functionality without LCL dependencies.

**To compile and run:**

```bash
cd tests
fpc verify-installation.pas
./verify-installation
```

**Tests:**
- FPC compiler information
- String operations
- Classes and objects (TStringList)
- File I/O operations

### verify-lcl.pas

Tests LCL (Lazarus Component Library) functionality.

**To compile and run on Linux:**

```bash
cd tests
fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
    -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
    -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
    -Fi/usr/lib/lazarus/3.0/lcl/include \
    -dLCL -dLCLgtk2 \
    verify-lcl.pas
./verify-lcl
```

**Or use the build script:**

```bash
cd tests
../scripts/build-with-lcl.sh verify-lcl.pas
./verify-lcl
```

**Tests:**
- LCL unit availability
- Form creation
- Control components (Button, Label)
- Graphics operations (TBitmap)

## Debugging Linux Installation Issues

If you're experiencing issues with the Linux installation:

1. **Run the test script in verbose mode:**
   ```bash
   ./scripts/test-action-locally.sh --verbose
   ```

2. **Check package availability:**
   ```bash
   apt-cache search lazarus
   apt-cache search fpc
   ```

3. **Verify unit paths:**
   ```bash
   find /usr/lib/lazarus -name "*.ppu" 2>/dev/null | head -20
   ```

4. **Test FPC directly:**
   ```bash
   fpc -i
   fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux -iV
   ```

5. **Check for missing dependencies:**
   ```bash
   apt-cache policy lazarus lcl-gtk2-3.0 lcl-units-3.0
   ```

## Common Issues

### LCL units not found

**Problem:** Compilation fails with "Can't find unit Classes" or similar

**Solution:**
- Verify Lazarus is installed: `dpkg -l | grep lazarus`
- Check unit paths exist: `ls -la /usr/lib/lazarus/3.0/lcl/units/`
- Use the correct architecture path (x86_64-linux, i386-linux, etc.)

### Package version mismatch

**Problem:** `lcl-gtk2-3.0` or `lcl-units-3.0` not available

**Solution:**
- Check available versions: `apt-cache search lcl-gtk2`
- Update action.yml to use the correct version numbers
- Or install from PPA for newer versions

### GTK2 dependencies missing

**Problem:** LCL compilation succeeds but runtime fails

**Solution:**
```bash
sudo apt-get install libgtk2.0-dev
```

## Testing in Docker

To test in a clean Linux environment:

```bash
docker run -it --rm -v $(pwd):/workspace ubuntu:22.04 bash
cd /workspace
./scripts/test-action-locally.sh
```

## CI/CD Testing

To test the actual GitHub Action locally using [act](https://github.com/nektos/act):

```bash
# Install act
curl https://raw.githubusercontent.com/nektos/act/master/install.sh | sudo bash

# Run the action
act -j test
```
