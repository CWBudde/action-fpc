# Setup Free Pascal Compiler

[![GitHub Release](https://img.shields.io/github/v/release/cwbudde/action-fpc?label=Version)](https://github.com/cwbudde/action-fpc/releases)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](LICENSE)
[![GitHub Actions](https://img.shields.io/badge/GitHub_Actions-2088FF?logo=github-actions&logoColor=white)](https://github.com/features/actions)

> A **fast** and **reliable** GitHub Action for installing Free Pascal Compiler (FPC) and optionally Lazarus IDE across all major platforms.

**Quick Links:** [Quick Start](#quick-start) • [Examples](#usage-examples) • [API Reference](#api-reference) • [Troubleshooting](#troubleshooting)

---

## Table of Contents

- [Features](#features)
- [Quick Start](#quick-start)
- [Why Use This Action?](#why-use-this-action)
- [Usage Examples](#usage-examples)
  - [Basic Usage](#basic-usage)
  - [FPC Only](#fpc-only-no-lazarus)
  - [With Dependencies](#with-additional-dependencies)
  - [Multi-Platform Build](#complete-multi-platform-example)
- [API Reference](#api-reference)
  - [Inputs](#inputs)
  - [Outputs](#outputs)
- [Platform Details](#platform-specific-details)
- [Build Patterns](#common-build-patterns)
- [Advanced Usage](#advanced-usage)
- [Troubleshooting](#troubleshooting)
- [FAQ](#faq)
- [Contributing](#contributing)

---

## Features

- ✨ **Zero Configuration** - Works out of the box with sensible defaults
- 🚀 **Multi-Platform** - Ubuntu, Windows, and macOS support
- ⚡ **Fast Setup** - Optimized installation process
- 🎯 **Dual Mode** - Install FPC alone OR FPC + Lazarus IDE
- 📦 **LCL Support** - Optional Lazarus Component Library for GUI apps
- 🔧 **Flexible** - Control versions, dependencies, and installation options
- 🎯 **Latest Versions** - FPC 3.2.2 and Lazarus 3.0 by default
- 📝 **Well Documented** - Comprehensive examples and guides

---

## Quick Start

### For FPC + Lazarus (GUI Applications)

```yaml
- name: Setup FPC with Lazarus
  uses: cwbudde/action-fpc@v1  # Lazarus included by default

- name: Build
  run: fpc MyProject.dpr
```

### For FPC Only (Console/Non-GUI Applications)

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    include-lazarus: false  # Faster installation

- name: Build
  run: fpc MyProgram.pas
```

That's it! Choose the mode that fits your project.

---

## Why Use This Action?

### Before (Manual Setup)

```yaml
- name: Install FPC and Lazarus
  run: |
    sudo apt-get update
    sudo apt-get install -y fpc lazarus lcl-gtk2-3.0 lcl-units-3.0
    sudo apt-get install -y libportaudio2 portaudio19-dev

- name: Display FPC version
  run: fpc -iV

- name: Build project
  run: |
    cd "Demos/My App"
    fpc -Fu../../Source \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
        -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
        -Fi/usr/lib/lazarus/3.0/lcl/include \
        -dLCL -dLCLgtk2 \
        MyApp.dpr
```

### After (With This Action)

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    install-dependencies: 'libportaudio2 portaudio19-dev'

- name: Build project
  run: |
    cd "Demos/My App"
    fpc -Fu../../Source \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
        -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
        -Fi/usr/lib/lazarus/3.0/lcl/include \
        -dLCL -dLCLgtk2 \
        MyApp.dpr
```

**Benefits:**
- ✅ One-line setup instead of multiple commands
- ✅ Works across Linux, Windows, and macOS
- ✅ Automatic PATH configuration
- ✅ Version pinning support
- ✅ Consistent behavior across all platforms

---

## Usage Examples

### Basic Usage

```yaml
name: Build

on: [push, pull_request]

jobs:
  build:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]

    steps:
      - uses: actions/checkout@v4

      - name: Setup FPC
        uses: cwbudde/action-fpc@v1

      - name: Build project
        run: fpc MyProject.dpr
```

### FPC Only (No Lazarus)

```yaml
steps:
  - uses: actions/checkout@v4

  - name: Setup FPC
    uses: cwbudde/action-fpc@v1
    with:
      include-lazarus: false

  - name: Compile
    run: fpc -Mobjfpc -Sh MyProgram.pas
```

### With Additional Dependencies

```yaml
steps:
  - uses: actions/checkout@v4

  - name: Setup FPC with PortAudio
    uses: cwbudde/action-fpc@v1
    with:
      install-dependencies: 'libportaudio2 portaudio19-dev'

  - name: Build audio application
    run: fpc -Fu./Source MyAudioApp.dpr
```

### Complete Multi-Platform Example

```yaml
name: Build & Release

on:
  push:
    branches: [ main, develop ]
  pull_request:
    branches: [ main ]

jobs:
  build:
    name: Build on ${{ matrix.os }}
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        include:
          - os: ubuntu-latest
            executable: MyApp
          - os: windows-latest
            executable: MyApp.exe
          - os: macos-latest
            executable: MyApp

    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Setup FPC and Lazarus
        uses: cwbudde/action-fpc@v1
        with:
          lazarus-version: '3.0.0'
          install-dependencies: 'libportaudio2 portaudio19-dev'  # Linux only

      - name: Display FPC version
        run: fpc -iV

      - name: Build project
        shell: bash
        run: |
          if [ "$RUNNER_OS" == "Windows" ]; then
            fpc -Fu./Source -FuC:\\lazarus\\lcl\\units\\x86_64-win64 MyApp.dpr
          else
            fpc -Fu./Source MyApp.dpr
          fi

      - name: Upload artifacts
        uses: actions/upload-artifact@v4
        if: success()
        with:
          name: ${{ matrix.os }}-binary
          path: ${{ matrix.executable }}
          retention-days: 7
```

---

## API Reference

### Inputs

All inputs are **optional** and have sensible defaults.

| Input | Description | Type | Default | Example |
|-------|-------------|------|---------|---------|
| `lazarus-version` | Lazarus version to install | String | `3.0.0` | `3.0.0`, `2.2.6` |
| `fpc-version` | FPC version (uses bundled if not specified) | String | `''` | `3.2.2` |
| `include-lazarus` | Install Lazarus and LCL | Boolean | `true` | `true`, `false` |
| `install-dependencies` | Additional system packages (space-separated) | String | `''` | `libportaudio2 libsdl2-dev` |
| `cache` | Enable installation caching (future) | Boolean | `true` | `true`, `false` |

**Input Details:**

#### `lazarus-version`
Specifies which Lazarus version to install. Currently supports version `3.0.0` on all platforms. The version must match what's available in the package manager (APT for Linux, Chocolatey for Windows, built from source for macOS).

#### `fpc-version`
Normally left empty to use the FPC version bundled with Lazarus. Only specify if you need a specific FPC version without Lazarus.

#### `include-lazarus`
Set to `false` if you only need the FPC compiler without the Lazarus IDE or LCL. This results in faster installation.

#### `install-dependencies`
Platform-specific package names:
- **Linux**: APT package names (e.g., `libportaudio2 portaudio19-dev`)
- **macOS**: Homebrew formula names (e.g., `portaudio sdl2`)
- **Windows**: Not currently used (dependencies handled via Chocolatey)

### Outputs

| Output | Description | Example Value |
|--------|-------------|---------------|
| `fpc-path` | Full path to FPC compiler executable | `/usr/bin/fpc` |
| `fpc-version` | Installed FPC version string | `3.2.2` |
| `lazarus-path` | Lazarus installation directory | `/usr/lib/lazarus/3.0` |

#### Using Outputs

```yaml
steps:
  - name: Setup FPC
    id: fpc-setup
    uses: cwbudde/action-fpc@v1

  - name: Display installation info
    run: |
      echo "FPC installed at: ${{ steps.fpc-setup.outputs.fpc-path }}"
      echo "FPC version: ${{ steps.fpc-setup.outputs.fpc-version }}"
      echo "Lazarus path: ${{ steps.fpc-setup.outputs.lazarus-path }}"
```

---

## Platform-Specific Details

<table>
<tr>
<th>Platform</th>
<th>Package Manager</th>
<th>FPC Version</th>
<th>Installation Path</th>
<th>LCL Widget</th>
</tr>
<tr>
<td><b>Ubuntu</b></td>
<td>APT</td>
<td>3.2.2</td>
<td><code>/usr/bin/fpc</code></td>
<td>GTK2</td>
</tr>
<tr>
<td><b>Windows</b></td>
<td>Chocolatey</td>
<td>3.2.2</td>
<td><code>C:\lazarus</code></td>
<td>Win32</td>
</tr>
<tr>
<td><b>macOS</b></td>
<td>Homebrew</td>
<td>3.2.2</td>
<td><code>/opt/homebrew/bin/fpc</code></td>
<td>Cocoa*</td>
</tr>
</table>

<sup>* macOS Cocoa widget requires manual Lazarus build (see Advanced Usage)</sup>

### Linux (Ubuntu)

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    install-dependencies: 'libportaudio2 portaudio19-dev libsdl2-dev'
```

**Details:**
- ✅ Installs from official Ubuntu APT repositories
- ✅ Includes LCL-GTK2 libraries by default
- ✅ Fast installation (~2 minutes)
- 📍 LCL units: `/usr/lib/lazarus/3.0/lcl/units/x86_64-linux`

### Windows

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    lazarus-version: '3.0.0'
```

**Details:**
- ✅ Uses Chocolatey package manager
- ✅ Automatically adds FPC to PATH
- ✅ Includes Win32 LCL libraries
- 📍 FPC binary: `C:\lazarus\fpc\3.2.2\bin\x86_64-win64\fpc.exe`
- 📍 LCL units: `C:\lazarus\lcl\units\x86_64-win64`

### macOS

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    install-dependencies: 'portaudio'
```

**Details:**
- ✅ Installs FPC via Homebrew
- ⚠️ Lazarus requires manual build from source
- 📍 FPC binary: `/opt/homebrew/bin/fpc` (Apple Silicon) or `/usr/local/bin/fpc` (Intel)
- 💡 For GUI apps, see [Building with LCL on macOS](#building-with-lcl-on-macos)

### Version Compatibility Matrix

| Action Version | FPC Version | Lazarus Version | Ubuntu | Windows | macOS |
|----------------|-------------|-----------------|--------|---------|-------|
| v1.x | 3.2.2 | 3.0.0 | ✅ 22.04+ | ✅ Server 2022+ | ✅ 12+ |
| Future | 3.2.2+ | 3.2+ | ✅ | ✅ | ✅ |

## Advanced Usage

### Building with LCL on macOS

For macOS projects requiring Lazarus LCL, use the provided helper script:

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    install-dependencies: 'portaudio'

- name: Install Lazarus LCL (macOS)
  if: runner.os == 'macOS'
  run: |
    chmod +x ${{ github.action_path }}/scripts/install-lazarus-macos.sh
    ${{ github.action_path }}/scripts/install-lazarus-macos.sh

- name: Build with LCL
  run: |
    chmod +x ${{ github.action_path }}/scripts/build-with-lcl.sh
    ${{ github.action_path }}/scripts/build-with-lcl.sh MyApp.dpr -Fu./Source
```

### Custom Compiler Flags

```yaml
- name: Build with custom flags
  run: |
    fpc -O3 -XX -CX -Xs \
        -Fu./Source \
        -Fi./Include \
        -FE./bin \
        -FU./lib \
        MyProject.dpr
```

## Common Build Patterns

### Console Application

```yaml
- name: Build console app
  run: fpc -Mobjfpc -Sh -CX MyConsoleApp.pas
```

### LCL GUI Application (Linux)

```yaml
- name: Build GUI app
  run: |
    fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
        -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
        -Fi/usr/lib/lazarus/3.0/lcl/include \
        -dLCL -dLCLgtk2 \
        MyGuiApp.dpr
```

### LCL GUI Application (Windows)

```yaml
- name: Build GUI app
  shell: cmd
  run: |
    fpc -FuC:\lazarus\lcl\units\x86_64-win64 ^
        -FuC:\lazarus\lcl\units\x86_64-win64\win32 ^
        -FuC:\lazarus\components\lazutils\lib\x86_64-win64 ^
        -FiC:\lazarus\lcl\include ^
        -dLCL -dLCLwin32 ^
        MyGuiApp.dpr
```

---

## Troubleshooting

### Common Issues

<details>
<summary><b>❌ FPC not found in PATH (Windows)</b></summary>

**Symptom:** `'fpc' is not recognized as an internal or external command`

**Solution:** The action automatically adds FPC to PATH, but the change may not take effect immediately. Try:

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1

- name: Verify FPC in PATH
  shell: pwsh
  run: |
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path","Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path","User")
    fpc -iV

# Or manually add to PATH
- name: Add FPC to PATH (manual)
  if: runner.os == 'Windows'
  run: |
    echo "C:\lazarus\fpc\3.2.2\bin\x86_64-win64" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
  shell: pwsh
```

</details>

<details>
<summary><b>❌ Error: Can't find unit X used by Y</b></summary>

**Symptom:** Compilation fails with missing unit errors

**Possible Causes:**
1. Missing Lazarus installation (need `include-lazarus: true`)
2. Incorrect unit search paths in compiler flags
3. Missing dependencies

**Solution:**

```yaml
# Ensure Lazarus is installed
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  with:
    include-lazarus: true  # Default, but make explicit

# Check your -Fu paths match the platform
- name: Build (Linux)
  if: runner.os == 'Linux'
  run: |
    fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
        -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
        -Fu/usr/lib/lazarus/3.0/components/lazutils/lib/x86_64-linux \
        MyApp.dpr
```

</details>

<details>
<summary><b>❌ Chocolatey installation fails on Windows</b></summary>

**Symptom:** Lazarus installation times out or fails

**Solution:** Increase timeout and add retry logic:

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1
  timeout-minutes: 30
  with:
    lazarus-version: '3.0.0'

# Or install manually with retry
- name: Manual Chocolatey install
  shell: pwsh
  run: |
    $attempt = 0
    $maxAttempts = 3
    while ($attempt -lt $maxAttempts) {
      try {
        choco install lazarus --version=3.0.0 -y --timeout=1800
        break
      } catch {
        $attempt++
        if ($attempt -eq $maxAttempts) { throw }
        Start-Sleep -Seconds 30
      }
    }
```

</details>

<details>
<summary><b>❌ macOS LCL application won't compile</b></summary>

**Symptom:** Missing Cocoa LCL units on macOS

**Solution:** macOS requires building Lazarus from source:

```yaml
- name: Setup FPC
  uses: cwbudde/action-fpc@v1

- name: Build Lazarus LCL for Cocoa
  if: runner.os == 'macOS'
  run: |
    # Clone this action repo for the scripts
    git clone https://github.com/cwbudde/action-fpc.git /tmp/action-fpc
    chmod +x /tmp/action-fpc/scripts/install-lazarus-macos.sh
    /tmp/action-fpc/scripts/install-lazarus-macos.sh

- name: Build with Cocoa LCL
  if: runner.os == 'macOS'
  run: |
    ARCH=$(uname -m)
    if [ "$ARCH" = "arm64" ]; then
      DARWIN_ARCH="aarch64-darwin"
    else
      DARWIN_ARCH="x86_64-darwin"
    fi

    fpc -Fu/usr/local/share/lazarus/lcl/units/$DARWIN_ARCH \
        -Fu/usr/local/share/lazarus/lcl/units/$DARWIN_ARCH/cocoa \
        -dLCL -dLCLcocoa \
        MyApp.dpr
```

</details>

<details>
<summary><b>❌ Slow installation times</b></summary>

**Symptom:** Setup takes 5+ minutes

**Solution:** Consider these optimizations:

```yaml
# 1. Use FPC-only if you don't need Lazarus
- name: Setup FPC (fast)
  uses: cwbudde/action-fpc@v1
  with:
    include-lazarus: false

# 2. Cache dependencies (future feature)
- name: Setup FPC (with cache)
  uses: cwbudde/action-fpc@v1
  with:
    cache: true
```

</details>

### Debug Tips

**Check installed versions:**
```bash
fpc -iV                    # FPC version
fpc -i                     # Full FPC info
ls /usr/lib/lazarus/3.0/   # Check Lazarus (Linux)
```

**Verify unit paths:**
```bash
# Linux
find /usr/lib/lazarus -name "*.ppu" | head -20

# Windows (PowerShell)
Get-ChildItem C:\lazarus\lcl\units -Recurse -Filter *.ppu | Select-Object -First 20

# macOS
find /usr/local/share/lazarus -name "*.ppu" | head -20
```

**Test basic compilation:**
```bash
echo 'program test; begin writeln("OK"); end.' > test.pas
fpc test.pas
./test  # Should print "OK"
```

---

## FAQ

<details>
<summary><b>Q: Does this work with Free Pascal 3.0.x or earlier versions?</b></summary>

Currently, this action installs FPC 3.2.2 which is the latest stable version. Support for custom FPC versions may be added in future releases. For now, you can use the `fpc-version` input (experimental) or install manually.

</details>

<details>
<summary><b>Q: Can I use this action with Delphi projects?</b></summary>

This action is designed for Free Pascal/Lazarus projects. While FPC has Delphi compatibility mode (`-Mdelphi`), it may not support all Delphi-specific features or libraries. Test thoroughly before migrating Delphi projects.

</details>

<details>
<summary><b>Q: Why does Windows installation take longer than Linux?</b></summary>

Windows uses Chocolatey to download and install the full Lazarus package (~200MB), which includes the IDE, compiler, and libraries. Linux uses pre-compiled APT packages that are smaller and faster to install.

</details>

<details>
<summary><b>Q: Can I build 32-bit executables?</b></summary>

Yes, but you need to specify the target architecture:

```bash
# 32-bit Windows
fpc -Twin32 -Pi386 MyApp.dpr

# 32-bit Linux
fpc -Tlinux -Pi386 MyApp.dpr
```

Note: 32-bit libraries must be available on the runner.

</details>

<details>
<summary><b>Q: How do I cross-compile for other platforms?</b></summary>

Cross-compilation requires installing FPC cross-compilers. This is not currently automated by this action. You would need to:

1. Install the base FPC (via this action)
2. Install cross-compilation units manually
3. Use `-T<target>` and `-P<cpu>` flags

Example for Linux → Windows:
```bash
sudo apt-get install fpc-3.2.2-win32
fpc -Twin64 -Px86_64 MyApp.dpr
```

</details>

<details>
<summary><b>Q: Is caching supported to speed up workflows?</b></summary>

Caching is planned for a future release. For now, the `cache` input is accepted but not yet implemented. You can manually cache the installation directories using GitHub's `actions/cache`:

```yaml
- name: Cache FPC
  uses: actions/cache@v3
  with:
    path: |
      /usr/lib/lazarus
      ~/.fpc
    key: ${{ runner.os }}-fpc-${{ hashFiles('**/*.dpr', '**/*.pas') }}
```

</details>

<details>
<summary><b>Q: Can I use this in a self-hosted runner?</b></summary>

Yes, but ensure your self-hosted runner has the required package managers:
- **Linux**: `apt-get` available
- **Windows**: Chocolatey installed
- **macOS**: Homebrew installed

The action will use these to install FPC/Lazarus.

</details>

<details>
<summary><b>Q: How do I report bugs or request features?</b></summary>

Please [open an issue](../../issues) on GitHub with:
- Your workflow YAML
- Runner OS and version
- Error messages or logs
- Expected vs actual behavior

</details>

---

## Contributing

Contributions are welcome! Here's how you can help:

- 🐛 **Report bugs** - [Open an issue](../../issues)
- 💡 **Suggest features** - [Start a discussion](../../discussions)
- 🔧 **Submit fixes** - [Create a pull request](../../pulls)
- 📖 **Improve docs** - Documentation PRs are always appreciated!

### Development

```bash
git clone https://github.com/cwbudde/action-fpc.git
cd action-fpc

# Test locally with act (https://github.com/nektos/act)
act -j test-fpc-only
```

---

## Credits & License

**Author:** Christian-W. Budde

**License:** [MIT License](LICENSE)

**Based on:** FPC/Lazarus workflows from the [IAP project](https://github.com/cwbudde/iap)

**Inspired by:** GitHub Actions ecosystem and Pascal community contributions

---

## Star History

If this action helped you, consider giving it a ⭐ on GitHub!

---

## Related Projects

- [Free Pascal Compiler](https://www.freepascal.org/) - The compiler itself
- [Lazarus IDE](https://www.lazarus-ide.org/) - RAD IDE for Free Pascal
- [fpcupdeluxe](https://github.com/LongDirtyAnimAlf/fpcupdeluxe) - FPC/Lazarus installer
- [setup-lazarus](https://github.com/gcarreno/setup-lazarus) - Alternative Lazarus action

---

**[⬆ Back to Top](#setup-free-pascal-compiler)**

---

*Made with ❤️ for the Pascal community*
