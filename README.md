# Setup Free Pascal Compiler (FPC) Action

A reusable GitHub Action for installing and configuring the Free Pascal Compiler (FPC) and Lazarus IDE on Linux, Windows, and macOS runners.

## Features

- 🚀 **Multi-platform support**: Works on Linux, Windows, and macOS
- 📦 **Easy to use**: Simple one-step setup for FPC and Lazarus
- ⚙️ **Configurable**: Control versions, dependencies, and installation options
- 🎯 **LCL Support**: Includes Lazarus Component Library for GUI applications
- 🔧 **Flexible**: Install FPC alone or with full Lazarus IDE

## Usage

### Basic Example

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

### Install FPC Only (No Lazarus)

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

### Install with Additional Dependencies

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

### Specify Lazarus Version

```yaml
steps:
  - uses: actions/checkout@v4

  - name: Setup FPC with Lazarus 3.0
    uses: cwbudde/action-fpc@v1
    with:
      lazarus-version: '3.0.0'

  - name: Build with LCL
    run: |
      fpc -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux \
          -Fu/usr/lib/lazarus/3.0/lcl/units/x86_64-linux/gtk2 \
          -dLCL -dLCLgtk2 \
          MyGuiApp.dpr
```

### Complete Multi-Platform Build Example

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

## Inputs

| Input | Description | Required | Default |
|-------|-------------|----------|---------|
| `lazarus-version` | Lazarus version to install (e.g., 3.0.0) | No | `3.0.0` |
| `fpc-version` | FPC version to install. If not specified, uses version bundled with Lazarus | No | `''` |
| `include-lazarus` | Whether to install Lazarus (includes LCL). Set to false for FPC-only installations | No | `true` |
| `install-dependencies` | Additional dependencies to install (space-separated) | No | `''` |
| `cache` | Enable caching of FPC/Lazarus installation (future feature) | No | `true` |

## Outputs

| Output | Description |
|--------|-------------|
| `fpc-path` | Path to the FPC compiler executable |
| `fpc-version` | Installed FPC version |
| `lazarus-path` | Path to the Lazarus installation directory |

### Using Outputs

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

## Platform-Specific Notes

### Linux (Ubuntu)

- Installs FPC and Lazarus from APT repositories
- Includes LCL-GTK2 libraries by default
- Additional dependencies installed via `apt-get`

### Windows

- Uses Chocolatey to install Lazarus (includes FPC)
- Automatically adds FPC to PATH
- Default installation path: `C:\lazarus`

### macOS

- Installs FPC via Homebrew
- Lazarus requires building from source (see advanced usage)
- Additional dependencies installed via `brew`

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

## Troubleshooting

### FPC not found in PATH (Windows)

The action automatically adds FPC to PATH, but if you encounter issues, manually add it:

```yaml
- name: Add FPC to PATH
  if: runner.os == 'Windows'
  run: |
    echo "C:\lazarus\fpc\3.2.2\bin\x86_64-win64" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
  shell: pwsh
```

### Missing LCL units

Ensure `include-lazarus` is set to `true` (default) and check the platform-specific paths in your build command.

### macOS compilation errors

For GUI applications on macOS, you may need to manually install and configure Lazarus LCL using the provided script.

## License

MIT

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

## Credits

Based on the FPC/Lazarus setup used in the [IAP project](https://github.com/cwbudde/iap).

## Support

If you encounter issues, please [open an issue](../../issues) on GitHub.
