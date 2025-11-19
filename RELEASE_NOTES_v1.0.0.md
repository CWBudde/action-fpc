# Release v1.0.0 - Initial Release

## Free Pascal Compiler GitHub Action

This is the initial release of the Free Pascal Compiler (FPC) setup action for GitHub workflows.

## Features

- **Multi-Platform Support**: Works on Ubuntu, macOS, and Windows runners
- **Version Flexibility**: Install specific FPC versions or use 'latest'
- **Architecture Support**: Configurable CPU architecture (x86_64, i386, aarch64, etc.)
- **Compiler Target Selection**: Choose between FPC or FPC-Source installations
- **Cached Downloads**: Automatically caches downloaded compiler packages for faster subsequent runs

## Usage

Add this action to your GitHub workflow:

```yaml
- uses: CWBudde/action-fpc@v1
  with:
    fpc-version: 'latest'  # or specific version like '3.2.2'
    os-family: 'Linux'      # Linux, Darwin, or Win64
    cpu-architecture: 'x86_64'
    compiler-target: 'fpc'
```

## What's Included

- Automated FPC installation for CI/CD pipelines
- Support for various operating systems and architectures
- Intelligent version resolution and download management
- Comprehensive error handling and logging

## Example Workflow

```yaml
name: Build with FPC
on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: CWBudde/action-fpc@v1
        with:
          fpc-version: 'latest'
      - name: Compile
        run: fpc yourprogram.pas
```

## Version Tags

- `@v1` - Always points to the latest v1.x.x release (recommended for auto-updates)
- `@v1.0.0` - Pinned to this specific release (recommended for stability)

## Documentation

See [README.md](README.md) for complete documentation and advanced usage examples.
