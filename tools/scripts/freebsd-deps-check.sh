#!/bin/sh
# FreeBSD dependencies check for SquiggleConf 2025 repository
# This script checks for required dependencies for working with the repository

echo "=== SquiggleConf 2025 FreeBSD Dependencies Check ==="
echo "Checking system information..."
uname -a
echo ""

# Function to check if a package is installed
check_pkg() {
  if pkg info -e "$1" >/dev/null 2>&1; then
    echo "✓ $1 is installed"
    return 0
  else
    echo "✗ $1 is not installed"
    return 1
  fi
}

# Function to check if a command exists
check_command() {
  if command -v "$1" >/dev/null 2>&1; then
    echo "✓ $1 command is available"
    version=$($1 --version 2>&1 | head -n 1)
    echo "  Version: $version"
    return 0
  else
    echo "✗ $1 command is not available"
    return 1
  fi
}

# Check for basic dependencies
echo "=== Checking Basic Dependencies ==="
check_pkg git
check_pkg emacs || check_pkg editors/emacs
check_pkg bash || check_pkg shells/bash
check_pkg gmake || check_pkg devel/gmake
echo ""

# Check for Node.js and npm
echo "=== Checking Node.js Environment ==="
check_command node
check_command npm
check_command npx

# Check for TypeScript
if check_command tsc; then
  HAS_TS=true
else
  echo "  TypeScript can be installed globally with: npm install -g typescript"
  HAS_TS=false
fi
echo ""

# Check for Rust toolchain
echo "=== Checking Rust Toolchain ==="
check_command rustc
check_command cargo
check_command rustup

if [ $? -ne 0 ]; then
  echo "  Rust can be installed with: pkg install rust || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
fi
echo ""

# Check for Go
echo "=== Checking Go Toolchain ==="
check_command go
if [ $? -ne 0 ]; then
  echo "  Go can be installed with: pkg install go"
fi
echo ""

# Check for WebAssembly tools
echo "=== Checking WebAssembly Tools ==="
check_command wasm-pack
if [ $? -ne 0 ]; then
  echo "  wasm-pack can be installed with: cargo install wasm-pack"
fi

check_command wasmer
if [ $? -ne 0 ]; then
  echo "  wasmer can be installed with: pkg install wasmer || curl https://get.wasmer.io -sSfL | sh"
fi
echo ""

# Check for Python (needed for scripts)
echo "=== Checking Python Environment ==="
check_command python || check_command python3
check_command pip || check_command pip3

echo "  For Python data visualization (optional):"
PIP_CMD="pip"
if ! command -v pip >/dev/null 2>&1 && command -v pip3 >/dev/null 2>&1; then
  PIP_CMD="pip3"
fi
echo "  $PIP_CMD install matplotlib numpy"
echo ""

# Installation recommendations
echo "=== FreeBSD Installation Recommendations ==="
echo "To install missing packages, you can use:"
echo ""
echo "# Basic tools"
echo "pkg install git bash emacs gmake"
echo ""
echo "# Node.js and npm"
echo "pkg install node npm"
echo "npm install -g typescript"
echo ""
echo "# Rust toolchain"
echo "pkg install rust || curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh"
echo ""
echo "# Go toolchain"
echo "pkg install go"
echo ""
echo "# WebAssembly tools"
echo "pkg install wasmer || curl https://get.wasmer.io -sSfL | sh"
echo "cargo install wasm-pack"
echo ""
echo "# Python tools"
echo "pkg install python3 py39-pip"
echo "$PIP_CMD install matplotlib numpy"
echo ""

# Compatibility matrix
echo "=== FreeBSD Compatibility Matrix ==="
echo "FreeBSD 14.x: Full compatibility (current system)"
echo "FreeBSD 13.x: Full compatibility"
echo "FreeBSD 12.x: Compatible with package adjustments"
echo "FreeBSD 11.x: Limited compatibility, upgrade recommended"
echo ""

echo "=== Check Complete ==="
