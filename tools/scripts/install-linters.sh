#!/bin/sh
# Script to install linters for SquiggleConf 2025
# This script detects the platform and installs the appropriate linters

set -e

echo "Installing linters for SquiggleConf 2025..."

install_freebsd_linters() {
  echo "Installing FreeBSD linters..."
  sudo pkg install -y
  sudo npm install -g eslint jsonlint
  pip install pylint ruff yamllint
}

install_debian_linters() {
  echo "Installing Debian/Ubuntu linters..."
  sudo apt-get update
  sudo apt-get install -y shellcheck golang pylint yamllint clang-format
  sudo npm install -g eslint jsonlint
  pip install --user ruff
  GO111MODULE=on go install mvdan.cc/sh/v3/cmd/shfmt@latest
}

install_redhat_linters() {
  echo "Installing RHEL/CentOS/Fedora linters..."
  sudo dnf install -y ShellCheck golang python3-pylint yamllint clang-tools-extra
  sudo npm install -g eslint jsonlint
  pip install --user ruff
  GO111MODULE=on go install mvdan.cc/sh/v3/cmd/shfmt@latest
}

install_macos_linters() {
  echo "Installing macOS linters..."
  brew install shellcheck shfmt pylint ruff yamllint clang-format
  npm install -g eslint jsonlint
}

# Detect OS and install appropriate linters
case "$(uname -s)" in
  FreeBSD)
    echo "Detected FreeBSD system"
    install_freebsd_linters
    ;;

  Linux)
    echo "Detected Linux system"
    if [ -f /etc/debian_version ]; then
      echo "Debian/Ubuntu based system"
      install_debian_linters
    elif [ -f /etc/redhat-release ]; then
      echo "RHEL/CentOS/Fedora based system"
      install_redhat_linters
    else
      echo "Unsupported Linux distribution. Please install linters manually."
      exit 1
    fi
    ;;

  Darwin)
    echo "Detected macOS system"
    if ! which brew >/dev/null; then
      echo "Homebrew not found. Please install Homebrew first: https://brew.sh/"
      exit 1
    fi
    install_macos_linters
    ;;

  *)
    echo "Unsupported operating system: $(uname -s)"
    echo "Please install linters manually"
    exit 1
    ;;
esac

echo "Linter installation completed successfully!"
echo "You can now use 'make lint' to lint all files in the repository."