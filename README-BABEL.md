# Org Babel Setup for SquiggleConf 2025

This document explains how to set up Org Babel for working with the SquiggleConf 2025 notes repository.

## Required Languages

The notes in this repository use several code block languages:

- JavaScript/Node.js (js)
- TypeScript (typescript)
- Python (python)
- Shell scripts (shell)
- Mermaid diagrams (mermaid)
- JSON (json)
- Rust (rust)
- GraphViz (dot)

## Basic Setup

For a minimal setup, add this to your Emacs configuration:

```elisp
;; Load the SquiggleConf Babel setup
(load-file "/path/to/squiggleconf-2025/squiggleconf-babel.el")
```

This will configure the basic language support for Org Babel.

## Complete Setup

For a complete setup with all features, you'll need:

1. Emacs 26.1+ (30.0+ recommended)
2. Org mode 9.3+
3. The following packages:
   - ob-mermaid (for Mermaid diagrams)
   - typescript-mode (for TypeScript)
   - json-mode (for JSON)
   - rust-mode (for Rust)

### Installation with use-package

```elisp
;; Install required packages
(use-package org
  :ensure t)

(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/path/to/mmdc"))

(use-package typescript-mode
  :ensure t)

(use-package json-mode
  :ensure t)

(use-package rust-mode
  :ensure t)

;; Load the SquiggleConf Babel setup
(load-file "/path/to/squiggleconf-2025/squiggleconf-babel.el")
```

## Platform-Specific Notes

### FreeBSD

On FreeBSD, you may need to install Node.js and the Mermaid CLI:

```sh
pkg install node npm
npm install -g @mermaid-js/mermaid-cli
```

### Linux

On Linux, install the required dependencies:

```sh
# Ubuntu/Debian
apt-get install nodejs npm
npm install -g @mermaid-js/mermaid-cli

# Fedora/RHEL
dnf install nodejs npm
npm install -g @mermaid-js/mermaid-cli
```

### macOS

On macOS, using Homebrew:

```sh
brew install node
npm install -g @mermaid-js/mermaid-cli
```

## Troubleshooting

If you encounter issues with specific languages:

1. Check that the language is supported in your Org Babel configuration
2. Ensure any external programs (like Node.js or Mermaid CLI) are installed
3. Try manually loading the required mode (e.g., `(require 'json-mode)`)

For Mermaid diagrams without the CLI, the placeholder function in `squiggleconf-babel.el` will prevent errors but won't generate diagrams.

## Manual Testing

You can test your setup by executing the following in Emacs:

```elisp
(require 'org)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (python . t)
   (shell . t)
   (mermaid . t)
   (json . t)))
```

Then try to execute a code block with `C-c C-c`.