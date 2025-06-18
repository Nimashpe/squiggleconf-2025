# GitHub Issues

## Issue 1: Complete Org-babel Language Support [RESOLVED]

**Title:** Improve cross-platform Org-babel language support

**Description:**

We need to enhance Org-babel language support for the SquiggleConf 2025 repository to ensure all code blocks execute properly across different platforms.

**Requirements:**

1. Support for all languages used in the repository:
   - JavaScript/Node.js
   - TypeScript
   - Python
   - Shell scripts
   - Mermaid diagrams
   - JSON
   - Rust
   - GraphViz (dot)
   - Racket
   - Zig
   - OCaml
   - Scheme/Guile

2. Cross-platform compatibility:
   - FreeBSD
   - Linux (Ubuntu/Debian, Fedora/RHEL)
   - macOS
   - Windows (WSL or native)

3. Installation instructions for:
   - Emacs packages
   - External dependencies
   - CLI tools (e.g., Mermaid CLI)

4. Fallback mechanisms for missing dependencies

**Current Status:**

RESOLVED. Enhanced support is implemented in `squiggleconf-babel.el` with fallback mechanisms for Mermaid, Racket, Zig, OCaml, and other languages. Documentation has been updated in `README-BABEL.md` with comprehensive installation instructions and platform-specific notes.

System information in `SYSTEM.org` and `CONTRIBUTORS.org` has been updated to include all relevant language tools installed on the system, including Racket, Guile, OCaml, Zig, and Bison.

**Acceptance Criteria:**

- ✓ All code blocks in the repository can be executed on supported platforms
- ✓ Error handling for missing dependencies is graceful
- ✓ Documentation clearly explains how to set up the environment

**Labels:** enhancement, documentation, cross-platform, resolved

## Issue 2: Test Mermaid Diagram Generation

**Title:** Test Mermaid diagram generation on systems with mermaid-cli installed

**Description:**

While we've implemented fallback mechanisms for systems without mermaid-cli, we need to verify that diagram generation works correctly on systems where it is installed.

**Requirements:**

1. Test Mermaid diagram generation on:
   - Linux Docker environment
   - macOS with Homebrew
   - Windows with npm

2. Ensure that diagrams are properly rendered and exported to specified file paths

**Current Status:**

Pending. Implementation is complete, but testing on systems with mermaid-cli is needed.

**Acceptance Criteria:**

- All Mermaid diagrams in session notes render correctly
- File paths are correctly handled
- Error handling for syntax errors is graceful

**Labels:** testing, documentation, cross-platform