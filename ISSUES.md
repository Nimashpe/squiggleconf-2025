# GitHub Issues

## Issue 1: Complete Org-babel Language Support

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

Basic support is implemented in `squiggleconf-babel.el` and documented in `README-BABEL.md`, but more robust error handling and platform-specific configuration is needed.

**Acceptance Criteria:**

- All code blocks in the repository can be executed on supported platforms
- Error handling for missing dependencies is graceful
- Documentation clearly explains how to set up the environment

**Labels:** enhancement, documentation, cross-platform