# System Information and Installed Tools

## System Information
```
FreeBSD nexushive 14.2-RELEASE FreeBSD 14.2-RELEASE releng/14.2-n269506-c8918d6c7412 GENERIC amd64
```

## Installed Development Tools

| Tool/Language | Version | Status |
|---------------|---------|--------|
| Node.js | v22.14.0 | ✓ Installed |
| npm | 10.9.2 | ✓ Installed |
| Deno | 2.1.4 (stable, release, x86_64-unknown-freebsd) | ✓ Installed |
| Rust | 1.85.0 (4d91de4e4 2025-02-17) | ✓ Installed |
| wasm-pack | 0.13.1 | ✓ Installed |
| Go | go1.21.13 freebsd/amd64 | ✓ Installed |
| TypeScript | Via Deno 5.6.2 | ✓ Installed |
| Emacs | GNU Emacs 30.1 | ✓ Installed |
| Graphviz | 12.2.1 | ✓ Installed |
| Babel | Python Babel 2.17.0 | ✓ Installed |

## Recommended Additional Packages

Before the conference, consider installing:

1. **TypeScript for Node.js (optional)**
   ```
   npm install -g typescript
   ```
   - While TypeScript is available via Deno, having the standalone TypeScript compiler might be useful

2. **Mermaid CLI**
   ```
   npm install -g @mermaid-js/mermaid-cli
   ```
   - Required for rendering Mermaid diagrams from Org files

3. **Browser JavaScript Frameworks (if needed)**
   ```
   npm install -g create-react-app
   npm install -g @vue/cli
   ```
   - Useful if planning to follow along with framework examples

4. **Jupyter Notebook Support**
   ```
   pkg install py39-jupyter-notebook
   ```
   - For running any Python notebook examples

5. **LaTeX (if planning to export PDFs)**
   ```
   pkg install texlive-full
   ```
   - Required for PDF exports from Org mode

## Required Emacs Packages

For the best experience with Org mode:
- ob-mermaid (for Mermaid diagrams)
- org-babel language support

You can install these using M-x package-install or by adding them to your `.emacs` file:

```elisp
(use-package ob-mermaid
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))
```

## All Required Tools Are Already Installed

Your system is well-prepared for the conference. All core tools needed for SquiggleConf 2025 sessions are already installed and up-to-date:

- Node.js and npm for JavaScript/TypeScript development
- Deno for modern JavaScript/TypeScript runtime
- Rust with wasm-pack for WebAssembly demos
- Go for backend and TS-to-Go examples
- Emacs for Org mode editing

The only recommended addition would be the Mermaid CLI if you plan to render the diagrams in the Org files.