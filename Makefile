# SquiggleConf 2025 Notes Makefile - FreeBSD

.PHONY: all validate-org lint-basic tangle export-html export-pdf export-md export clean help

# Default target
all: validate-org tangle

# Validate org files
validate-org:
	@echo "Validating Org files..."
	@emacs --batch --load tools/emacs/validate-org.el

# Basic lint without checking languages
lint-basic:
	@echo "Basic linting of Org files (ignoring language warnings)..."
	@emacs --batch --load tools/emacs/lint-org.el

# Tangle org files
tangle:
	@echo "Tangling Org files..."
	@emacs --batch --load tools/emacs/tangle-org.el

# Export org files to HTML
export-html:
	@echo "Exporting Org files to HTML..."
	@mkdir -p exports/html
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Exporting %s to HTML...\" file) \
				(org-html-export-to-html)))"
	@find . -name "*.html" -exec mv {} exports/html/ \;

# Export org files to PDF
export-pdf:
	@echo "Exporting Org files to PDF..."
	@mkdir -p exports/pdf
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Exporting %s to PDF...\" file) \
				(org-latex-export-to-pdf)))"
	@find . -name "*.pdf" -exec mv {} exports/pdf/ \;

# Export org files to Markdown
export-md:
	@echo "Exporting Org files to Markdown..."
	@mkdir -p exports/markdown
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Exporting %s to Markdown...\" file) \
				(org-md-export-to-markdown)))"
	@find . -name "*.md" -exec mv {} exports/markdown/ \;

# Export all formats
export: export-html export-pdf export-md

# Clean generated files
clean:
	@echo "Cleaning generated files..."
	@rm -rf exports
	@find . -name "*.html" -delete
	@find . -name "*.pdf" -delete
	@find . -name "*.tex" -delete

# Simple help message
help:
	@echo "SquiggleConf 2025 Notes Makefile"
	@echo ""
	@echo "Usage: gmake [target]"
	@echo ""
	@echo "Targets:"
	@echo "  all          - Run validation and tangle (default)"
	@echo "  validate-org - Simple check that Org files can be loaded"
	@echo "  lint-basic   - Check Org files for issues (ignoring language warnings)"
	@echo "  tangle       - Extract code blocks from Org files"
	@echo "  export-html  - Export Org files to HTML"
	@echo "  export-pdf   - Export Org files to PDF"
	@echo "  export-md    - Export Org files to Markdown"
	@echo "  export       - Export Org files to all formats"
	@echo "  clean        - Remove generated files"
	@echo "  help         - Show this help message"