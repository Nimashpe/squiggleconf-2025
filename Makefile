# SquiggleConf 2025 Notes Makefile

.PHONY: all validate-org lint-basic tangle export-html export-pdf export-md export clean help

# Default target: validate and tangle org files
all: validate-org tangle  ## Run validation and tangle (default)

# Validate org files (simple check for validity)
validate-org:  ## Simple check that Org files can be loaded
	@echo "Validating Org files..."
	@emacs --batch --load tools/emacs/validate-org.el

# Basic lint without checking languages
lint-basic:  ## Check Org files for issues (ignoring language warnings)
	@echo "Basic linting of Org files (ignoring language warnings)..."
	@emacs --batch --load tools/emacs/lint-org.el

# Tangle org files to extract code
tangle:  ## Extract code blocks from Org files
	@echo "Tangling Org files..."
	@emacs --batch --load tools/emacs/tangle-org.el

# Export org files to HTML
export-html:  ## Export Org files to HTML
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
export-pdf:  ## Export Org files to PDF
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
export-md:  ## Export Org files to Markdown
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
export: export-html export-pdf export-md  ## Export Org files to all formats

# Clean generated files
clean:  ## Remove generated files
	@echo "Cleaning generated files..."
	@rm -rf exports
	@find . -name "*.html" -delete
	@find . -name "*.pdf" -delete
	@find . -name "*.tex" -delete

# Dynamic help message that reads target comments
help:  ## Show this help message
	@echo "SquiggleConf 2025 Notes Makefile"
	@echo ""
	@echo "Usage: gmake [target]"
	@echo ""
	@echo "Targets:"
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  %-12s %s\n", $$1, $$2}'