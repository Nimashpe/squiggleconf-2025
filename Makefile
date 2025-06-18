.PHONY: all lint-basic lint tangle export clean help

# Default target
all: lint-basic tangle

# Basic lint without checking languages
lint-basic:
	@echo "Basic linting of Org files (ignoring language warnings)..."
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(setq org-lint-warning-face 'font-lock-warning-face)" \
		--eval "(defun my-org-lint-filter (issues) (cl-remove-if (lambda (i) (eq (nth 2 i) 'suspicious-language-in-src-block)) issues))" \
		--eval "(advice-add 'org-lint--collect-issues :filter-return #'my-org-lint-filter)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Linting %s...\" file) \
				(let ((issues (org-lint))) \
					(if issues \
						(progn \
							(message \"Issues found in %s:\" file) \
							(dolist (issue issues) \
								(message \"  %s\" issue)) \
							(kill-emacs 1)) \
						(message \"No issues found in %s\" file)))))"

# Full lint (with language checks)
lint:
	@echo "Linting Org files (with language checks)..."
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Linting %s...\" file) \
				(let ((issues (org-lint))) \
					(if issues \
						(progn \
							(message \"Issues found in %s:\" file) \
							(dolist (issue issues) \
								(message \"  %s\" issue)) \
							(kill-emacs 1)) \
						(message \"No issues found in %s\" file)))))"

# Tangle org files
tangle:
	@echo "Tangling Org files..."
	@emacs --batch \
		--eval "(require 'org)" \
		--eval "(dolist (file (directory-files-recursively \".\" \"\\.org$$\")) \
			(with-current-buffer (find-file file) \
				(message \"Tangling %s...\" file) \
				(org-babel-tangle)))"

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
	@find . -name "*.md" -delete
	@find . -name "*.tex" -delete

# Help message
help:
	@echo "SquiggleConf 2025 Notes Makefile"
	@echo ""
	@echo "Available targets:"
	@echo "  all        - Run basic lint and tangle (default)"
	@echo "  lint-basic - Check Org files for issues (ignoring language warnings)"
	@echo "  lint       - Full lint including language checks"
	@echo "  tangle     - Extract code blocks from Org files"
	@echo "  export-html - Export Org files to HTML"
	@echo "  export-pdf  - Export Org files to PDF"
	@echo "  export-md   - Export Org files to Markdown"
	@echo "  export     - Export Org files to all formats"
	@echo "  clean      - Remove generated files"
	@echo "  help       - Show this help message"