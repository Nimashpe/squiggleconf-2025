;; Linter for Org files
;; This checks org files for common issues

(require 'org)
(require 'org-lint)

;; Filter out language warnings
(defun my-org-lint-filter (issues)
  "Filter out certain types of issues like language warnings."
  (cl-remove-if (lambda (i) (eq (nth 2 i) 'suspicious-language-in-src-block)) issues))

;; Apply the filter
(advice-add 'org-lint--collect-issues :filter-return #'my-org-lint-filter)

(defun lint-org-files ()
  "Lint all Org files in the current directory and subdirectories."
  (let ((files (directory-files-recursively "." "\\.org$"))
        (issues-found nil))
    (dolist (file files)
      (message "Linting %s..." file)
      (condition-case err
          (with-current-buffer (find-file file)
            (let ((issues (org-lint)))
              (if issues
                  (progn
                    (message "Issues found in %s: %d issues (filtered out language warnings)" 
                             file (length issues))
                    (setq issues-found t))
                (message "No issues found in %s" file))))
        (error
         (message "ERROR: Cannot process file %s: %s" file (error-message-string err))
         (setq issues-found t))))
    (if issues-found
        (progn
          (message "Some Org files had issues (non-fatal)")
          (message "These issues are informational only and won't prevent tangling"))
      (message "All Org files are clean"))))

;; Run the linter
(lint-org-files)