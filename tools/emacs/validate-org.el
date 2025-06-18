;; Simple validation for Org files
;; This just checks if files can be loaded as Org mode files

(require 'org)

(defun validate-org-files ()
  "Validate all Org files in the current directory and subdirectories."
  (let ((files (directory-files-recursively "." "\\.org$"))
        (valid t))
    (dolist (file files)
      (message "Checking %s..." file)
      (condition-case err
          (with-current-buffer (find-file file)
            (if (eq major-mode 'org-mode)
                (message "Valid Org file: %s" file)
              (message "WARNING: File not in org-mode: %s" file)
              (setq valid nil)))
        (error
         (message "ERROR: Cannot process file %s: %s" file (error-message-string err))
         (setq valid nil))))
    (if valid
        (message "All Org files are valid")
      (message "Some Org files had issues")
      (kill-emacs 1))))

;; Run the validation
(validate-org-files)