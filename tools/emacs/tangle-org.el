;; Tangle all Org files
;; This extracts code blocks from org files

(require 'org)

(defun tangle-org-files ()
  "Tangle all Org files in the current directory and subdirectories."
  (let ((files (directory-files-recursively "." "\\.org$")))
    (dolist (file files)
      (message "Tangling %s..." file)
      (condition-case err
          (with-current-buffer (find-file file)
            (org-babel-tangle))
        (error
         (message "ERROR: Cannot tangle file %s: %s" file (error-message-string err))
         (kill-emacs 1))))
    (message "All Org files tangled successfully")))

;; Run the tangler
(tangle-org-files)