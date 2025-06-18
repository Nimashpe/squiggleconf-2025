;; SquiggleConf 2025 Babel setup for Org mode
;; This file provides minimal setup for Org Babel language support

;; Load org-mode if not already loaded
(require 'org)

;; Define languages for code blocks
(defvar squiggleconf-babel-languages
  '((js . t)
    (python . t)
    (shell . t)
    (mermaid . t)
    (json . t)
    (rust . t)
    (typescript . t)
    (dot . t))
  "Languages to be supported for SquiggleConf 2025 notes.")

;; Configure Babel languages
(defun squiggleconf-setup-babel-languages ()
  "Set up Babel languages for SquiggleConf."
  (org-babel-do-load-languages
   'org-babel-load-languages
   squiggleconf-babel-languages))

;; Add specific language mode associations
(with-eval-after-load 'org
  (add-to-list 'org-src-lang-modes '("json" . json))
  (add-to-list 'org-src-lang-modes '("mermaid" . fundamental))
  (add-to-list 'org-src-lang-modes '("typescript" . typescript)))

;; Ensure packages are installed if use-package is available
(when (require 'use-package nil t)
  ;; For mermaid diagrams
  (use-package ob-mermaid
    :ensure t
    :config
    (setq ob-mermaid-cli-path "/usr/local/bin/mmdc"))

  ;; For TypeScript
  (use-package typescript-mode
    :ensure t)

  ;; For JSON
  (use-package json-mode
    :ensure t))

;; Provide basic compatibility even without the packages
(unless (featurep 'ob-mermaid)
  (defun org-babel-execute:mermaid (body params)
    "Placeholder for mermaid execution if ob-mermaid not available.
Returns a message with BODY length and PARAMS keys."
    (format "Mermaid diagram (%d chars, %d params)"
            (length body)
            (length params))))

;; Initialize the setup
(squiggleconf-setup-babel-languages)

;; Provide the feature
(provide 'squiggleconf-babel)