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
    (dot . t)
    (racket . t)
    (zig . t)
    (ocaml . t)
    (scheme . t)
    (fennel . t)
    (janet . t)
    (hy . t))
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
  (add-to-list 'org-src-lang-modes '("typescript" . typescript))
  (add-to-list 'org-src-lang-modes '("racket" . racket))
  (add-to-list 'org-src-lang-modes '("zig" . zig))
  (add-to-list 'org-src-lang-modes '("ocaml" . tuareg))
  (add-to-list 'org-src-lang-modes '("scheme" . scheme))
  (add-to-list 'org-src-lang-modes '("fennel" . fennel))
  (add-to-list 'org-src-lang-modes '("janet" . janet))
  (add-to-list 'org-src-lang-modes '("hy" . hy)))

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
    :ensure t)
    
  ;; For Racket
  (use-package racket-mode
    :ensure t)
    
  ;; For Zig
  (use-package zig-mode
    :ensure t)
    
  ;; For OCaml
  (use-package tuareg
    :ensure t)
    
  ;; For Scheme/Guile
  (use-package geiser
    :ensure t)
    
  ;; For Fennel
  (use-package fennel-mode
    :ensure t)
    
  ;; For Janet
  (use-package janet-mode
    :ensure t)
    
  ;; For Hy
  (use-package hy-mode
    :ensure t))

;; Provide basic compatibility even without the packages
(unless (featurep 'ob-mermaid)
  (defun org-babel-execute:mermaid (body params)
    "Placeholder for mermaid execution if ob-mermaid not available.
Saves the diagram source to a file and returns a message."
    (let* ((file (cdr (assq :file params)))
           (out-file (or file "diagram.mmd")))
      ;; Write the mermaid source to a file for later rendering
      (when file
        (with-temp-file file
          (insert body)))
      (format "Mermaid diagram saved to %s for later rendering.\nMermaid CLI not available in current environment.\nDiagram will be rendered on systems with mermaid-cli installed." 
              out-file))))

;; Provide fallbacks for other languages if not available
(unless (featurep 'ob-racket)
  (defun org-babel-execute:racket (body params)
    "Placeholder for racket execution if racket support not available."
    (format "Racket code (%d chars) - will execute on systems with Racket support." 
            (length body))))

(unless (featurep 'ob-zig)
  (defun org-babel-execute:zig (body params)
    "Placeholder for zig execution if zig support not available."
    (format "Zig code (%d chars) - will execute on systems with Zig support." 
            (length body))))

(unless (featurep 'ob-ocaml)
  (defun org-babel-execute:ocaml (body params)
    "Placeholder for ocaml execution if ocaml support not available."
    (format "OCaml code (%d chars) - will execute on systems with OCaml support." 
            (length body))))

(unless (featurep 'ob-fennel)
  (defun org-babel-execute:fennel (body params)
    "Placeholder for fennel execution if fennel support not available."
    (format "Fennel code (%d chars) - will execute on systems with Fennel support." 
            (length body))))

(unless (featurep 'ob-janet)
  (defun org-babel-execute:janet (body params)
    "Placeholder for janet execution if janet support not available."
    (format "Janet code (%d chars) - will execute on systems with Janet support." 
            (length body))))

(unless (featurep 'ob-hy)
  (defun org-babel-execute:hy (body params)
    "Placeholder for hy execution if hy support not available."
    (format "Hy code (%d chars) - will execute on systems with Hy support." 
            (length body))))

;; Initialize the setup
(squiggleconf-setup-babel-languages)

;; Provide the feature
(provide 'squiggleconf-babel)