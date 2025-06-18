;;; squiggleconf-2025.el --- Complete Emacs configuration for SquiggleConf 2025 -*- lexical-binding: t; -*-

;; Author: Your Name
;; Created: 2025
;; Description: Comprehensive Emacs setup for tracking SquiggleConf 2025 sessions,
;;              with org-mode, Babel, capture templates, journaling, and tooling support.

;;; Commentary:
;; This configuration provides everything needed for effective note-taking at
;; SquiggleConf 2025, including session tracking, speaker notes, code execution,
;; diagram generation, and more.

;;; Code:

;;;; Package Setup
(require 'org)
(require 'org-capture)
(require 'org-agenda)
(require 'ob-python)
(require 'ob-shell)
(require 'ob-js)

;;;; Conference Variables
(defgroup squiggleconf nil
  "SquiggleConf 2025 conference note-taking configuration."
  :group 'org)

(defcustom squiggleconf-directory "~/squiggleconf-2025/"
  "Root directory for SquiggleConf notes."
  :type 'directory
  :group 'squiggleconf)

(defcustom squiggleconf-speakers
  '("Anthony Fu" "Shelley Vohr" "Oliver Dunk" "Pete Gonzalez"
    "Amy Dutton" "Mattia Manzati" "Jake Bailey" "Alexander Lichter"
    "Giacomo Cavalieri" "Maddy Montaquila" "Jessica Garson"
    "Nicolò Ribaudo" "Dominic Nguyen" "Oliver Medhurst"
    "David Sherret" "Dylan Goings" "Michelle Bakels" "TJ DeVries")
  "List of SquiggleConf 2025 speakers."
  :type '(repeat string)
  :group 'squiggleconf)

(defcustom squiggleconf-tracks
  '("TypeScript" "Build Tools" "Language Servers" "Testing"
    "WebAssembly" "Developer Experience" "Package Management"
    "Performance" "Accessibility" "Open Source")
  "Conference tracks/topics."
  :type '(repeat string)
  :group 'squiggleconf)

;;;; Directory Structure
(defun squiggleconf-ensure-directories ()
  "Ensure all conference directories exist."
  (dolist (dir '("sessions" "speakers" "journal" "tools"
                 "diagrams" "exports" "networking" "insights"
                 "tools/scripts" "tools/demo-projects"))
    (make-directory (expand-file-name dir squiggleconf-directory) t)))

;;;; Babel Configuration
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (shell . t)
   (js . t)
   (emacs-lisp . t)
   (sql . t)
   (dot . t)
   (plantuml . t)))

;; TypeScript support via ts-node
(defun org-babel-execute:typescript (body params)
  "Execute TypeScript code block via ts-node."
  (let ((tmp-file (org-babel-temp-file "ts-" ".ts")))
    (with-temp-file tmp-file
      (insert body))
    (org-babel-eval
     (format "ts-node %s" (org-babel-process-file-name tmp-file))
     "")))

;; Go support
(defun org-babel-execute:go (body params)
  "Execute Go code block."
  (let ((tmp-file (org-babel-temp-file "go-" ".go")))
    (with-temp-file tmp-file
      (insert "package main\n\n")
      (insert body))
    (org-babel-eval
     (format "go run %s" (org-babel-process-file-name tmp-file))
     "")))

;; Rust support
(defun org-babel-execute:rust (body params)
  "Execute Rust code block."
  (let ((tmp-file (org-babel-temp-file "rust-" ".rs")))
    (with-temp-file tmp-file
      (insert body))
    (org-babel-eval
     (format "rustc %s -o /tmp/rust-out && /tmp/rust-out"
             (org-babel-process-file-name tmp-file))
     "")))

;; Mermaid support
(defcustom ob-mermaid-cli-path "mmdc"
  "Path to mermaid CLI executable."
  :type 'string
  :group 'squiggleconf)

(defun org-babel-execute:mermaid (body params)
  "Execute mermaid code block and generate diagram."
  (let* ((out-file (or (cdr (assoc :file params))
                       (error "Mermaid requires :file parameter")))
         (theme (or (cdr (assoc :theme params)) "default"))
         (cmd (format "%s -i - -o %s -t %s"
                      ob-mermaid-cli-path
                      (org-babel-process-file-name out-file)
                      theme)))
    (with-temp-buffer
      (insert body)
      (shell-command-on-region (point-min) (point-max) cmd))
    nil))

;; Don't ask for confirmation when executing code blocks
(setq org-confirm-babel-evaluate nil)

;; Set default header args for better workflow
(setq org-babel-default-header-args
      '((:results . "output")
        (:exports . "both")
        (:mkdirp . "yes")
        (:cache . "no")))

;;;; Capture Templates
(setq org-capture-templates
      `(;; Session captures
        ("s" "Session Templates")
        ("ss" "Session Notes" entry
         (file+headline ,(expand-file-name "sessions/index.org" squiggleconf-directory)
                        "Session Notes")
         "* TODO %^{Session Title}
:PROPERTIES:
:SPEAKER: %^{Speaker|%(mapconcat 'identity squiggleconf-speakers \"|\")}
:TRACK: %^{Track|%(mapconcat 'identity squiggleconf-tracks \"|\")}
:TIME: %^{Time}U
:LOCATION: Simons IMAX Theater
:END:

** Overview
%?

** Key Concepts

** Code Examples

#+begin_src %^{Language|python|typescript|go|rust|javascript}

#+end_src

** Architecture

#+begin_src mermaid :file ../diagrams/%^{Diagram name}.png
graph LR
    A[Component] --> B[Component]
#+end_src

** Key Takeaways
- 
- 
- 

** Questions for Speaker
1. 

** Resources
- Slides: 
- Repository: 
- Documentation: 

** Action Items
- [ ] 
")
        
        ("si" "Quick Session Insight" item
         (file+olp ,(expand-file-name "sessions/insights.org" squiggleconf-directory)
                   "Session Insights" "Day %^{Day|1|2}")
         "- [%<%H:%M>] *%^{Session}*: %? (%^{Speaker})")
        
        ("sc" "Session Code Snippet" entry
         (file+headline ,(expand-file-name "sessions/code-snippets.org" squiggleconf-directory)
                        "Code Snippets")
         "* %^{Title}
:PROPERTIES:
:SESSION: %^{Session}
:LANGUAGE: %^{Language|typescript|python|go|rust}
:END:

#+begin_src %\\3 :tangle ./snippets/%^{Filename}
%?
#+end_src
")

        ;; Speaker captures
        ("k" "Speaker Templates")
        ("ks" "Speaker Notes" entry
         (file+headline ,(expand-file-name "speakers/index.org" squiggleconf-directory)
                        "Speakers")
         "* %^{Speaker Name}
:PROPERTIES:
:ORGANIZATION: %^{Organization}
:SESSION: %^{Session Title}
:GITHUB: %^{GitHub Username}
:TWITTER: %^{Twitter Handle}
:END:

** Background
%?

** Notable Projects
- 

** Key Insights from Session
- 

** Follow-up
- [ ] Follow on Twitter/GitHub
- [ ] Check out projects:
  - [ ] 
")
        
        ("kq" "Question for Speaker" item
         (file+headline ,(expand-file-name "speakers/questions.org" squiggleconf-directory)
                        "Questions Queue")
         "- [ ] *%^{Speaker}*: %? (Session: %^{Session})")

        ;; Journal entries
        ("j" "Journal Templates")
        ("jd" "Daily Journal" entry
         (file+olp+datetree ,(expand-file-name "journal/conference-journal.org" squiggleconf-directory))
         "* %<%H:%M> %^{Entry Title}
:PROPERTIES:
:ENERGY: %^{Energy Level|1-10}
:LOCATION: %^{Location|Simons IMAX Theater|Networking Area|Lunch Area}
:END:

%?

** Sessions Attended
- [ ] 

** People Met
- 

** Tools to Explore
- 
")
        
        ("jm" "Meeting/Networking" entry
         (file+headline ,(expand-file-name "networking/contacts.org" squiggleconf-directory)
                        ,(format-time-string "Contacts - %Y-%m-%d"))
         "* %^{Name}
:PROPERTIES:
:ORGANIZATION: %^{Company/Project}
:ROLE: %^{Role}
:EMAIL: %^{Email}
:GITHUB: %^{GitHub}
:MET_AT: %^{Where did you meet?}
:DISCUSSED: %^{Topics discussed}
:END:

** Notes
%?

** Follow-up Actions
- [ ] Connect on LinkedIn/GitHub
- [ ] 
")

        ;; Tool tracking
        ("t" "Tooling Templates")
        ("tt" "Tool to Try" entry
         (file+headline ,(expand-file-name "tools/to-explore.org" squiggleconf-directory)
                        "Tools to Explore")
         "* TODO %^{Tool Name} %^g
:PROPERTIES:
:URL: %^{URL}
:LANGUAGE: %^{Language|TypeScript|JavaScript|Go|Rust|Multiple}
:CATEGORY: %^{Category|LSP|Bundler|Testing|Linting|Package Manager|DevTools}
:SESSION: %^{Related Session}
:SPEAKER: %^{Mentioned by}
:END:

** Description
%?

** Why Interesting
- 

** Installation
#+begin_src bash
%^{Install command}
#+end_src

** Example Usage
#+begin_src %^{Language|typescript|javascript|python}

#+end_src

** Comparison to Existing Tools
- 
")
        
        ("tp" "Tool Comparison" entry
         (file+headline ,(expand-file-name "tools/comparisons.org" squiggleconf-directory)
                        "Tool Comparisons")
         "* %^{Tool A} vs %^{Tool B}
:PROPERTIES:
:CATEGORY: %^{Category}
:DATE: %U
:END:

** Overview

| Feature | %\\1 | %\\2 |
|---------+------+------|
| %^{Feature 1} | | |
| %^{Feature 2} | | |
| %^{Feature 3} | | |

** Code Example: %\\1
#+begin_src %^{Language}

#+end_src

** Code Example: %\\2
#+begin_src %\\4

#+end_src

** Verdict
%?
")

        ;; Ideas and insights
        ("i" "Ideas & Insights")
        ("ii" "Conference Insight" entry
         (file+headline ,(expand-file-name "insights/insights.org" squiggleconf-directory)
                        "Key Insights")
         "* %^{Insight Title}
:PROPERTIES:
:RELATED_SESSION: %^{Related Session}
:IMPACT: %^{Impact|High|Medium|Low}
:ACTIONABLE: %^{Actionable?|Yes|No|Maybe}
:END:

%?

** How This Changes Things
- 

** Action Items
- [ ] 
")
        
        ("ip" "Project Idea" entry
         (file+headline ,(expand-file-name "insights/project-ideas.org" squiggleconf-directory)
                        "Project Ideas")
         "* %^{Project Name}
:PROPERTIES:
:INSPIRED_BY: %^{Session/Speaker}
:TECHNOLOGIES: %^{Technologies}
:EFFORT: %^{Effort|Weekend|Week|Month|Quarter}
:END:

** Problem Statement
%?

** Proposed Solution

** MVP Features
- [ ] 
- [ ] 
- [ ] 

** Tech Stack
- Language: 
- Framework: 
- Tools: 

** Similar Projects
- 
")))

;;;; Custom Commands and Functions

(defun squiggleconf-insert-mermaid-template ()
  "Insert a mermaid diagram template at point."
  (interactive)
  (insert "#+begin_src mermaid :file ./diagrams/"
          (read-string "Diagram filename (without extension): ")
          ".png\n"
          "graph TD\n"
          "    A[Start] --> B{Decision}\n"
          "    B -->|Yes| C[Do this]\n"
          "    B -->|No| D[Do that]\n"
          "#+end_src\n"))

(defun squiggleconf-insert-comparison-table ()
  "Insert a tool comparison table."
  (interactive)
  (let ((tool1 (read-string "Tool 1: "))
        (tool2 (read-string "Tool 2: ")))
    (insert (format "| Feature | %s | %s |\n" tool1 tool2)
            "|---------|------|------|\n"
            "| Speed | | |\n"
            "| Memory Usage | | |\n"
            "| Developer Experience | | |\n"
            "| Documentation | | |\n"
            "| Community | | |\n")))

(defun squiggleconf-tangle-session ()
  "Tangle code blocks from current session file."
  (interactive)
  (let ((session-name (file-name-base (buffer-file-name))))
    (org-babel-tangle)
    (message "Tangled code blocks for session: %s" session-name)))

(defun squiggleconf-export-session-html ()
  "Export current session to HTML with custom styling."
  (interactive)
  (let ((org-html-head-extra
         "<style>
          body { font-family: -apple-system, BlinkMacSystemFont, sans-serif; max-width: 900px; margin: 0 auto; padding: 2em; }
          h1 { color: #2563eb; border-bottom: 3px solid #2563eb; padding-bottom: 0.5em; }
          h2 { color: #1e40af; margin-top: 2em; }
          pre { background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 1em; overflow-x: auto; }
          code { background: #f1f5f9; padding: 0.2em 0.4em; border-radius: 3px; font-size: 0.9em; }
          blockquote { border-left: 4px solid #2563eb; padding-left: 1em; color: #64748b; }
          .org-src-container { margin: 1.5em 0; }
          table { border-collapse: collapse; margin: 1em 0; }
          th, td { border: 1px solid #e2e8f0; padding: 0.5em 1em; }
          th { background: #f8fafc; font-weight: 600; }
          .todo { color: #dc2626; font-weight: bold; }
          .done { color: #16a34a; font-weight: bold; }
          </style>"))
    (org-html-export-to-html)))

(defun squiggleconf-create-session-summary ()
  "Create a summary of all sessions with key insights."
  (interactive)
  (let ((summary-file (expand-file-name "conference-summary.org" squiggleconf-directory)))
    (with-temp-file summary-file
      (insert "#+TITLE: SquiggleConf 2025 Summary\n"
              "#+DATE: " (format-time-string "<%Y-%m-%d %a>") "\n\n"
              "* Sessions Summary\n\n")
      (dolist (file (directory-files 
                     (expand-file-name "sessions" squiggleconf-directory)
                     t "\\.org$"))
        (when (file-exists-p file)
          (insert "** " (file-name-base file) "\n")
          (insert "[[file:" file "][Full Notes]]\n\n"))))
    (find-file summary-file)))

(defun squiggleconf-search-notes (query)
  "Search all conference notes for QUERY."
  (interactive "sSearch conference notes for: ")
  (let ((org-agenda-files (directory-files-recursively 
                          squiggleconf-directory 
                          "\\.org$")))
    (org-search-view nil query)))

(defun squiggleconf-daily-review ()
  "Create daily review entry with session summaries."
  (interactive)
  (org-capture nil "jd")
  (insert "\n** Sessions Summary\n")
  (insert "*** Morning\n- \n\n")
  (insert "*** Afternoon\n- \n\n")
  (insert "** Top 3 Insights\n1. \n2. \n3. \n\n")
  (insert "** Tomorrow's Plan\n- [ ] "))

;;;; Agenda Configuration
(defun squiggleconf-agenda-files ()
  "Get all agenda files for the conference."
  (directory-files-recursively squiggleconf-directory "\\.org$"))

(defun squiggleconf-agenda ()
  "Show conference-specific agenda."
  (interactive)
  (let ((org-agenda-files (squiggleconf-agenda-files)))
    (org-agenda nil "n")))

;; Custom agenda views
(setq org-agenda-custom-commands
      '(("c" "Conference View"
         ((tags-todo "SESSION"
                     ((org-agenda-overriding-header "📹 Sessions to Watch")))
          (tags-todo "TOOL"
                     ((org-agenda-overriding-header "🔧 Tools to Explore")))
          (tags-todo "SPEAKER"
                     ((org-agenda-overriding-header "👤 Speaker Follow-ups")))
          (tags-todo "PROJECT"
                     ((org-agenda-overriding-header "💡 Project Ideas")))))
        ("r" "Conference Review"
         ((agenda "" ((org-agenda-span 2)
                     (org-agenda-start-day "2025-09-18")
                     (org-agenda-overriding-header "Conference Schedule")))
          (alltodo "" ((org-agenda-overriding-header "All Action Items")))))))

;;;; Export Configuration
(setq org-export-with-author nil
      org-export-with-email nil
      org-export-with-toc t
      org-export-with-section-numbers nil
      org-html-validation-link nil
      org-export-with-sub-superscripts '{})

;;;; Keybindings
(defvar squiggleconf-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Capture
    (define-key map (kbd "C-c s c") 'org-capture)
    ;; Insert templates
    (define-key map (kbd "C-c s m") 'squiggleconf-insert-mermaid-template)
    (define-key map (kbd "C-c s t") 'squiggleconf-insert-comparison-table)
    ;; Export and tangle
    (define-key map (kbd "C-c s T") 'squiggleconf-tangle-session)
    (define-key map (kbd "C-c s e") 'squiggleconf-export-session-html)
    ;; Navigation
    (define-key map (kbd "C-c s a") 'squiggleconf-agenda)
    (define-key map (kbd "C-c s s") 'squiggleconf-search-notes)
    (define-key map (kbd "C-c s r") 'squiggleconf-daily-review)
    (define-key map (kbd "C-c s S") 'squiggleconf-create-session-summary)
    map)
  "Keymap for SquiggleConf mode.")

;;;; Minor Mode
(define-minor-mode squiggleconf-mode
  "Minor mode for SquiggleConf 2025 note-taking."
  :lighter " SqConf"
  :keymap squiggleconf-mode-map
  (when squiggleconf-mode
    (squiggleconf-ensure-directories)
    (setq-local org-directory squiggleconf-directory)
    (setq-local org-default-notes-file 
                (expand-file-name "inbox.org" squiggleconf-directory))))

;;;; Auto-enable for conference files
(defun squiggleconf-maybe-enable ()
  "Enable squiggleconf-mode if in conference directory."
  (when (and buffer-file-name
             (string-prefix-p (expand-file-name squiggleconf-directory)
                             buffer-file-name))
    (squiggleconf-mode 1)))

(add-hook 'org-mode-hook #'squiggleconf-maybe-enable)

;;;; Hydra for quick access (optional, requires hydra package)
(with-eval-after-load 'hydra
  (defhydra hydra-squiggleconf (:color blue :hint nil)
    "
SquiggleConf 2025 Actions
------------------------
_c_: Capture          _a_: Agenda         _T_: Tangle session
_s_: Search notes     _e_: Export HTML    _r_: Daily review
_m_: Insert mermaid   _t_: Comparison     _S_: Summary
_q_: Quit
"
    ("c" org-capture)
    ("a" squiggleconf-agenda)
    ("s" squiggleconf-search-notes)
    ("e" squiggleconf-export-session-html)
    ("m" squiggleconf-insert-mermaid-template)
    ("t" squiggleconf-insert-comparison-table)
    ("T" squiggleconf-tangle-session)
    ("r" squiggleconf-daily-review)
    ("S" squiggleconf-create-session-summary)
    ("q" nil))
  
  (global-set-key (kbd "C-c C-s") 'hydra-squiggleconf/body))

;;;; Initialize
(defun squiggleconf-initialize ()
  "Initialize SquiggleConf environment."
  (interactive)
  (squiggleconf-ensure-directories)
  (let ((readme (expand-file-name "README.org" squiggleconf-directory)))
    (unless (file-exists-p readme)
      (with-temp-file readme
        (insert "#+TITLE: SquiggleConf 2025 Notes\n"
                "#+DATE: September 18-19, 2025\n"
                "#+LOCATION: New England Aquarium, Boston\n\n"
                "* Welcome to SquiggleConf 2025! 🌊\n\n"
                "** Quick Start\n"
                "- =C-c s c= : Capture (session, speaker, tool, etc.)\n"
                "- =C-c s a= : Show agenda\n"
                "- =C-c s s= : Search all notes\n"
                "- =C-c C-s= : Hydra menu (if available)\n\n"
                "** Conference Info\n"
                "- [[file:sessions/][Sessions]]\n"
                "- [[file:speakers/][Speakers]]\n"
                "- [[file:tools/][Tools & Demos]]\n"
                "- [[file:journal/][Conference Journal]]\n")))
    (find-file readme))
  (message "SquiggleConf 2025 environment initialized!"))

;; Auto-initialize on first load
(unless (file-exists-p squiggleconf-directory)
  (when (y-or-n-p "Initialize SquiggleConf 2025 directory structure? ")
    (squiggleconf-initialize)))

(provide 'squiggleconf-2025)
;;; squiggleconf-2025.el ends here
