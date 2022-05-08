(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;;;
;;; Straight.el packet manager
;;;
(defvar bootstrap-version)
(let ((bootstrap-file
        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
      (url-retrieve-synchronously
        "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
        'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(add-to-list 'load-path "~/.emacs.d/ox-extra/") ; provides support for :ignore: tags for latex export

(use-package evil
  :straight (evil)
  :config
    (evil-mode 1)

    (use-package evil-surround
      :straight (evil-surround :type git :host github :repo "emacs-evil/evil-surround")
      :ensure t
      :config (global-evil-surround-mode))

    (use-package evil-org
      :straight (evil-org-mode :fork (:type git :host github :repo "Somelauw/evil-org-mode"))
      :after org
      :hook (org-mode . (lambda () (evil-org-mode 1)))
      :config
      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))
    )

    (use-package evil-easymotion
      :straight (evil-easymotion :type git :host github :repo "PythonNut/evil-easymotion")
      :config
      (evilem-default-keybindings ","))
    (define-key evil-insert-state-map (kbd "j") 'escape-if-next-char-is-j) ;; Ensure that jj leaves insert mode but don't lag after typing the first j

    ;(use-package evil-indent-textobject
      ;:ensure t)

    ;;; ESC really quits
    ;;; From here: https://stackoverflow.com/questions/8483182/evil-mode-best-practice#10166400
    (defun minibuffer-keyboard-quit ()
      "Abort recursive edit.
      In Delete Selection mode, if the mark is active, just deactivate it;
      then it takes a second \\[keyboard-quit] to abort the minibuffer."
      (interactive)
      (if (and delete-selection-mode transient-mark-mode mark-active)
          (setq deactivate-mark  t)
        (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
        (abort-recursive-edit)))

    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
    (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  :custom
    (evil-want-C-i-jump nil) ; Without this, tab doesn't work ...
)

;;______________________________________________________________________
;;;;  Installing Org with straight.el
;;; https://github.com/raxod502/straight.el/blob/develop/README.md#installing-org-with-straightel
(require 'subr-x)
(straight-use-package 'git)

(defun org-git-version ()
  "The Git version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (git-run "describe"
              "--match=release\*"
              "--abbrev=6"
              "HEAD"))))

(defun org-release ()
  "The release version of 'org-mode'.
Inserted by installing 'org-mode' or when a release is made."
  (require 'git)
  (let ((git-repo (expand-file-name
                   "straight/repos/org/" user-emacs-directory)))
    (string-trim
     (string-remove-prefix
      "release_"
      (git-run "describe"
               "--match=release\*"
               "--abbrev=0"
               "HEAD")))))

(provide 'org-version)

;; (straight-use-package 'org) ; or org-plus-contrib if desired

(use-package org
  :custom
  (org-directory "~/Documents/Notizen/")

  (org-alphabetical-lists t)
  ;;(setq org-confirm-babel-evaluate nil) ; Otherwise, you need to set #+PROPERTY: header-args :eval never-export in the beginning or your document
  (org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}")
  (org-babel-shell-names '("sh" "bash" "csh" "ash" "dash" "ksh" "mksh" "posh" "zsh"))
  (org-cycle-separator-lines 2) ;; hides empty lines between headlines when looking at an outline
  (org-fold-catch-invisible-edits 'show-and-error) ;; An invisible area is hidden as "...", but can still be edited. This setting
                                                   ;; throws an error and shows the hidden area.
  (org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
  (org-hide-leading-stars t)
  (org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
  (org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
  (org-src-tab-acts-natively t) ;; you want this to have completion in blocks
  (org-use-sub-superscripts "{}")

  :hook (org-babel-after-execute . (lambda() (org-display-inline-images))) ; Show generated figures inline
  :hook (org-mode . (lambda () (org-display-inline-images 1)))
  :hook (org-mode . (lambda () (org-babel-result-hide-all)))
  :hook (org-mode . (lambda () (delete '("\\.pdf\\'" . default) org-file-apps) (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s")))) ;; Open PDFs in evince
  :hook (org-mode . (lambda () (auto-fill-mode 1) (set-fill-column 100))) ;; Line break after 100 characters for org-mode files

  :config
  (use-package org-download
    :straight (org-download)
    :config
    (setq org-download-method 'attach))


  (use-package org-bullets
      :straight (org-bullets)
      :hook (org-mode . (lambda () (org-bullets-mode 1))))

  (use-package org-table-sticky-header
      :straight (org-table-sticky-header :type git :host github :repo "cute-jumper/org-table-sticky-header" :depth 1)
      :hook (org-mode . (lambda () (org-table-sticky-header-mode 1))))


  ;; http://stackoverflow.com/questions/9005843/interactively-enter-headline-under-which-to-place-an-entry-using-capture
  (defun Shurakai/find-journal-tree ()
    (defconst Shurakai/journal-headline-format "[%Y-%m-%d %a]")
    (setq headline (format-time-string Shurakai/journal-headline-format))
    (goto-char (point-min)) ;; Go to the very beginning of the file
    (re-search-forward (format-time-string "\* %Y")) ;; Find the heading with the right year
    (outline-show-children) ;; and open it (otherwise, the next search doesn't find anything?)
    (if (re-search-forward  ;; see https://www.gnu.org/software/emacs/manual/html_node/eintr/re_002dsearch_002dforward.html
         (format org-complex-heading-regexp-format (regexp-quote headline)) 
         nil t)
       (goto-char (point-at-bol)) ;; then
       (goto-char (point-max))    ;; else
       (insert "** " headline))   ;; also else
    (org-end-of-subtree))

   (add-to-list 'org-structure-template-alist '("m" . "src emacs-lisp\n"))
   (add-to-list 'org-structure-template-alist '("r" . "src R :results output :session *R* :exports both\n"))
   (add-to-list 'org-structure-template-alist '("R" . "#+begin_src R :results file graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R*\n"))

   (add-to-list 'org-structure-template-alist '("p" . "src python :results output :exports both\n"))
   (add-to-list 'org-structure-template-alist '("P" . "src python :results output :session *python* :exports both\n"))

   (add-to-list 'org-structure-template-alist '("b" . "src sh :results output :exports both\n"))
   (add-to-list 'org-structure-template-alist '("B" . "src sh :session foo :results output :exports both\n"))

   ;;;
   ;;; Org Babel
   ;;;

   ;; Load languages on demand (avoid having to list them one by one here in the init file)
   ;; From here: https://emacs.stackexchange.com/questions/20577/org-babel-load-all-languages-on-demand
   ;; Before that, I used
   ;; (org-babel-do-load-languages
   ;;     'org-babel-load-languages
   ;;      '(
   ;;        (shell . t)
   ;;          <more languages here>))
   (defadvice org-babel-execute-src-block (around load-language nil activate)
      "Load language only when needed"
      (let ((language (org-element-property :language (org-element-at-point))))
        (unless (cdr (assoc (intern language) org-babel-load-languages))
          (add-to-list 'org-babel-load-languages (cons (intern language) t))
          (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))
        ad-do-it))

   (setq org-src-preserve-indentation t) ;; When tangled, the source code will retain its indentation; may be important for white-space sensitive code

   ;;;
   ;;; Org Capture
   ;;;
   (setq org-capture-templates
         (quote (
                 ;("j" "Berufl. Journal" plain (file+function "~/workspace/inria/research-collab/journal.org" Shurakai/find-journal-tree)
                 ; "*** %? %^g" )
                 ("p" "Priv. Journal" plain (file+function "~/org/journal_privat.org" Shurakai/find-journal-tree)
                  "*** %? %^g" )
                 ("f" "Franz. Vokabel" plain (file "~/Documents/Notizen/Franzoesisch/Vokabeln.org")
                  "** \\nbsp{} :KARTEIKARTE:FRANZ:
   :PROPERTIES:
   :DRILL_CARD_TYPE: twosided
   :END:
   \*** Deutsch
       %^{prompt}
   \*** Französisch
       %^{prompt}
   "))))

   ;;;
   ;;; Org Agenda
   ;;;
   (add-to-list 'org-modules 'org-habit) ;; Habits must be loaded this way according to the org manual: https://orgmode.org/manual/Tracking-your-habits.html - how to load it with use-package?
   (setq org-agenda-include-all-todo t)
   (setq org-agenda-include-diary t) ;; Shows entries from the Emacs diary - which I don't use...
   (setq org-agenda-ndays 7)
   (setq org-agenda-show-all-dates t)
   (setq org-agenda-skip-deadline-if-done t)
   (setq org-agenda-skip-scheduled-if-done t)
   (setq org-agenda-start-on-weekday nil)
   (setq org-agenda-start-with-follow-mode t) ;; When the curser is positioned on a line (or moved), show that entry in another window
   (setq org-agenda-files (quote ("~/Documents/Notizen/privat.org" "~/org/journal_privat.org" )))
   (setq org-deadline-warning-days 14)
   (setq org-stuck-projects '("+LEVEL=3/-DONE-CANCELLED-DEFERRED-READ-TESTED-VISITED" ("*") nil "")) ;; This needs to be adapted

   (setq org-agenda-sorting-strategy 
     (quote 
       (time-up priority-down)))

   (setq org-agenda-custom-commands '(
     ("a" "Agenda and open tasks" (
        ;(stuck "" ( (org-agenda-overriding-header "Bescheuerte Projekte")))
        (tags-todo "-KOCHREZEPT-BUCH/!TODO|STARTED|WAITING") ; Show everything with one of the listed TODO states that does not have one of the tags
        (agenda "") ;; Agenda auch anzeigen
     ))
     ("k" "Kochrezepte die noch nie gekocht wurden" (
        (tags-todo "+KOCHREZEPT/!TODO")
     ))
   ))

   ;;;
   ;;; LaTeX
   ;;;
   (unless (boundp 'org-latex-classes) (setq org-latex-classes nil))
   (add-to-list 'org-latex-packages-alist '("" "booktabs")) ;; Make booktabs package automatically available
   (add-to-list 'org-latex-classes
                '("article" "\\documentclass{article}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}" 
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
    (add-to-list 'org-latex-classes
            '("jura-ea" "\\documentclass[12pt,a4paper,oneside,headings=small,numbers=noenddot,BCOR=12mm,DIV=calc]{scrbook}%aus dem KOMA-Script-Paket "
            ;; These are the headline levels and their corresponding LaTeX commands. Make sure they exist!
              ("\\chapter{%s}" . "\\chapter*{%s}")
              ("\\section{%s}" . "\\section*{%s}")
              ("\\subsection{%s}" . "\\subsection*{%s}")
              ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
              ("\\paragraph{%s}" . "\\paragraph*{%s}")
              ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
              ("\\abschnitt{%s}" . "\\abschnitt*{%s}")
              ("\\uabschnitt{%s}" . "\\uabschnitt*{%s}")))
    (add-to-list 'org-latex-classes
         '("thesis"
           "\\documentclass[american,%
     paper=A4,               % paper size --> A4 is default in Germany
     twoside=true,           % onesite or twoside printing
     openright,              % doublepage cleaning ends up right side
     parskip=full,           % spacing value / method for paragraphs
     chapterprefix=true,     % prefix for chapter marks
     11pt,                   % font size
     headings=normal,        % size of headings
     bibliography=totoc,     % include bib in toc
     listof=totoc,           % include listof entries in toc
     titlepage=on,           % own page for each title page
     captions=tableabove,    % display table captions above the float env
     draft=false]{scrreprt}            % value for draft version "
            ("\\chapter{%s}" . "\\chapter{%s}")
            ("\\section{%s}" . "\\section{%s}")
            ("\\subsection{%s}" . "\\subsection{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{$blacktriangleright$ %s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

   (setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f ; bibtex `basename %f | sed 's/\.tex//'` ; pdflatex -interaction nonstopmode -output-directory  %o %f ; pdflatex -interaction nonstopmode -output-directory %o %f"))
   (setq org-latex-table-caption-above nil)

   (use-package ox-koma-letter)
   (use-package ox-extra
     :config
      (ox-extras-activate '(ignore-headlines))
      (defun headline-numbering-filter (data backend info)
      "No numbering in headlines that have a property :numbers: no"
      (let* ((beg (next-property-change 0 data))
             (headline (if beg (get-text-property beg :parent data))))
        (if (and (eq backend 'latex)
             (string= (org-element-property :NUMBERS headline) "no"))
            (replace-regexp-in-string
             "\\(part\\|chapter\\|\\(?:sub\\)*section\\|\\(?:sub\\)?paragraph\\)"
             "\\1*" data nil nil 1)
          data)))
      (setq org-export-filter-headline-functions '(headline-numbering-filter)))

   ;;;
   ;;; Org Cliplink
   ;;;
   (use-package org-cliplink
     :straight (org-cliplink))

   ;;;
   ;;; Org Drill (Flashcards)
   ;;;
   (use-package org-drill
     :straight (org-drill :type git :host gitlab :repo "phillord/org-drill")
     :config
       (defun custom/org-drill-tag(tag)
         "Start org-drill with a user chosen question tag."
         (interactive "sInput the tag to drill: ")
         (custom-set-variables '(org-drill-question-tag tag))
         (org-drill)
         (custom-set-variables '(org-drill-question-tag "KARTEIKARTE")))

     :custom
      (org-drill-question-tag "KARTEIKARTE")
      (org-drill-add-random-noise-to-intervals-p t)
      (org-drill-save-buffers-after-drill-sessions-p nil)
      (org-drill-sm5-initial-interval 0.25)
      (org-drill-learn-fraction 0.2)
   )

   ;;;
   ;;; Org noter
   ;;;
   ;(use-package org-noter
     ;:straight (org-noter)
     ;:config
       ;(use-package pdf-tools
         ;:straight (pdf-tools)
         ;:hook (pdf-annot-activate-handler-functions . (lambda() (org-noter-pdftools-jump-to-note)))
         ;:config 
           ;(pdf-tools-install))
       ;(use-package org-noter-pdftools
         ;:straight (org-noter-pdftools)))


     ;;;
     ;;; Key bindings for org
     ;;;
     :bind ( ("\C-cl" . 'org-store-link)
             ("\C-cc" . 'org-capture)
             ("\C-ca" . 'org-agenda)
             ("\C-cb" . 'org-iswitchb))
)

(use-package paren
    :config
    (show-paren-mode 1)
    (transient-mark-mode t)
)

(use-package recentf
  :config
  (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key "\C-x\ \C-r" 'recentf-open-files))


(use-package typopunct
  :straight (typopunct :type git :host github :repo "emacsmirror/typopunct" :depth 1)

  :hook (org-mode . (lambda() (typopunct-mode 1))))


;;;
;;; General Emacs setup
;;;

(load-theme 'tango-dark t)

(column-number-mode 1)
(line-number-mode 1)
(tool-bar-mode -1) ; Disable toolbar
;;;(scroll-bar-mode 0)

(setq frame-title-format '("Emacs - " (buffer-file-name "%f" (dired-directory dired-directory "%b")))) ;; Overwrite default 'Emacs@hostname'
(setq inhibit-splash-screen t) ;; Suppress the initial splash screen so that my default file is shown
(global-set-key (kbd "C-x x") 'execute-extended-command)
(find-file "~/org/journal_privat.org")

;(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(global-set-key (kbd "C-c i") (lambda() (interactive)(org-babel-load-file "~/.emacs.d/init.org")))
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

; Escape on jj
(defun escape-if-next-char (c)
  "Watches the next letter.  If c, then switch to Evil's normal mode; otherwise insert a k and forward unpressed key to unread-command events"
  (self-insert-command 1)
  (let ((next-key (read-event)))
    (if (= c next-key)
      (progn
        (delete-backward-char 1)
        (evil-esc 1))
      (setq unread-command-events (list next-key)))))

(defun escape-if-next-char-is-j (arg)
  (interactive "p")
  (if (= arg 1)
    (escape-if-next-char ?j)
    (self-insert-command arg)))

