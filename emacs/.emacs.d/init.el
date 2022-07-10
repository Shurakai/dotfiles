;(setq debug-on-error t
      ;debug-on-signal nil
      ;debug-on-quit nil)

; cheinrich: I started this configuration a long time ago, but recently also discovered https://github.com/yantar92/emacs-config/blob/master/config.org
; which is a huge config file. It is mostly not for me - but maybe you may want to check it out.

(defvar my/init-el-start-time (current-time) "For benchmarking my init.el: Time when init.el was started")

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
(straight-use-package 'org)

(add-to-list 'load-path "~/.emacs.d/ox-extra/") ; provides support for :ignore: tags for latex export

(use-package evil
  :straight (evil)
  :config
    (evil-mode 1)

    (use-package evil-surround
      :straight (evil-surround :type git :host github :repo "emacs-evil/evil-surround")
      ;:ensure t
      :config (global-evil-surround-mode))

	;; Org bindings for evil
    ;; It's set up in "themes" that can be combined; see 
    ;; https://github.com/Somelauw/evil-org-mode/blob/master/doc/keythemes.org for a list
    ;; Nice features are M-gh to go to the top heading, M-h/j/k/l on a heading to demote/promote it (and move it up/down...)
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

    ;; Required until emacs 28; once upgraded to emacs 28, check evil-undo-system and set that value to 'undo-redo
    (use-package undo-tree
      :config
      (global-undo-tree-mode t))

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
    (evil-undo-system 'undo-tree)
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
  ;(org-babel-exp-inline-code-template "src_%lang[%switches%flags]{%body}")
  (org-babel-shell-names '("sh" "bash" "csh" "ash" "dash" "ksh" "mksh" "posh" "zsh"))
  (org-babel-python-command "python3")
  (org-cycle-separator-lines 2) ;; >= n empty (!) lines are needed to show them after the end of a subtree; otherwise, < n empty lines are ignored; when 0, then empty lines are never shown
  (org-fold-catch-invisible-edits 'show-and-error) ;; An invisible area is hidden as "...", but can still be edited. This setting
                                                   ;; throws an error and shows the hidden area.
  (org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
  (org-hide-leading-stars t)
  (org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html
  (org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
  (org-src-tab-acts-natively t) ;; you want this to have completion in blocks
  (org-use-sub-superscripts "{}")


  (org-log-into-drawer t) ;; Log into a drawer called "LOGBOOK" (so that we can fold it away)
  ;; This redefines the log messages; note that this does not /activate/ the messages by itself, that has to be done e.g. by setting
  ; #+LOGGING: logreschedule lognoterepeat 
  ; etc.
  ; (One can also set org-log-reschule to either 'time or 'note)
  (org-log-note-headings '((done        . "%t: DONE")
                           (state       . "State %-12s from %-12S %t") ; Should not be changed as Agenda Log mode depends on this format
                           (note        . "%t: Note taken")
                           (reschedule  . "%t: Schedule changed (%S -> %s)")
                           (delschedule . "%t: Schedule deleted (was %S)")
                           (redeadline  . "%t: Deadline changed (%S -> %s)")
                           (deldeadline . "%t: Deadline removed (was %S)")
                           (refile      . "%t: Refiled")
                           (clock-out   . "")))

  ;; This portion is for ESS-Script (used by the org-babel R script)
  (inferior-R-args "--no-save --no-restore") ; When quitting a session manually, don't ask if this should be saved
  (ess-toggle-underscore nil) ; Do not substitute _ with <- when typing on the prompt manually
  (ess-startup-directory nil) ; Use current buffers default directory and don't ask for "R startup directory" when executing with org-babel-R
  (ess-ask-for-ess-directory nil) ;



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
   ;; 
   ;; Unfortunately, this only works as expected for #+begin_src and not inline source code blocks:
   ;; the :language property does not exist for inline source blocks; the org-element-inline-src-block-parser function shows
   ;; how to parse inline source blocks but I didn't get that to work...
   ;(defadvice org-babel-execute-src-block (around load-language nil activate)
      ;"Load language only when needed"
      ;(let ((language (org-element-property :language (org-element-at-point))))
        ;(unless (cdr (assoc (intern language) org-babel-load-languages)) ;; Does this language already exist in org-babel-load-languages? If not, insert new entry
          ;;(message "%s %s %s" load-language around activate)
          ;(add-to-list 'org-babel-load-languages '(R . t))
          ;(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
        ;ad-do-it)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)
     (R . t)
     (C . t)
     (ruby . t)
     ;(ocaml . t)
     ;(ditaa . t)
     ;(dot . t)
     (haskell . t)
     ;(octave . t)
     (sqlite . t)
     (perl . t)
     (screen . t)
     ;(plantuml . t)
     ;(lilypond . t)
     (org . t)
     (makefile . t)
     ))

   (setq org-src-preserve-indentation t) ;; When tangled, the source code will retain its indentation; may be important for white-space sensitive code

   ;;;
   ;;; Org Capture
   ;;;
   (setq org-capture-templates
         (quote (
                 ;("j" "Berufl. Journal" plain (file+function "~/workspace/inria/research-collab/journal.org" Shurakai/find-journal-tree)
                 ; "*** %? %^g" )
                 ("p" "Priv. Journal - normaler Eintrag" plain (file+function "~/org/journal_privat.org" Shurakai/find-journal-tree)
                  "*** %? %(org-set-tags-command)" )
                 ("s" "Priv. Journal - Eintrag mit Datum" plain (file+function "~/org/journal_privat.org" Shurakai/find-journal-tree)
                  "*** TODO %? %^g
SCHEDULED: %^t" )
                 ("k" "Priv. Journal - Kochrezept" plain (file+function "~/org/journal_privat.org" Shurakai/find-journal-tree)
                  (file "~/dotfiles/emacs/.emacs.d/org-capture-templates/kochrezept" ))
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

   (setq org-agenda-include-all-todo t)
   (setq org-agenda-include-diary t) ;; Shows entries from the Emacs diary - which I don't use...
   (setq org-agenda-span 'week) ; org-agenda-ndays is obsolete, therefore we now use org-agenda-span
   (setq org-agenda-show-all-dates t)
   (setq org-agenda-skip-deadline-if-done t)
   (setq org-agenda-skip-scheduled-if-done t)
   (setq org-agenda-skip-scheduled-if-deadline-is-shown t) ; If a task with a deadline has also been scheduled (or potentially even has an active timestamp), do not show an additional entry - only show the deadline
   (setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled) ; Don't show prewarnings before the scheduled date
   (setq org-agenda-start-on-weekday nil) ; Always start on the current day, not a specific weekday
   (setq org-agenda-start-with-follow-mode t) ;; When the curser is positioned on a line (or moved), show that entry in another window
   (setq org-agenda-sticky t) ; Keep agenda buffer in the background and do not re-generate every time agenda is called; I believe this might be a performance improvement at some point - refresh agenda buffer by pressing =r=

   ; Do not show tasks that have been scheduled or have deadlines in the normal todo list
   (setq org-agenda-todo-ignore-deadlines (quote all))
   (setq org-agenda-todo-ignore-scheduled (quote all))

   (setq org-agenda-window-setup (quote other-window)) ; Open agenda in another window
   (setq org-agenda-files (quote ("~/Documents/Notizen/privat.org" "~/org/journal_privat.org" )))
   (setq org-deadline-warning-days 14)
   (setq org-stuck-projects '("+LEVEL=3/-DONE-CANCELLED-DEFERRED-READ-TESTED-VISITED" ("*") nil "")) ;; This needs to be adapted

   (setq org-agenda-sorting-strategy
     (quote
      ((agenda habit-down time-up priority-down category-keep deadline-up alpha-up)
       (todo priority-down category-keep)
       (tags priority-down category-keep)
       (search category-keep))))

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

   (add-to-list 'org-modules 'org-habit) ;; Habits must be loaded this way according to the org manual: https://orgmode.org/manual/Tracking-your-habits.html - how to load it with use-package?
   (setq org-habit-show-habits-only-for-today t) ;

   ;;; Org Habit colors
   ; See https://protesilaos.com/codelog/2022-01-02-review-modus-themes-org-habit-colours/
   ; Tango colors: http://tango.freedesktop.org/static/cvs/tango-art-tools/palettes/Tango-Palette.png

   ; cheinrich: To avoid too many colors, I decided to set colors to the same value, no matter whether
   ; they are in the future or not

   ; Face for days on which a task shouldn‘t be done yet
   (set-face-attribute 'org-habit-clear-face nil :background "#4e9a06") ; dark green
   (set-face-attribute 'org-habit-clear-future-face nil :background "#4e9a06") ; dark green

   ; Face for days on which a task should start to be done (could have been done)
   (set-face-attribute 'org-habit-ready-face nil :background "#8ae234") ; light green
   (set-face-attribute 'org-habit-ready-future-face nil :background "#8ae234") ; light green

   ; Face for days on which a task is due (if the task was going to be overdue the next day)
   (set-face-attribute 'org-habit-alert-face nil :background "#fce94f") ; yellow
   (set-face-attribute 'org-habit-alert-future-face nil :background "#fce94f") ; yellow

   ; Face for days on which a task is overdue
   (set-face-attribute 'org-habit-overdue-face nil :background "#ef2929") ; red
   (set-face-attribute 'org-habit-overdue-future-face nil :background "#ef2929") ; dark red


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
             ("\C-cb" . 'org-iswitchb)
	  )
)

(use-package ess
    :straight (ess))

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
(setq-default line-spacing 0.00)
;;;(scroll-bar-mode 0)

; Disable "large file warnings" for files less than 50 MB; the default (10 MB) often triggers warnings for PDFs
(setq large-file-warning-threshold (* 50 1024 1024)); 50 Mb
(setq bidi-paragraph-direction 'left-to-right) ; Allegedly speeds emacs up a bit by telling it that all text is left to right (which is always the case for me)

; Set default fonts; passing the :height parameter helps with fontsize
(set-face-attribute 'default nil :family "Meslo LG M DZ for Powerline")
;(set-fontset-font "fontset-default" 'chinese-gbk (font-spec :size 15.0 :family "Sarasa Mono hc")) ;


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
; There is actually an entire evil-escape package for this that also exits buffers (https://github.com/syl20bnr/evil-escape/blob/master/evil-escape.el)
(defun escape-if-next-char (c)
  "Watches the next letter.  If c, then switch to Evil's normal mode; otherwise insert a k and forward unpressed key to unread-command events"
  (self-insert-command 1)
  (let ((next-key (read-event)))
    (if (= c next-key)
      (progn
        (delete-backward-char 1)
        (evil-force-normal-state)) ; CH: (evil-esc 1) didn't leave insert mode anymore (noticed in 2022), so I'm using this now - it seems to work...
      (setq unread-command-events (list next-key)))))

(defun escape-if-next-char-is-j (arg)
  (interactive "p")
  (if (= arg 1)
    (escape-if-next-char ?j)
    (self-insert-command arg)))


;;
;; Christian: I took the following two functions from Karl Voit's public config. They help with creating a CUSTOM_ID (or just ID) automatically.
;; https://github.com/novoid/dot-emacs/blob/master/config.org (22.05.22)
;; There is also a blog post on an older version of this: https://karl-voit.at/2019/11/16/UOMF-Linking-Headings/
;;

(require 'ffap)
(defun my/generate-sanitized-alnum-dash-string(str)
"Returns a string which contains only a-zA-Z0-9 with single dashes
 replacing all other characters in-between them.

 Some parts were copied and adapted from org-hugo-slug
 from https://github.com/kaushalmodi/ox-hugo (GPLv3)."
(let* (;; Remove "<FOO>..</FOO>" HTML tags if present.
       (str (replace-regexp-in-string "<\\(?1:[a-z]+\\)[^>]*>.*</\\1>" "" str))
       ;; Remove URLs if present in the string.  The ")" in the
       ;; below regexp is the closing parenthesis of a Markdown
       ;; link: [Desc](Link).
       (str (replace-regexp-in-string (concat "\\](" ffap-url-regexp "[^)]+)") "]" str))
       ;; Replace "&" with " and ", "." with " dot ", "+" with
       ;; " plus ".
       (str (replace-regexp-in-string
             "&" " and "
             (replace-regexp-in-string
              "\\." " dot "
              (replace-regexp-in-string
               "\\+" " plus " str))))
       ;; Replace German Umlauts with 7-bit ASCII.
       (str (replace-regexp-in-string "ä" "ae" str nil))
       (str (replace-regexp-in-string "ü" "ue" str nil))
       (str (replace-regexp-in-string "ö" "oe" str nil))
       (str (replace-regexp-in-string "ß" "ss" str nil))
       ;; Replace all characters except alphabets, numbers and
       ;; parentheses with spaces.
       (str (replace-regexp-in-string "[^[:alnum:]()]" " " str))
       ;; On emacs 24.5, multibyte punctuation characters like "："
       ;; are considered as alphanumeric characters! Below evals to
       ;; non-nil on emacs 24.5:
       ;;   (string-match-p "[[:alnum:]]+" "：")
       ;; So replace them with space manually..
       (str (if (version< emacs-version "25.0")
                (let ((multibyte-punctuations-str "：")) ;String of multibyte punctuation chars
                  (replace-regexp-in-string (format "[%s]" multibyte-punctuations-str) " " str))
              str))
       ;; Remove leading and trailing whitespace.
       (str (replace-regexp-in-string "\\(^[[:space:]]*\\|[[:space:]]*$\\)" "" str))
       ;; Replace 2 or more spaces with a single space.
       (str (replace-regexp-in-string "[[:space:]]\\{2,\\}" " " str))
       ;; Replace parentheses with double-hyphens.
       (str (replace-regexp-in-string "\\s-*([[:space:]]*\\([^)]+?\\)[[:space:]]*)\\s-*" " -\\1- " str))
       ;; Remove any remaining parentheses character.
       (str (replace-regexp-in-string "[()]" "" str))
       ;; Replace spaces with hyphens.
       (str (replace-regexp-in-string " " "-" str))
       ;; Remove leading and trailing hyphens.
       (str (replace-regexp-in-string "\\(^[-]*\\|[-]*$\\)" "" str)))
  str)
)

(defun my/id-get-or-generate()
"Returns the ID property if set or generates and returns a new one if not set.
 The generated ID is stripped off potential progress indicator cookies and
 sanitized to get a slug. Furthermore, it is prepended with an ISO date-stamp
 if none was found before."
    (interactive)
        (when (not (org-id-get))
            (progn
               (let* (
                      (my-heading-text (nth 4 (org-heading-components)));; retrieve heading string
                      (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+%\\]\\)" "" my-heading-text));; remove progress indicators like "[25%]"
                      (my-heading-text (replace-regexp-in-string "\\(\\[[0-9]+/[0-9]+\\]\\)" "" my-heading-text));; remove progress indicators like "[2/7]"
                      (my-heading-text (replace-regexp-in-string "\\(\\[#[ABC]\\]\\)" "" my-heading-text));; remove priority indicators like "[#A]"
                      (my-heading-text (replace-regexp-in-string "\\[\\[\\(.+?\\)\\]\\[" "" my-heading-text t));; removes links, keeps their description and ending brackets
;;                      (my-heading-text (replace-regexp-in-string "[<\\[][12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)[>\\]]" "" my-heading-text t));; removes day of week and time from date- and time-stamps (doesn't work somehow)
                      (my-heading-text (replace-regexp-in-string "<[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)>" "" my-heading-text t));; removes day of week and time from active date- and time-stamps
                      (my-heading-text (replace-regexp-in-string "\\[[12][0-9]\\{3\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\( .*?\\)\\]" "" my-heading-text t));; removes day of week and time from inactive date- and time-stamps
                      (new-id (my/generate-sanitized-alnum-dash-string my-heading-text));; get slug from heading text
                      (my-created-property (assoc "CREATED" (org-entry-properties))) ;; nil or content of CREATED time-stamp
                     )
                   (when (not (string-match "[12][0-9][0-9][0-9]-[01][0-9]-[0123][0-9]-.+" new-id))
                           ;; only if no ISO date-stamp is found at the beginning of the new id:
                           (if my-created-property (progn
                               ;; prefer date-stamp of CREATED property (if found):
                               (setq my-created-datestamp (substring (org-entry-get nil "CREATED" nil) 1 11)) ;; returns "2021-12-16" or nil (if no CREATED property)
                               (setq new-id (concat my-created-datestamp "-" new-id))
                           )
                           ;; use today's date-stamp if no CREATED property is found:
                           (setq new-id (concat (format-time-string "%Y-%m-%d--") new-id))))
                   (org-set-property "ID" new-id)
                   )
                 )
        )
        (kill-new (concat "id:" (org-id-get)));; put ID in kill-ring
        (org-id-get);; retrieve the current ID in any case as return value
)

(use-package bind-key
  :straight (bind-key)
  :bind (:prefix-map my-map
         :prefix-docstring "My own keyboard map"
         :prefix "\C-ck" ;; I wanted to use \C-c \C-k (k as in "keymap") but that didn't overwrite the mapping (already set to org-kill-note-or-show-branches -- see https://github.com/jwiegley/use-package/issues/811#issuecomment-573314421, but that doesn't work
         ("-" . text-scale-decrease)
         ("+" . text-scale-increase))
  :after org)

(bind-keys
  :map my-map
  ("g h" . my/id-get-or-generate)) ; 'generate headline'

(message "BENCHMARK: Init.el loaded in in %.2fs " (float-time (time-subtract (current-time) my/init-el-start-time)))
