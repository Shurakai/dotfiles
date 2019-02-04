(require 'package)
(package-initialize)
(setq package-archives
'(("ELPA" . "http://tromey.com/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/")
   ("melpa" . "http://melpa.milkbox.net/packages/")
   ("marmalade" . "http://marmalade-repo.org/packages/")))

(add-to-list 'load-path "~/.emacs.d/elpa/evil-1.0.8")
(add-to-list 'load-path "~/.emacs.d/elpa/ob-lua-20160411.2024")
(add-to-list 'load-path "~/.emacs.d/elpa/typopunct-1.0")
(add-to-list 'load-path "~/.emacs.d/elpa/evil-easymotion-20160617.1840")

(add-to-list 'load-path "~/.emacs.d/org-mode/lisp/")
(add-to-list 'load-path "~/.emacs.d/org-mode/contrib/lisp/")
(require 'org-install)
(require 'org)

(setq auto-mode-alist
   (append (mapcar 'purecopy
      '(("\\.c$"   . c-mode)
        ("\\.h$"   . c-mode)
        ("\\.c.simp$" . c-mode)
        ("\\.h.simp$" . c-mode)
        ("\\.a$"   . c-mode)
        ("\\.w$"   . cweb-mode)
        ("\\.cc$"   . c++-mode)
        ("\\.S$"   . asm-mode)
        ("\\.s$"   . asm-mode)
        ("\\.p$"   . pascal-mode)
        ("\\.Rmd$" . poly-markdown-mode)
        ("\\.pas$" . pascal-mode)
        ("\\.tex$" . LaTeX-mode)
        ("\\.txi$" . Texinfo-mode)
        ("\\.el$"  . emacs-lisp-mode)
        ("emacs"  . emacs-lisp-mode)
        ("\\.ml[iylp]?" . tuareg-mode)
        ("[mM]akefile" . makefile-mode)
        ("[mM]akefile.*" . makefile-mode)
        ("\\.mak" . makefile-mode)
        ("\\.cshrc" . sh-mode)
        ("\\.html$" . html-mode)
        ("\\.org$" . org-mode)
)) auto-mode-alist))

  (load-theme 'tango-dark t)

(setq inhibit-splash-screen t)

(setq frame-title-format
  '("Emacs - " (buffer-file-name "%f"
    (dired-directory dired-directory "%b"))))

;;(scroll-bar-mode 0)
(tool-bar-mode -1)

(line-number-mode 1)
(column-number-mode 1)

(load-library "paren")
(show-paren-mode 1)
;;(transient-mark-mode t)
(require 'paren)

(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))

(global-set-key (kbd "C-c i") 
(lambda() (interactive)(org-babel-load-file "~/.emacs.d/init.org")))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(cond
 ((string-equal system-type "darwin")   ; Mac OS X
  (progn
    (setq
     ns-command-modifier 'meta         ; Apple/Command key is Meta
         ns-alternate-modifier nil         ; Option is the Mac Option key
         ns-use-mac-modifier-symbols  nil  ; display standard Emacs (and not standard Mac) modifier symbols
         ))
  )
 )

;; (cua-mode t)

(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key (kbd "C-c <right>") 'hs-show-block)
    (local-set-key (kbd "C-c <left>")  'hs-hide-block)
    (local-set-key (kbd "C-c <up>")    'hs-hide-all)
    (local-set-key (kbd "C-c <down>")  'hs-show-all)
    (hs-minor-mode t)))

(global-set-key [f5] '(lambda () (interactive) (revert-buffer nil t nil)))

(global-set-key "\^x\^e" 'compile)

(defun jump-mark ()
  (interactive)
  (set-mark-command (point)))
(defun beginning-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (beginning-of-defun))
(defun end-of-defun-and-mark ()
  (interactive)
  (push-mark (point))
  (end-of-defun))

(global-set-key "\^c\^b" 'beginning-of-defun-and-mark)
(global-set-key "\^c\^e" 'end-of-defun-and-mark)
(global-set-key "\^c\^j" 'jump-mark)
(global-set-key [S-f6] 'jump-mark)              ;; jump from mark to mark

(global-set-key "\M-g" 'goto-line)

(setq select-active-regions nil)
(setq x-select-enable-primary t)
(setq x-select-enable-clipboard t)
(setq mouse-drag-copy-region t)

;;  (if(string-equal system-type "gnu/linux")   ; Linux!
;;      (
      ;; (require (quote xclip))
      ;; (xclip-mode 1)
;;      )()
;;        )

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size

;; Inspired from http://tex.stackexchange.com/questions/166681/changing-language-of-flyspell-emacs-with-a-shortcut
;; (defun spell (choice)
;;    "Switch between language dictionaries."
;;    (interactive "cChoose:  (a) American | (f) Francais")
;;     (cond ((eq choice ?1)
;;            (setq flyspell-default-dictionary "american")
;;            (setq ispell-dictionary "american")
;;            (ispell-kill-ispell))
;;           ((eq choice ?2)
;;            (setq flyspell-default-dictionary "francais")
;;            (setq ispell-dictionary "francais")
;;            (ispell-kill-ispell))
;;           (t (message "No changes have been made."))) )

(define-key global-map (kbd "C-c s a") (lambda () (interactive) (ispell-change-dictionary "american")))
(define-key global-map (kbd "C-c s f") (lambda () (interactive) (ispell-change-dictionary "francais")))
(define-key global-map (kbd "C-c s r") 'flyspell-region)
(define-key global-map (kbd "C-c s b") 'flyspell-buffer)
(define-key global-map (kbd "C-c s s") 'flyspell-mode)

(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x M-g") 'magit-dispatch-popup)
;; (global-magit-file-mode 1)

(defun auto-fill-mode-on () (TeX-PDF-mode 1))
(add-hook 'tex-mode-hook 'TeX-PDF-mode-on)
(add-hook 'latex-mode-hook 'TeX-PDF-mode-on)
(setq TeX-PDF-mode t)

(add-hook 'org-mode-hook
      '(lambda ()
         (delete '("\\.pdf\\'" . default) org-file-apps)
         (add-to-list 'org-file-apps '("\\.pdf\\'" . "evince %s"))))
(setq TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))

(defun auto-fill-mode-on () (auto-fill-mode 1))
(add-hook 'text-mode-hook 'auto-fill-mode-on)
(add-hook 'emacs-lisp-mode 'auto-fill-mode-on)
(add-hook 'tex-mode-hook 'auto-fill-mode-on)
(add-hook 'latex-mode-hook 'auto-fill-mode-on)

;; (setq c-tab-always-indent nil)

  (setq c-default-style "k&r")
  (setq c-basic-offset 2)

 (defun c-reformat-buffer()
    (interactive)
    (save-buffer)
    (setq sh-indent-command (concat
                             "indent -i2 -kr --no-tabs"
                             buffer-file-name
                             )
          )
    (mark-whole-buffer)
    (universal-argument)
    (shell-command-on-region
     (point-min)
     (point-max)
     sh-indent-command
     (buffer-name)
     )
    (save-buffer)
    )
  ;;(define-key c-mode-base-map [f7] 'c-reformat-buffer)

(defalias 'yes-or-no-p 'y-or-n-p)

(require 'org-drill)
(setq org-drill-question-tag "KARTEIKARTE")
(setq org-drill-add-random-noise-to-intervals-p t)
(setq org-drill-save-buffers-after-drill-sessions-p nil)

(defun custom/org-drill-tag(tag)
  "Start org-drill with a user chosen question tag."
  (interactive "sInput the tag to drill: ")
  (custom-set-variables
   '(org-drill-question-tag tag))
  (org-drill)
  (custom-set-variables
   '(org-drill-question-tag "KARTEIKARTE")))

(require 'org-crypt)
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))

(setq org-crypt-key "516FCC3C")
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.

(setq auto-save-default nil)
  ;; Auto-saving does not cooperate with org-crypt.el: so you need
  ;; to turn it off if you plan to use org-crypt.el quite often.
  ;; Otherwise, you'll get an (annoying) message each time you
  ;; start Org.

  ;; To turn it off only locally, you can insert this:
  ;;
  ;; # -*- buffer-auto-save-file-name: nil; -*-

   (require 'org-download)
   (setq org-download-method 'attach)

(find-file "~/workspace/inria/research-collab/journal.org")

(setq org-directory "~/Documents/Notizen/")

(setq org-hide-leading-stars t)
(setq org-alphabetical-lists t)
(setq org-src-fontify-natively t)  ;; you want this to activate coloring in blocks
(setq org-src-tab-acts-natively t) ;; you want this to have completion in blocks
(setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
(setq org-pretty-entities t)       ;; to have \alpha, \to and others display as utf8 http://orgmode.org/manual/Special-symbols.html

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-capture-templates
      (quote (("j" "Berufl. Journal" plain (file+function "~/workspace/inria/research-collab/journal.org" Shurakai/find-journal-tree)
               "*** %? %^g" )
              ("p" "Priv. Journal" plain (file+function "~/org/journal_privat.org" Shurakai/find-journal-tree)
               "*** %? %^g" )
              ("f" "Franz. Vokabel" plain (file "~/Documents/Notizen/Franzoesisch/Vokabeln.org")
               "** :KARTEIKARTE:FRANZ:
:PROPERTIES:
:DRILL_CARD_TYPE: twosided
:END:
\*** Deutsch
    %^{prompt}
\*** Französisch
    %^{prompt}
" ))))

(setq org-agenda-include-all-todo t)
(setq org-agenda-include-diary t)

(setq org-id-method (quote uuidgen))

;; (global-visual-line-mode t)

;; see http://thread.gmane.org/gmane.emacs.orgmode/42715
(eval-after-load 'org-list
  '(add-hook 'org-checkbox-statistics-hook (function ndk/checkbox-list-complete)))

(defun ndk/checkbox-list-complete ()
  (save-excursion
    (org-back-to-heading t)
    (let ((beg (point)) end)
      (end-of-line)
      (setq end (point))
      (goto-char beg)
      (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]" end t)
            (if (match-end 1)
                (if (equal (match-string 1) "100%")
                    ;; all done - do the state change
                    (org-todo 'done)
                  (org-todo 'todo))
              (if (and (> (match-end 2) (match-beginning 2))
                       (equal (match-string 2) (match-string 3)))
                  (org-todo 'done)
                (org-todo 'todo)))))))

(defun org-cua-dwim-turn-on-org-cua-mode-partial-support ()
  "This turns on org-mode cua-mode partial support; Assumes
shift-selection-mode is available."
  (interactive)
  (set (make-local-variable 'org-support-shift-select) t)
  (cua-mode 1)
  (add-hook 'pre-command-hook 'cua--pre-command-handler nil t)
  (add-hook 'post-command-hook 'cua--post-command-handler nil t)
  (set (make-local-variable 'cua-mode) t)
  (set (make-local-variable 'org-cua-dwim-was-move) nil)
  (set (make-local-variable 'shift-select-mode) nil))

;;;###autoload
(add-hook 'org-mode-hook 'org-cua-dwim-turn-on-org-cua-mode-partial-support)

(defvar org-cua-dwim-was-move nil)
(defvar org-cua-dwim-debug nil)
(defvar org-cua-dwim t)

(defadvice handle-shift-selection (around org-cua-dwim)
  (let ((is-org-mode (and (not (minibufferp))
                          (eq major-mode 'org-mode)))
        (do-it t))
    ;(setq org-cua-dwim-shift-translated this-command-keys-shift-translated)
    (when (and org-cua-dwim
               is-org-mode this-command-keys-shift-translated
               (not org-cua-dwim-was-move))
      (when org-cua-dwim-debug
        (message "Turn ON shift-select-mode & delete-selection-mode"))
      (delete-selection-mode 1)
      (set (make-local-variable 'org-cua-dwim-was-move) t)
      ;(set (make-local-variable 'cua--last-region-shifted) t)
      ;(set (make-local-variable 'cua--explicit-region-start) nil)
      (set (make-local-variable 'shift-select-mode) t)
      (set (make-local-variable 'cua-mode) nil))
    (when (and org-cua-dwim
               is-org-mode (not this-command-keys-shift-translated)
               org-cua-dwim-was-move)
      (when org-cua-dwim-debug
        (message "Turn Off shift-select-mode & delete-selection-mode"))
      (delete-selection-mode -1)
      (set (make-local-variable 'shift-select-mode) nil)
      (set (make-local-variable 'cua-mode) t)
      (set (make-local-variable 'org-cua-dwim-was-move) nil))
    (when do-it
      ad-do-it)
    (when (and org-cua-dwim
               is-org-mode
               mark-active)
      (cua--select-keymaps))))

(defmacro org-cua-dwim-fix-cua-command (cmd)
  "Defines advice for a CUA-command that will turn on CUA mode
before runnind ant hen run the `cua--precommand-handler'"
  `(progn
     (defadvice ,(intern cmd) (around org-cua-dwim)
     "Try to fix the org copy and paste problem."
     (when (and (not (minibufferp)) (not cua-mode)
                (eq major-mode 'org-mode))
       (when org-cua-dwim-debug
         (message "Turn Off shift-select-mode & delete-selection-mode  (CUA command)"))
       (delete-selection-mode -1)
       (set (make-local-variable 'shift-select-mode) nil)
       (set (make-local-variable 'cua-mode) t)
       (set (make-local-variable 'org-cua-dwim-was-move) nil)
       (cua--pre-command-handler))
     ad-do-it)
     (ad-activate ',(intern cmd))))

;; Advise all CUA commands active when selection is active
(org-cua-dwim-fix-cua-command "cua--prefix-override-handler")
(org-cua-dwim-fix-cua-command "cua-repeat-replace-region")
(org-cua-dwim-fix-cua-command "cua--shift-control-c-prefix")
(org-cua-dwim-fix-cua-command "cua--shift-control-x-prefix")
(org-cua-dwim-fix-cua-command "cua-toggle-rectangle-mark")
(org-cua-dwim-fix-cua-command "cua-delete-region")
(org-cua-dwim-fix-cua-command "cua-cut-region")
(org-cua-dwim-fix-cua-command "cua-copy-region")
(org-cua-dwim-fix-cua-command "cua-cancel")
(org-cua-dwim-fix-cua-command "cua-toggle-global-mark")
(org-cua-dwim-fix-cua-command "cua-paste")
(org-cua-dwim-fix-cua-command "cua-exchange-point-and-mark")
(org-cua-dwim-fix-cua-command "cua-scroll-down")
(org-cua-dwim-fix-cua-command "cua-scroll-up")
(org-cua-dwim-fix-cua-command "cua-set-mark")
(org-cua-dwim-fix-cua-command "cua-paste-pop")



(ad-activate 'handle-shift-selection)

(require 'org-protocol)

(setq org-export-babel-evaluate nil) ;; This is for org-mode<9. 
;;  Otherwise, you need to set #+PROPERTY: header-args :eval never-export in the beginning or your document
(setq org-confirm-babel-evaluate nil)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '(
     (shell . t)
     (python . t)
     (R . t)
     (C . t)
     (ruby . t)
     (ocaml . t)
     (ditaa . t)
     (dot . t)
     (haskell . t)
     (octave . t)
     (sqlite . t)
     (perl . t)
     (screen . t)
     (plantuml . t)
     (lilypond . t)
     (org . t)
     (makefile . t)
     ))
  (setq org-src-preserve-indentation t)

(add-to-list 'org-structure-template-alist
        '("S" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("m" "#+begin_src emacs-lisp\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("P" "#+begin_src python :results output :session *python* :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("b" "#+begin_src sh :results output :exports both\n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-to-list 'org-structure-template-alist
        '("B" "#+begin_src sh :session foo :results output :exports both \n\n#+end_src" "<src lang=\"sh\">\n\n</src>"))

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images) 
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-babel-result-hide-all)

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

 (unless (boundp 'org-latex-classes) (setq org-latex-classes nil))

 (add-to-list 'org-latex-classes '("acm-proc-article-sp" "\\documentclass{acm_proc_article-sp}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

 (setq org-latex-to-pdf-process '("pdflatex -interaction nonstopmode -output-directory %o %f ; bibtex `basename %f | sed 's/\.tex//'` ; pdflatex -interaction nonstopmode -output-directory  %o %f ; pdflatex -interaction nonstopmode -output-directory %o %f"))

(add-to-list 'org-latex-classes '("article" "\\documentclass{article}\n \[NO-DEFAULT-PACKAGES]\n \[EXTRA]\n  \\usepackage{graphicx}\n  \\usepackage{hyperref}"  ("\\section{%s}" . "\\section*{%s}") ("\\subsection{%s}" . "\\subsection*{%s}")                       ("\\subsubsection{%s}" . "\\subsubsection*{%s}")                       ("\\paragraph{%s}" . "\\paragraph*{%s}")                       ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;(if (require 'org-toc nil t)
;;    (add-hook 'org-mode-hook 'toc-org-enable)
;;  (warn "toc-org not found"))

;; Set to the name of the file where new notes will be stored
(setq org-mobile-inbox-for-pull "~/org/flagged.org")
;; Set to <your Dropbox root directory>/MobileOrg.
(setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")

(if (require 'org-bullets)
     (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
     (warn "org-bullets not found"))
(org-bullets-mode 1)

(require 'htmlize)
;; (require 'org-publish) ;;; this is obsolete! do not call this or you will regret it at some point!
(defun sr-org-notes-sitemap-complete ()
  "Take the sitemap-file and turn it into the menu-structure file for Marco Pratesi's phplayersmenu."
  (let* ((base-dir (file-name-as-directory (plist-get project-plist :base-directory)))
         (orig (expand-file-name (concat base-dir (plist-get project-plist :sitemap-filename))))
         (strip-suffix (or (plist-get project-plist :base-extension) "org"))
         (add-suffix (or (plist-get project-plist :html-extension) "html"))
         (link-target (or (plist-get project-plist :menu/link-target) "_blank"))
         (menu-file (or (plist-get project-plist :menu/structure-file) nil))

         (visiting (find-buffer-visiting orig))
         (visiting-output (find-buffer-visiting menu-file))

         (input-buffer (find-file orig))
         (output-buffer (find-file menu-file))

         (old-ndots 1)
         (sub "")
         (old-sub ""))

    (unless menu-file
      (throw 'sr-org-note-kb-completion-error
             "No menu structure file provided. Giving up."))

    (with-current-buffer output-buffer
      (erase-buffer))

    (with-current-buffer input-buffer
      (widen)
      (goto-char (point-min))
      (while (re-search-forward org-bracket-link-analytic-regexp (point-max) t)
        (let ((link (match-string-no-properties 3))
              (text (match-string-no-properties 5))
              (pos 0)
              (ndots 1))

      (with-current-buffer output-buffer
        (if (string-match (concat "\\(" strip-suffix "\\)$") link)
            (setq link (replace-match add-suffix t t link)))
        (while (setq pos (string-match "/" link pos))
          (setq ndots (+ ndots 1))
          (setq pos (+ pos 1)))

        (when (< 1 ndots)
          (string-match "\\(/[^/]*\\)$" link)
          (setq sub (replace-match "" t t link))

          (unless (string= sub old-sub)
            (let ((ds 0)
                  (subs (split-string sub "/"))
                  (old-subs (split-string old-sub "/")))
              (while (string= (car old-subs) (car subs))
                (setq ds (+ ds 1))
                (pop old-subs)
                (pop subs))
              (dolist (d subs)
                (setq ds (+ ds 1))
                (insert
                 (concat
                  (make-string ds ?.) "|" d "\n")))
              (setq old-sub sub))))

        (insert
         (concat
          (make-string ndots ?.) "|" text "|" link "|||" link-target "\n"))
        (setq old-ndots ndots)
        ))))

    (or visiting (kill-buffer input-buffer))

    (with-current-buffer output-buffer
      (save-buffer))
    (or visiting-output (kill-buffer output-buffer))
))

;; stolen from http://orgmode.org/worg/sources/emacs.el
(eval-after-load "ox-html"
'(setq org-html-scripts
       (concat org-html-scripts "\n"
               "<script type=\"text/javascript\">
    function rpl(expr,a,b) {
      var i=0
      while (i!=-1) {
         i=expr.indexOf(a,i);
         if (i>=0) {
            expr=expr.substring(0,i)+b+expr.substring(i+a.length);
            i+=b.length;
         }
      }
      return expr
    }

    function show_org_source(){
       document.location.href = rpl(document.location.href,\".php\",\".org\");
    }
</script>
")))


(setq org-html-htmlize-output-type 'css)

(setq org-publish-project-alist
      '(
("org-notes"
 :base-directory "~/org/public_html/"
 :base-extension "org"
 :html-extension "php"
 :htmlize-output-type "css"
 :htmlized-source t
 :publishing-directory "~/public_html/org/"
 :recursive t
 :publishing-function (org-html-publish-to-html org-org-publish-to-org)
 :headline-levels 4             ; Just the default for this project.
 :auto-preamble t
 :auto-sitemap t                ; Generate sitemap.org automagically...
 :sitemap-filename "sitemap-generated.org"
 :sitemap-title "Sitemap"         ; ... with title 'Sitemap'. 
 :sitemap-sort-files "alphabetically"
 :sitemap-sort-folders "last"
 :sitemap-ignore-case t
;;; tree menus
 :sitemap-style "tree"
 :exclude "sitemap.org"
 ;; Layersmenu:
 :completion-function sr-org-notes-sitemap-complete
 :menu/structure-file "~/org/public_html/menu-structure-file.txt"
 :menu/link-target "_self" ;; optional
 :html-postamble "<div id=\"show_source\"><input type=\"button\" value=\"Show Org source\" onClick='show_org_source()'></div>"
;; :body-only t ;; Only export section between <body> </body>
)
("org-static"
 :base-directory "~/org/public_html/"
 :base-extension "css\\|js\\|ijs\\|svg\\|jpeg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|css\\|php\\|html\\|jpg\\|png\\|gif\\|zip\\|pdf\\|ps\\|ps.gz\\|tar.gz\\|tgz\\|c\\|ml\\|txt\\|avi\\|bib\\|m\\|tex\\|exe\\|dat\\|R\\|csv\\|htaccess"
 :publishing-directory "~/public_html/org/"
 :recursive t
 :publishing-function org-publish-attachment
 )
 ("org-sources"
  :base-directory  "~/org/public_html/"
  :base-extension "org"
  :publishing-directory "~/public_html/org/"
  :recursive t
  :publishing-function org-publish-attachment)
 ("org" :components ("org-notes" "org-static" "org-sources"))
      ))

(defun org-publish-current-file-custom () 
  (interactive)
  (setq org-export-babel-evaluate nil) ;; do not run babel!
  (org-publish-current-file) 
  (shell-command "make -C ~/org/public_html publish-light")
)

(global-set-key [f6] 'org-publish-current-file-custom)

(defun org-publish-all-custom ()
   (setq org-export-babel-evaluate nil) ;; do not run babel!
   (org-publish-all)  ;; passing t to org-publish-all forces to regenerate everything
   (kill-emacs 0)
   )

;;  (add-to-list 'load-path "/usr/share/org-mode/lisp/")
;; (require 'ox-freemind)

(require 'ox-md)

(require 'tramp)
(setq tramp-default-method "scp")

(require 'recentf)
    (recentf-mode 1)
    (setq recentf-max-menu-items 25)
    (global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun undo-kill-buffer (arg)
  "Re-open the last buffer killed.  With ARG, re-open the nth buffer."
  (interactive "p")
  (let ((recently-killed-list (copy-sequence recentf-list))
         (buffer-files-list
          (delq nil (mapcar (lambda (buf)
                              (when (buffer-file-name buf)
                                (expand-file-name (buffer-file-name buf)))) (buffer-list)))))
    (mapc
     (lambda (buf-file)
       (setq recently-killed-list
             (delq buf-file recently-killed-list)))
     buffer-files-list)
    (find-file
     (if arg (nth arg recently-killed-list)
       (car recently-killed-list)))))
(global-set-key (kbd "C-S-t") 'undo-kill-buffer)

(require 'evil)
(require 'evil-easymotion)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(custom-enabled-themes (quote (tango-dark)))
 '(line-number-mode nil)
 '(org-agenda-files
   (quote
    ("~/Documents/Notizen/privat.org" "~/org/journal_privat.org" "~/workspace/inria/research-collab/journal.org")))
 '(org-agenda-ndays 7)
 '(org-agenda-show-all-dates t)
 '(org-agenda-skip-deadline-if-done t)
 '(org-agenda-skip-scheduled-if-done t)
 '(org-agenda-start-on-weekday nil)
 '(org-babel-exp-inline-code-template "src_%lang[%switches%flags]{}")
 '(org-babel-shell-names
   (quote
    ("sh" "bash" "csh" "ash" "dash" "ksh" "mksh" "posh" "zsh")))
 '(org-deadline-warning-days 14)
 '(org-default-notes-file "/tmp/notes.org")
 '(org-hide-leading-stars t)
 '(org-latex-table-caption-above nil)
 '(org-remember-store-without-prompt t)
 '(org-remember-templates
   (quote
    ((116 "* TODO %?
  %u" "~/Documents/Notizen/privat.org" "Tasks")
     (110 "* %u %?" "/tmp/notes.org" "Notes"))))
 '(org-reverse-note-order t)
 '(package-selected-packages
   (quote
    (org-bullets magit helm dash-functional typopunct ob-lua lua-mode evil-org evil-easymotion ess epresent color-theme-sanityinc-tomorrow color-theme)))
 '(remember-annotation-functions (quote (org-remember-annotation)))
 '(remember-handler-functions (quote (org-remember-handler)))
 '(safe-local-variable-values
   (quote
    ((org-emphasis-alist
      ("*" bold)
      ("/" italic)
      ("_" nil)
      ("=" org-verbatim verbatim)
      ("~" org-code verbatim)
      ("+"
       (:strike-through nil)))
     (org-emphasis-alist
      ("*" bold)
      ("/" italic)
      ("__" underline)
      ("=" org-verbatim verbatim)
      ("~" org-code verbatim)
      ("+"
       (:strike-through nil)))))))

(require 'remember)
(evilem-default-keybindings ",")
;; Escape on jj
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
 
(define-key evil-insert-state-map (kbd "j") 'escape-if-next-char-is-j)
(setq org-use-sub-superscripts "{}")

(eval-after-load "org"
  '(progn
     (require 'typopunct)
     (typopunct-change-language 'english)
     (typopunct-mode t)))

(with-eval-after-load 'ox-latex
   (add-to-list 'org-latex-classes
        '("articleA"
          "\\documentclass[12pt,a4paper,oneside,smallheadings,pointlessnumbers,BCOR12mm,DIVcalc]{scrreprt}%aus dem KOMA-Script-Paket"
          ("\\chapter{%s}" . "\\chapter*{%s}")
          ("\\section{%s}" . "\\section*{%s}")
          ("\\subsection{%s}" . "\\subsection*{%s}")
          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
          ("\\paragraph{%s}" . "\\paragraph*{%s}")
          ("\\subparagraph{%s}" . "\\subparagraph*{%s}")
        )))

;;        (org-babel-tangle-file "~/.emacs.d/init.org"
;;                               "~/.emacs.d/init.el"
;;                               "emacs-lisp")
(global-set-key (kbd "C-x x") 'execute-extended-command)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
