(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(setq custom-file (make-temp-file "emacs-custom"))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq use-file-dialog nil)
(setq-default indent-tabs-mode nil)
(setq-default compilation-scroll-output t)
(setq compilation-max-output-line-length nil)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t)
(setopt use-short-answers t)
(add-to-list 'default-frame-alist '(font . "Comic Code-16"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq default-input-method 'spanish-prefix)
(setq auto-revert-verbose nil)
(setq set-mark-command-repeat-pop t)
(global-so-long-mode 1)
;; (load (concat user-emacs-directory "local/make-mark-visible.el"))

;; WARNING: This is something I should take a closer look into
(setq
 display-buffer-alist
 '((".*"
    (display-buffer-reuse-window
     display-buffer-in-previous-window)
    (reusable-frames . t))))


;; Dired options
(use-package dired
  :config
  (setq dired-listing-switches "-alh")
  (setq dired-dwim-target t)
  (add-to-list 'dired-guess-shell-alist-user '("\\.pdf\\'" "zathura"))
  :hook ((dired-mode) . auto-revert-mode)
  :bind (:map
	 dired-mode-map
	 ("<mouse-2>" . 'dired-mouse-find-file)))

(use-package cc-mode
  :config (setq-default c-basic-offset 4)
  )

(use-package pyvenv
  :ensure t
  :config (setq pyvenv-mode-line-indicator '(pyvenv-virtual-env-name
                                             ("pyvenv:" pyvenv-virtual-env-name)))
  (pyvenv-mode t)
  (setq python-shell-setup-codes '("from importlib import reload"))
  (setq python-shell-font-lock-enable nil)
  :hook (
         (pyvenv-post-activate .
          (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python"))))
         (pyenv-post-deactivate .
          (lambda () (setq python-shell-interpreter "python")))
         )
  )

(defun python-send-paragraph () (interactive)
       (save-excursion
         (mark-paragraph)
         (command-execute 'python-shell-send-region)))
(use-package python
  :ensure t
  :bind (:map python-mode-map
              ("C-c h" . python-send-paragraph)
              ("C-c r" . python-shell-restart)
              ("C-c C-k" . kill-compilation)
              :map inferior-python-mode-map
              ("C-c r" . python-shell-restart)
              ("C-c C-k" . kill-compilation)
              )
  )

(use-package company
  :ensure t
  :custom
  (company-selection-wrap-around t)
  :config
  (setq company-backends (delete 'company-clang company-backends))
  (global-company-mode)
  (setq company-idle-delay nil)
  )
(use-package browse-kill-ring
  :ensure t
  :bind( :map global-map
	 ("M-y" . 'browse-kill-ring)
	 )
  )
(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  )
(use-package julia-mode
  :ensure t
  :mode "\\.jl$")
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)
  :init (setenv "JULIA_NUM_THREADS" "8")
  :config (julia-repl-set-terminal-backend 'eat))
(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))
(use-package solarized-theme :ensure t)
(use-package eat
  :ensure t
  :bind( :map eat-mode-map
	 ("C-c C-o" . 'compile-goto-error)
	 )
  )
(use-package multiple-cursors
  :ensure t
  :bind (:map global-map 
	      ("C->" . 'mc/mark-next-like-this)
	      ("C->" . 'mc/mark-next-like-this)
	      ("C-<" . 'mc/mark-previous-like-this)
	      ("C-c C-<" . 'mc/mark-all-like-this)
	      )
  )
(use-package fasm-mode
  :mode "\\.fasm\\'")
(use-package odin-mode)
(use-package jai-mode)
(use-package simpc-mode
  :mode ("\\.[hc]\\(pp\\)?\\'" . simpc-mode))

(use-package image-mode
  :hook (image-mode . auto-revert-mode))
(use-package text-mode
  :hook (text-mode . auto-fill-mode)
  :hook (text-mode . (lambda () (set-fill-column 100)))
  :mode ("\\.md\\'" . text-mode))

;; LaTeX
;; Set up zathura with synctex (that's what TeX-source-correlate-mode does
(use-package latex
  :ensure auctex
  :hook
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . (lambda () (local-unset-key (kbd "C-c ]"))))
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (LaTeX-electric-left-right-brace t)
  (TeX-electric-math `("$" . "$"))
  (TeX-master nil)
  :config
  (TeX-add-style-hook
   "latex"
   (lambda ()
     (TeX-add-symbols
      '("eqref" TeX-arg-ref))))
  (setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
	                              "dvi2tty")
	                             ((output-dvi style-pstricks)
	                              "dvips and gv")
	                             (output-dvi "xdvi")
	                             (output-pdf "Zathura")
	                             (output-html "xdg-open"))
	)
  )

(defun cdlatex-indent-maybe ()
  (when (or (bolp) (looking-back "^[ \t]+") (region-active-p))
    (LaTeX-indent-line) t))
(use-package cdlatex
  :ensure t
  :init
  (setq cdlatex-takeover-parenthesis nil)
  ;; (setq cdlatex-math-modify-prefix nil)
  :hook
  (cdlatex-tab . cdlatex-indent-maybe)
  (LaTeX-mode . cdlatex-mode)
  (org-mode . org-cdlatex-mode)
)
  
;; Magit
(use-package magit
  :ensure t)
(use-package transient
  :ensure t)

;; Gnuplot
(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode)
  :hook (gnuplot-comint-mode . (lambda ()
				 (gnuplot-send-string-to-gnuplot "set terminal qt size 800, 600\n" "line")
				 ))
  )


(use-package org
  :config
  (setq org-highlight-latex-and-related '(native entities))
  (setq org-format-latex-options '(:foreground default :background default :scale 2.5 :html-foreground
                                               "Black" :html-background "Transparent" :html-scale 1.0
                                               :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
  (setq org-format-latex-header "\\documentclass{article}
\\usepackage[usenames]{color}
[DEFAULT-PACKAGES]
[PACKAGES]
\\pagestyle{empty}             % do not remove
% The settings below are copied from fullpage.sty
\\setlength{\\textwidth}{\\paperwidth}
\\addtolength{\\textwidth}{-3cm}
\\setlength{\\oddsidemargin}{1.5cm}
\\addtolength{\\oddsidemargin}{-2.54cm}
\\setlength{\\evensidemargin}{\\oddsidemargin}
\\setlength{\\textheight}{\\paperheight}
\\addtolength{\\textheight}{-\\headheight}
\\addtolength{\\textheight}{-\\headsep}
\\addtolength{\\textheight}{-\\footskip}
\\addtolength{\\textheight}{-3cm}
\\setlength{\\topmargin}{1.5cm}
\\addtolength{\\topmargin}{-2.54cm}
\\newcommand{\\dd}{\\text{d}}")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((gnuplot . t)))
  (setq org-capture-templates
        '(
          ("t" "General TODO"
           entry (file+headline "~/org/todos.org" "General Tasks")
           "* TODO %? \n:Created: %T\n "
           )
          ("n" "Note"
           entry (file+headline "~/org/notes.org" "Random Notes")
           "** %?"
           :empty-lines 0)

          ("s" "Slipbox"
           entry (file "~/org/org-roam/inbox.org")
          "* %?\n")
          ))
  (setq org-agenda-files '("~/org"))
  :hook
  (org-mode . (lambda () (local-unset-key (kbd "C-,"))))
  )
(use-package counsel
  :ensure t
  :config
  (ivy-mode)
  (recentf-mode)
  (setq recentf-max-saved-items 100)
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-r" . counsel-recentf)
              ("C-x C-f" . counsel-find-file)
              )
  :hook (buffer-list-update . recentf-track-opened-file)
  )

(use-package ivy-bibtex :ensure t
  :config
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "zathura" nil 0 nil fpath)
          )
        )
  (setq bibtex-completion-pdf-field "file")
  (setq bibtex-completion-bibliography '("~/org/org-roam/Bibliography.bib"))
  :bind (
	 ("C-c ]" . ivy-bibtex-with-local-bibliography)
         )
  )

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-directory "~/org/org-roam")
  (setq org-roam-capture-templates
      '(("m" "main" plain
         "%?"
         :if-new (file+head "main/${slug}.org"
                            "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("r" "reference" plain "%?"
         :if-new
         (file+head "reference/${title}.org" "#+title: ${title}\n")
         :immediate-finish t
         :unnarrowed t)
        ("a" "article" plain "%?"
         :if-new
         (file+head "articles/${title}.org" "#+title: ${title}\n#+filetags: :article:\n")
         :immediate-finish t
         :unnarrowed t)))
  (setq org-roam-node-display-template
      (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
)

(use-package org-ref
  :ensure t
  :after org
  :init
  (setq org-ref-insert-label-function 'org-ref-insert-label-link)
  (setq org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (setq org-latex-prefer-user-labels t)
  (setq org-latex-pdf-process '("latexmk -f -bibtex -pdf -%latex -interaction=nonstopmode -output-directory=%o %f"))
  (require 'bibtex)
  (require 'org-ref-ivy)
  :bind (:map org-mode-map
              ("C-c ]" . org-ref-insert-link)
              )
  :hook (org-mode . (lambda ()
                      (setq-local bibtex-completion-bibliography (org-ref-find-bibliography))
                      (setq-local bibtex-completion-library-path ".")
                      ))
  )

(use-package move-dup
  :ensure t
  :bind (
	 ("M-[" . move-dup-move-lines-up)
	 ("M-]" . move-dup-move-lines-down))
  )

;; Tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save/")


;; Enable Disabled commands
(dolist (command '(upcase-region narrow-to-region))
  (put command 'disabled nil))

;; My keybindings
(keymap-global-set "C-," #'duplicate-line)
(keymap-global-set "C-c C-o" #'find-file-at-point)
(keymap-global-set "C-c o" #'find-file-at-point)
(keymap-global-set "M-o" #'other-window)
(keymap-global-set "M-O" #'window-swap-states)
(keymap-global-set "M-/" #'company-complete)
(keymap-global-set "M-T" #'transpose-regions)
(keymap-global-set "<f5>" #'compile)
(keymap-global-set "C-x ;" #'indent-for-comment)

(keymap-global-set "C-c a" #'org-agenda)
(keymap-global-set "C-c c" #'org-capture)
(keymap-global-set "M-u" #'upcase-dwim)
(keymap-global-set "M-l" #'downcase-dwim)
(keymap-global-set "M-c" #'capitalize-dwim)

(global-set-key (kbd "C-M-i") #'org-roam-node-insert)
(global-set-key (kbd "C-M-o") #'org-roam-node-find)
(global-set-key (kbd "C-M-r") #'org-roam-buffer-toggle)

;; My functions
(defun switch-theme (theme)
  ;; This interactive call is taken from `load-theme'
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
			     (mapcar 'symbol-name
				     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(cl-defmethod org-roam-node-type ((node org-roam-node))
  "Return the TYPE of NODE."
  (condition-case nil
      (file-name-nondirectory
       (directory-file-name
        (file-name-directory
         (file-relative-name (org-roam-node-file node) org-roam-directory))))
    (error "")))
