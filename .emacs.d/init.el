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
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t)
(setopt use-short-answers t)
(add-to-list 'default-frame-alist '(font . "Iosevka-14"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)
(setq default-input-method 'spanish-prefix)
(setq auto-revert-verbose nil)
(setq set-mark-command-repeat-pop t)
(load (concat user-emacs-directory "local/make-mark-visible.el"))

;; Dired options
(use-package dired
  :config
  (setq dired-listing-switches "-alh")
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
  :hook (
         ((pyvenv-post-activate) .
          (lambda () (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3"))))
         ((pyenv-post-deactivate) .
          (lambda () (setq python-shell-interpreter "python3")))
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
(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . TeX-source-correlate-mode)
  :custom
  (TeX-parse-self t)
  (TeX-auto-save t)
  (LaTeX-electric-left-right-brace t)
  (TeX-electric-math `("$" . "$"))
  (TeX-master nil)
  :config
  (setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
	  "dvi2tty")
	 ((output-dvi style-pstricks)
	  "dvips and gv")
	 (output-dvi "xdvi")
	 (output-pdf "Zathura")
	 (output-html "xdg-open"))
	)
  )

;; (defun cdlatex-indent-maybe ()
;;   (when (or (bolp) (looking-back "^[ \t]+") (region-active-p))
;;     (LaTeX-indent-line) t))
;; (use-package cdlatex
;;   :ensure t
;;   :init
;;   ;; (setq cdlatex-takeover-parenthesis nil)
;;   (setq cdlatex-math-modify-prefix nil)
;;   :hook
;;   ;; (cdlatex-tab . cdlatex-indent-maybe)
;;   ;; (LaTeX-mode . cdlatex-mode)
;;   (org-mode . #'turn-on-org-cdlatex))
  
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
  (setq org-latex-compiler "lualatex")
  (add-to-list 'org-latex-packages-alist '("utf8x" "inputenc"))
  (add-to-list 'org-latex-packages-alist '("" "libertine"))
  (add-to-list 'org-latex-packages-alist '("" "unicode-math"))
  (setq org-highlight-latex-and-related '(native entities))
  :hook
  (org-mode . turn-on-org-cdlatex)
  )
(use-package cdlatex :ensure t)
(use-package counsel
  :ensure t
  :config
  (ivy-mode)
  (recentf-mode)
  :bind (:map global-map
              ("M-x" . counsel-M-x)
              ("C-x C-r" . counsel-recentf)
              ("C-x C-f" . counsel-find-file)
              ))
(use-package ivy-bibtex :ensure t
  :config
  (setq bibtex-completion-pdf-open-function
        (lambda (fpath)
          (call-process "zathura" nil 0 nil fpath)
          )
        )
  (setq bibtex-completion-pdf-field "file")
  )

(use-package org-ref
  :ensure t
  :after org
  :init
  (setq org-ref-insert-label-function 'org-ref-insert-label-link)
  (setq org-ref-insert-ref-function 'org-ref-insert-ref-link)
  (setq org-latex-prefer-user-labels t)
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

