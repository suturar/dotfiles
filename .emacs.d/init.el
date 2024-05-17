(setq custom-file (make-temp-file "emacs-custom"))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq use-file-dialog nil)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t)
(add-to-list 'default-frame-alist '(font . "MonaspiceArNerdFontMono-15"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
;; Dired options
(setq dired-listing-switches "-alh")
;; Packages
(use-package company
  :ensure t
  :hook (prog-mode latex-mode))
(use-package julia-mode
  :ensure t
  :mode "\\.jl$")
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)
  :init (setenv "JULIA_NUM_THREADS" "8")
  :config (julia-repl-set-terminal-backend 'vterm))
(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))
(use-package vterm
  :ensure t
  :bind( :map vterm-mode-map
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
(use-package odin-mode
  :bind (:map odin-mode-map
	      ("C-c C-r" . 'odin-run-project)
	      ("C-c C-c" . 'odin-build-project)
	      ("C-c C-t" . 'odin-test-project)
	      )
  )
(use-package smex
  :ensure t
  :bind (:map global-map
	      ("M-x" . 'smex)
	      ("M-X" . 'smex-major-mode-commands)
	      ("C-c C-c M-x" . 'execute-extended-command)
	      )
)
