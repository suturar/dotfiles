(load (concat user-emacs-directory "elpaca-init.el"))
(elpaca elpaca-use-package
  ;; Enable Elpaca support for use-package's :ensure keyword.
  (elpaca-use-package-mode))
(setq custom-file (make-temp-file "emacs-custom"))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq make-backup-files nil)
(setq use-file-dialog nil)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(setq column-number-mode t)
(setopt use-short-answers t)
(add-to-list 'default-frame-alist '(font . "MonaspiceArNerdFontMono-15"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
;; Dired options
(setq dired-listing-switches "-alh")
;; Packages
(use-package company
  :ensure t
  :hook ((prog-mode LaTeX-mode) . company-mode))
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
(use-package fasm-mode
  :mode "\\.fasm\\'")
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
;; LaTeX
;; Set up zathura with synctex (that's what TeX-source-correlate-mode does
(use-package tex
  :ensure auctex
  :hook (LaTeX-mode . TeX-source-correlate-mode)
  :config
  (setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
	  "dvi2tty")
	 ((output-dvi style-pstricks)
	  "dvips and gv")
	 (output-dvi "xdvi")
	 (output-pdf "Zathura")
	 (output-html "xdg-open"))
	))
;; Magit
(use-package magit
  :ensure t)
(use-package transient
  :ensure t)

;; Gnuplot
(use-package gnuplot
  :ensure t
  :mode ("\\.gp\\'" . gnuplot-mode))

;; Tramp
(setq tramp-auto-save-directory "~/.emacs.d/tramp-auto-save/")

;; Ido-mode
(ido-mode t)
(setq ido-auto-merge-work-directories-length -1)

;; Enable Disabled commands
(dolist (command '(upcase-region))
  (put command 'disabled nil))

