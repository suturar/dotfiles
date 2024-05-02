(setq custom-file (make-temp-file "emacs-custom"))
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq use-file-dialog nil)
(global-display-line-numbers-mode 1)
(setq display-line-numbers-type 'relative)
(add-to-list 'default-frame-alist '(font . "MonaspiceArNerdFontMono-15"))
(add-to-list 'load-path (concat user-emacs-directory "local/"))
(use-package company
  :ensure t
  :hook (prog-mode latex-mode))
(use-package julia-mode
  :ensure t
  :mode "\\.jl$")
(use-package gruber-darker-theme
  :ensure t
  :init (load-theme 'gruber-darker t))
(use-package multiple-cursors
  :ensure t
  :config 
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  )
(use-package odin-mode
  :bind (:map odin-mode-map
	      ("C-c C-r" . 'odin-run-project)
	      ("C-c C-c" . 'odin-build-project)
	      ("C-c C-t" . 'odin-test-project)
	      )
  )

