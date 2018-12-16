;;
;; Packages
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(let ((my/packages '(
                      clojure-mode-extra-font-locking
                      diminish
                      flx-ido
                      ido-vertical-mode
                      magit
                      projectile
                      smex
                      tagedit
                      use-package
                      wrap-region)))
  (dolist (p my/packages)
    (unless (package-installed-p p)
      (package-install p))))

;; smex
(smex-initialize)
(setq smex-save-file (concat user-emacs-directory "data/" "smex-items"))

;; ido
(flx-ido-mode 1)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; projectile
(projectile-mode 1)

(use-package add-node-modules-path
  :ensure t
  :config
  (add-hook 'js-mode-hook #'add-node-modules-path))

(use-package alchemist
  :ensure t)

(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-ö") 'avy-goto-char))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :config
  (define-clojure-indent
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 'defun)
    (describe 'defun)
    (it 'defun)
    (match 1)))

(use-package clj-refactor
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  (add-hook 'clojure-mode-hook 'yas-minor-mode))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package elm-mode
  :ensure t
  :config
  (setq elm-format-on-save t))

(use-package emmet-mode
  :ensure t
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'flycheck-mode))

(use-package haskell-mode
  :ensure t)

(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook #'intero-mode))

(use-package js2-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (setq js-switch-indent-offset 2))

(use-package ido-completing-read+
	:ensure t
	:config
	(ido-mode 1)
	(ido-everywhere 1)
	(ido-ubiquitous-mode 1))

(use-package nord-theme
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this))

(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package prettier-js
  :ensure t
  :config
  (add-hook 'js2-mode-hook 'prettier-js-mode))

(use-package psc-ide
  :ensure t
  :config
  (add-hook 'purescript-mode-hook
    (lambda ()
      (psc-ide-mode)
      (company-mode)
      (flycheck-mode))))

(use-package purescript-mode
  :ensure t)

(use-package restclient
  :ensure t)

(use-package smartparens
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'js-mode-hook #'smartparens-mode)
  (add-hook 'ruby-mode-hook #'smartparens-mode)
  :bind
  ("C-)" . 'sp-forward-slurp-sexp)
  ("C-(" . 'sp-forward-barf-sexp))

(use-package terraform-mode
  :ensure t)

(use-package tide
  :ensure t
  :after
  (typescript-mode company flycheck)
  :hook
  ((typescript-mode . tide-setup)
    (typescript-mode . tide-hl-identifier-mode)))

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 2))

(use-package wrap-region
  :ensure t)

(use-package yaml-mode
  :ensure t)

;;
;; Load paths
;;

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")


;;
;; Keyboard
;;

;; set option key modifiers
(setq mac-option-modifier 'none)
(setq mac-right-option-modifier 'meta)


;;
;; Key bindings
;;

;; switch buffers
(global-set-key (kbd "C-x C-n") 'my-next-buffer)
(global-set-key (kbd "C-x C-p") 'my-previous-buffer)

;; magit
(global-set-key (kbd "C-x g") 'magit-status)

;; hippie-expand
(global-set-key (kbd "M-<tab>") 'hippie-expand)

;; comments
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)

;; smex
(global-set-key (kbd "M-x") 'smex)

;; backward kill line from point
(global-set-key (kbd "s-<backspace>") 'my-backward-kill-line-from-point)


;;
;; UI
;;

;; line numbers
(global-linum-mode 1)

;; disable scroll bar
(scroll-bar-mode -1)

;; disable tool bar
(tool-bar-mode -1)

;; disable menu bar
(menu-bar-mode -1)

;; theme
(load-theme 'brin t)

;; matching parenthesis
(show-paren-mode 1)

;; no bell
(setq ring-bell-function 'ignore)

;; font
(when (eq system-type 'gnu/linux)
  (set-default-font "DejaVu Sans Mono 10"))

(when (eq system-type 'darwin)
  (set-default-font "Hasklig 13"))

;; or
;; (when (eq system-type 'darwin)
;;   (set-face-attribute 'default nil :height 130))

;;
;; Navigation
;;

;; recently opened files
(recentf-mode 1)
(setq recentf-max-menu-items 42)


;;
;; Functions
;;

;; load-file current-file
(defun my-load-current-file ()
  "Load current file."
  (interactive)
  (load-file (buffer-file-name)))

(defun my-backward-kill-line-from-point ()
  "Kills line to the start of the line from point."
  (interactive)
  (kill-line 0))

;; skip global buffers when navigating to next/prev buffer
(defun my-next-buffer ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (next-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal current-buffer-name (buffer-name))))
      (next-buffer))))

(defun my-previous-buffer ()
  (interactive)
  (let ((current-buffer-name (buffer-name)))
    (previous-buffer)
    (while
        (and
         (string-match-p "^\*" (buffer-name))
         (not (equal current-buffer-name (buffer-name))))
      (previous-buffer))))

;;
;; Miscellaneous
;;

;; enable server mode
(server-mode t)

;; backups
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))

;; custom file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;
;; Performance (?)
;;

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
