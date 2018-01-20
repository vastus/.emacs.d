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
                      ido-ubiquitous
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
(ido-mode 1)
(ido-ubiquitous-mode 1)
(ido-vertical-mode 1)
(ido-everywhere 1)
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;; projectile
(projectile-mode 1)

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

(use-package haskell-mode
  :ensure t)

(use-package hindent
  :ensure t
  :config
  (setq hindent-process-path "~/.local/bin/hindent")
  (setq hindent-reformat-buffer-on-save t)
  (add-hook 'haskell-mode-hook #'hindent-mode))

(use-package multiple-cursors
  :ensure t
  :config
  (require 'multiple-cursors)
  (global-set-key (kbd "s-d") 'mc/mark-next-like-this))

(use-package paredit
  :ensure t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode))

(use-package restclient
  :ensure t)

(use-package web-mode
  :ensure t
  :config
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-style-padding 2))

(use-package wrap-region
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
  (set-default-font "Fira Code 13"))
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
;; Miscellanous
;;

;; enable server mode
(server-mode t)


;;
;; Performance (?)
;;

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
