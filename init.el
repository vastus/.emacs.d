;;
;; Packages
;;

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(let ((my/packages '(cider
                      clojure-mode
                      clojure-mode-extra-font-locking
                      diminish
                      elm-mode
                      exec-path-from-shell
                      flx-ido
                      ido-ubiquitous
                      ido-vertical-mode
                      magit
                      paredit
                      projectile
                      smex
                      tagedit
                      use-package
                      wrap-region)))
  (dolist (p my/packages)
    (unless (package-installed-p p)
      (package-install p))))

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; (use-package parinfer
;;   :ensure t
;;   :bind
;;   (("C-," . parinfer-toggle-mode))
;;   :init
;;   (progn
;;     (setq parinfer-extensions
;;           '(defaults       ; should be included.
;; 	     pretty-parens  ; different paren styles for different modes.
;; 	     evil           ; If you use Evil.
;; 	     lispy          ; If you use Lispy. With this extension, you should install Lispy and do not enable lispy-mode directly.
;; 	     paredit        ; Introduce some paredit commands.
;; 	     smart-tab      ; C-b & C-f jump positions and smart shift with tab & S-tab.
;; 	     smart-yank))   ; Yank behavior depend on mode.
;;     (add-hook 'clojure-mode-hook #'parinfer-mode)
;;     (add-hook 'emacs-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'common-lisp-mode-hook #'parinfer-mode)
;;     (add-hook 'scheme-mode-hook #'parinfer-mode)
;;     (add-hook 'lisp-mode-hook #'parinfer-mode)))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package elm-mode
  :ensure t
  :config
  (setq elm-format-on-save t))

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
(global-set-key (kbd "C-x C-n") 'next-buffer)
(global-set-key (kbd "C-x C-p") 'previous-buffer)

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
(load-theme 'dorsey t)

;; matching parenthesis
(show-paren-mode 1)

;; no bell
(setq ring-bell-function 'ignore)

;; font
(when (eq system-type 'gnu/linux)
  (set-default-font "DejaVu Sans Mono 10"))


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


;;
;; Performance (?)
;;

;; increase garbage collection threshold
(setq gc-cons-threshold 20000000)
