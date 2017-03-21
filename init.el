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
		      exec-path-from-shell
		      magit
		      paredit
		      smex
		      tagedit)))
  (dolist (p my/packages)
    (unless (package-installed-p p)
      (package-install p))))

;; exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))


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


;;
;; Functions
;;

;; load-file current-file
(defun my-load-current-file ()
  "Load current file."
  (interactive)
  (load-file (buffer-file-name)))
