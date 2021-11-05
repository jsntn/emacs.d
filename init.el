;;; init.el --- A personal Emacs configuration       -*- lexical-binding: t; -*-

;; ===============================================================
;;                    hi@jsntn.com
;;                        2021
;; ===============================================================

;;; Commentary:
;;

;;; Code:

;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; ===============================================================
;; display settings
;; ===============================================================

(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)
(setq column-number-mode t) ;; turn on column numbers

;; wrap lines at 80 characters
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

;; disable splash screen and startup message
(setq inhibit-startup-message t)

;; disable the bars display
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)


;; ===============================================================
;; package management
;; ===============================================================

;;; use-package initialization
;;; install use-package if not done
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;;; use-package for all others
(require 'use-package)

(setq use-package-always-ensure t) ;; to install the package if it is not installed

(use-package evil
  :config
  (evil-mode 1)
  ;; change the cursor color in terms of evil mode
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (define-key evil-normal-state-map (kbd "ff") 'evil-scroll-page-down)
  (define-key evil-normal-state-map (kbd "bb") 'evil-scroll-page-up)
  )

(use-package helm
  :bind
  (("C-x C-b" . helm-buffers-list) ;; use helm to list buffers
   ("C-x C-r" . helm-recentf) ;; use helm to list recent files
   ("M-x" . helm-M-x)) ;; enhanced M-x command
  )

(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  )

(use-package swiper
  :bind
  ("C-s" . swiper) ;; quick keys to swiper
  ;; having own history variable allows to get more use of M-p, M-n and C-r.
  )

(use-package which-key
  :config
  ;; allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  (which-key-mode 1)
  (which-key-setup-side-window-right)
  )

(use-package window-numbering
  :config
  (window-numbering-mode)
  )

(use-package workgroups2)


;; ===============================================================
;; footer
;; ===============================================================
;; stop adding "custom" fields to the end
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; workgroups2 configuration
(workgroups-mode 1) ; put this one at the bottom of .emacs

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
