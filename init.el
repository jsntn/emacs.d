;;; init.el --- A personal Emacs configuration       -*- lexical-binding: t; -*-

;; =============================================================================
;; hi@jsntn.com
;; 2020, 2021
;; =============================================================================

;;; Commentary:
;;

;;; Code:


(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "This Emacs configuration is based on v%s" minver))
  )


;; =============================================================================
;; variables settings
;; =============================================================================

;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715

(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)) ; define
									  ; site-lisp-dir

(setq custom-file (locate-user-emacs-file "custom.el"))


;; =============================================================================
;; load-path settings
;; =============================================================================

;; cl - Common Lisp Extension
(require 'cl-lib) ; https://emacs.stackexchange.com/questions/48109/require-cl-or-require-cl-lib

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; add both site-lisp and its immediate subdirs to `load-path'
(let ((symbol-value 'site-lisp-dir))
  (push site-lisp-dir load-path)
  (sanityinc/add-subdirs-to-load-path site-lisp-dir))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; =============================================================================
;; local variables settings
;; =============================================================================

(require 'local-var nil 'noerror) ; allow users to provide an optional
				  ; "local-var" containing personal variables


;; =============================================================================
;; local packages management
;; =============================================================================

(require 'local-packages nil 'noerror) ; allow users to provide an optional
				       ; "local-packages" containing local
				       ; packages
;; above must come before use-package settings, as it involves package.el which
;; downloads packages from the package-archives


;; =============================================================================
;; speed-up settings
;; =============================================================================

;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


;; =============================================================================
;; package management
;; =============================================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-enable-at-startup nil) ; disable automatic loading of installed
				     ; packages after the init file
(package-initialize)

;;; use-package initialization
;;; install use-package if not done
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;;; use-package for all others
(require 'use-package)

(setq use-package-always-ensure t) ; to install the package if it is not
				   ; installed

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package ace-jump-mode)

(use-package company
  :init
  (global-company-mode)
  :config
  ;; No delay in showing suggestions.
  (setq company-idle-delay 0)
  ;; Show suggestions after entering one character.
  (setq company-minimum-prefix-length 2)
  ;; When the list of suggestions is shown, and you go through the list of
  ;; suggestions and reach the end of the list, the end of the list of
  ;; suggestions does not wrap around to the top of the list again. This is a
  ;; minor inconvenience that can be solved:
  (setq company-selection-wrap-around t)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)
  )

(use-package counsel
  :defer 4
  )

(use-package doom-themes
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-org-config)
  ;; personal modified version of doom-monokai-classic
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
  (load-theme 'doom-monokai-classic t)
  (set-background-color "black")
  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
  )

(use-package evil
  :init
  (setq evil-want-integration t) ; this is optional since it's already set to t
				 ; by default.
  (setq evil-want-keybinding nil)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil)
    )
  :config
  (evil-mode 1)
  ;; change the cursor color in terms of evil mode
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (setq evil-undo-system 'undo-tree)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  :defer 4
  )

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  )

(use-package expand-region)

(use-package flycheck)

(use-package general)

(use-package helm
  :defer 2
  )

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  )

(use-package hl-todo
  :hook (
	 (prog-mode . hl-todo-mode)
	 (org-mode . hl-todo-mode)
	 )
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(
	  ("TODO" :foreground "white" :background "#5F87FF")
	  ("DONE" :foreground "white" :background "#2E8B57")
	  ("FIXME" :foreground "white" :background "red")
	  ("DEBUG" :foreground "#E6DB74" :background "black" :box t)
	  ("HACK" :foreground "#9C91E4" :background "black" :box t)
	  ("REVIEW" :foreground "#F02660" :background "black" :box t)
	  ("NOTE" :foreground "#C0C0C0" :background "#308014" :box t)
	  ("DEPRECATED" font-lock-doc-face :strike-through t :box t)
	  ("FOLLOWUP" :foreground "white" :background "#808A87" :box t)
	  ("ANSWER" :foreground "white" :background "#808A87" :box t)
	  ("MARK" :foreground "black" :background "#FFFFFF" :box t)
	  ("IMPROVEMENT" :foreground "white" :background "#FF9900" :box t)
	  ("ENHANCEMENT" :foreground "white" :background "#9900FF" :box t)
	  ("FEATURE" :foreground "white" :background "#38761d" :box t)
	  )
	)
  )

(use-package imenu-list)

(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  )

(use-package org-bullets
  :init
  (when (display-graphic-p)
  (setq org-bullets-bullet-list '("⬛" "○" "¶" "►"))
  )
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :defer 4
  )

(use-package ox-hugo
  :after ox
  )

(use-package swiper)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package elpa-mirror)

(use-package elpy
  :config
  (elpy-enable)
  ;; use flycheck instead of flymake
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(use-package vline
  :load-path (lambda () (symbol-value 'load-path))
  :config
  (set-face-background vline-face "#283639")
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

(use-package yaml-mode)


;; =============================================================================
;; add-hook settings
;; =============================================================================

(defun jsntn/hs-hide-all ()
  (hs-minor-mode 1)
  (hs-hide-all)
  (set (make-variable-buffer-local 'my-hs-hide-all) t)
  (set (make-variable-buffer-local 'my-hs-hide-block) t))

(dolist (hook '(
		prog-mode-hook
		sh-mode-hook
		lisp-mode-hook
		))
  (add-hook hook 'jsntn/hs-hide-all))

(dolist (hook '(
		prog-mode-hook
		text-mode-hook
		css-mode-hook
		lisp-mode-hook
		))
  (add-hook hook 'my/show-trailing-whitespace))

(dolist (hook '(
		css-mode-hook
		php-mode-hook
		html-mode-hook
		prog-mode-hook
		))
  (add-hook hook 'xah-syntax-color-hex))

(dolist (hook '(
		yaml-mode-hook
		python-mode-hook
		sh-mode-hook
		))
  (add-hook hook 'flycheck-mode))


(require 'init-display) ; display settings
(require 'init-font) ; font settings
(require 'init-keybindings) ; keybindings with general.el
(require 'init-org) ; Org-mode settings
(require 'init-sessions) ; session settings


;; =============================================================================
;; IBuffer mode settings
;; =============================================================================

(setq ibuffer-default-sorting-mode 'recency)

(defun ibuffer-jump-to-last-buffer ()
  (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
(add-hook 'ibuffer-hook #'ibuffer-jump-to-last-buffer)


;; =============================================================================
;; others settings
;; =============================================================================

(defun open-init-file()
  "open init.el."
  (interactive)
  (find-file (symbol-value 'user-init-file)))

(save-place-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ; use 'y/n' instead of 'yes/no'

(setq confirm-kill-emacs
      ;; prevent mis-operation
      (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y")))

;; to prevent kill and yank commands from accessing the clipboard
(setq x-select-enable-clipboard nil)

;; via https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (equal default-directory dir))
	(my-reload-dir-locals-for-current-buffer)))))

(add-hook 'emacs-lisp-mode-hook
	  (defun enable-autoreload-for-dir-locals ()
	    (when (and (buffer-file-name)
		       (equal dir-locals-file
			      (file-name-nondirectory (buffer-file-name))))
	      (add-hook 'after-save-hook
			'my-reload-dir-locals-for-all-buffer-in-this-directory
			nil t))))

(defun eh-org-clean-space (text backend info)
  "Remove the space between chinese characters during exporting to HTML files."
  ;; https://github.com/hick/emacs-chinese#%E4%B8%AD%E6%96%87%E6%96%AD%E8%A1%8C
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
	  (string text))
      ;; Org converts line-break with space by default, remove this as this is
      ;; not necessary for chinese characters
      (setq string
	    (replace-regexp-in-string
	     (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
	     "\\1\\2" string))
;;      ;; remove the space before the bold
;;      (setq string
;;	    (replace-regexp-in-string
;;	     (format "\\(%s\\) +\\(<\\)" regexp)
;;	     "\\1\\2" string))
;;      ;; remove the space after the bold
;;      (setq string
;;	    (replace-regexp-in-string
;;	     (format "\\(>\\) +\\(%s\\)" regexp)
;;	     "\\1\\2" string))
      string))
  )
(with-eval-after-load 'ox
  (add-to-list 'org-export-filter-paragraph-functions 'eh-org-clean-space)
  )


;; ===============================================================
;; programming settings: Shell
;; ===============================================================

(defun my/shell-mode-config ()
  (setq flycheck-select-checker "sh-shellcheck"))

(add-hook 'sh-mode-hook 'my/shell-mode-config)


;; ===============================================================
;; programming settings: YAML
;; ===============================================================

(defun my/yaml-mode-config ()
  (setq flycheck-select-checker "yaml-yamllint")
  (setq auto-mode-alist
	(append
	 '(("\\.yml\\'" . yaml-mode))
	 '(("\\.yaml\\'" . yaml-mode))
	 auto-mode-alist)))

(add-hook 'yaml-mode-hook 'my/yaml-mode-config)


;; ===============================================================
;; programming settings: Python
;; ===============================================================

(defun my/python-mode-config ()
  (setq python-indent-offset 4
	python-indent 4
	indent-tabs-mode nil
	default-tab-width 4
	flycheck-select-checker "python-flake8")
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode)))

(add-hook 'python-mode-hook 'my/python-mode-config)


;; =============================================================================
;; local configuration settings
;; =============================================================================

(require 'local-config nil 'noerror) ; allow users to provide an optional
				     ; "local-config" containing personal
				     ; settings


;; =============================================================================
;; footer
;; =============================================================================

(when (file-exists-p custom-file)
  ;; stop adding "custom" fields to the end
  ;; variables configured via the interactive 'customize' interface
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
