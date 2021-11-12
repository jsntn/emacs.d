;;; init.el --- A personal Emacs configuration       -*- lexical-binding: t; -*-

;; ===============================================================
;; hi@jsntn.com
;; 2020, 2021
;; ===============================================================

;;; Commentary:
;;

;;; Code:


;; ===============================================================
;; variables settings
;; ===============================================================

;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715

(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


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

(use-package ace-jump-mode
  :bind
  ("C-c f" . ace-jump-char-mode)
  )

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-org-config)
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory)) ;; personal modified version of doom-monokai-classic
  (load-theme 'doom-monokai-classic t)
  (set-background-color "black")
  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
  )

(use-package company
  :hook (after-init . global-company-mode)
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

(use-package evil
  :init
  (setq evil-want-integration t) ;; this is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
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
  (define-key evil-normal-state-map (kbd "be") 'ibuffer)
  (evil-set-undo-system 'undo-tree)
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  :config
  (evil-leader/set-leader ",")
  (evil-leader/set-key
    "b" 'bookmark-bmenu-list
    "d" 'dired
    "f" 'ace-jump-char-mode
    "o" 'helm-imenu
    "r" 'revert-buffer
    "w" 'windows-split-toggle
    )
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  )

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region)
  (global-set-key (kbd "C--") 'er/contract-region)
  )

(use-package helm
  :bind
  (("C-x C-b" . helm-buffers-list) ;; use helm to list buffers
   ("C-x C-r" . helm-recentf) ;; use helm to list recent files
   ("M-x" . helm-M-x)) ;; enhanced M-x command
  )

(use-package imenu-list
  :bind (("C-'" . imenu-list-smart-toggle)
	 :map imenu-list-major-mode-map
	 ("g" . evil-goto-first-line)
	 ("G" . evil-goto-line)
	 ("j" . evil-next-line)
	 ("k" . evil-previous-line))
  )

(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  (global-set-key [f8] 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
  (evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
  (evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
  (evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)
  )

(use-package org-bullets
  :init
  (setq org-bullets-bullet-list
	'("⬛" "○" "¶" "►"))
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package ox-hugo
  :after ox
  )

(use-package swiper
  :bind
  ("C-s" . swiper) ;; quick keys to swiper
  ;; having own history variable allows to get more use of M-p, M-n and C-r.
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package vline
  :load-path (lambda () (symbol-value 'site-lisp-dir))
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


;; ===============================================================
;; display settings
;; ===============================================================

(add-hook 'emacs-lisp-mode-hook 'show-paren-mode) ;; highlight matching parenthesis
(global-hl-line-mode 1) ;; highlight current line

(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)
(setq column-number-mode t) ;; turn on column numbers

;; wrap lines at 80 characters
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

;; disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; disable the bars display
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

(defun windows-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
		    #'split-window-vertically
		  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
	(other-window 1)
	(switch-to-buffer (other-buffer)))))
  )

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background
		      (let* (
			     (ms (match-string-no-properties 0))
			     (r (substring ms 1 2))
			     (g (substring ms 2 3))
			     (b (substring ms 3 4)))
			(concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush)
  )

(add-hook 'css-mode-hook 'xah-syntax-color-hex)
(add-hook 'php-mode-hook 'xah-syntax-color-hex)
(add-hook 'html-mode-hook 'xah-syntax-color-hex)
(add-hook 'prog-mode-hook 'xah-syntax-color-hex)

;; visualize trailing whitespace
(defun my-show-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  )
(dolist (hook '(prog-mode-hook text-mode-hook css-mode-hook lisp-mode-hook))
  (add-hook hook 'my-show-trailing-whitespace))

(global-set-key (kbd "C-M-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-M-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-M-<down>") 'shrink-window)
(global-set-key (kbd "C-M-<up>") 'enlarge-window)


;; ===============================================================
;; font settings
;; ===============================================================

(setq inhibit-compacting-font-caches t) ; don't compact font caches during GC.

(defvar emacs-english-font nil
  "The font name of English.")

(defvar emacs-cjk-font nil
  "The font name for CJK.")

(defvar emacs-font-size-pair nil
  "Default font size pair for (english . chinese).")

(defvar emacs-font-size-pair-list nil
  "This list is used to store matching (english . chinese) font-size.")

(defun font-exist-p (fontname)
  "Test if this FONTNAME is exist or not."
  (if (or (not fontname) (string= fontname ""))
      nil
    (if (not (x-list-fonts fontname))
	nil t)))

(defun set-font (english chinese size-pair)
  "Setup Emacs ENGLISH and CHINESE font SIZE-PAIR on x 'window-system'."
  (if (font-exist-p english)
      (set-frame-font (format "%s:pixelsize=%d" english (car size-pair)) t)
    )

  (if (font-exist-p chinese)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
	(set-fontset-font (frame-parameter nil 'font) charset
			  (font-spec :family chinese :size (cdr size-pair))))
    )
  )

(defun emacs-step-font-size (step)
  "Increase/Decrease Emacs's font STEP size."
  (let ((scale-steps emacs-font-size-pair-list))
    (if (< step 0) (setq scale-steps (reverse scale-steps)))
    (setq emacs-font-size-pair
	  (or (cadr (member emacs-font-size-pair scale-steps))
	      emacs-font-size-pair))
    (when emacs-font-size-pair
      (message "emacs font size set to %.1f" (car emacs-font-size-pair))
      (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))))

(defun increase-emacs-font-size ()
  "Decrease Emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size 1))

(defun decrease-emacs-font-size ()
  "Increase Emacs's font-size acording emacs-font-size-pair-list."
  (interactive) (emacs-step-font-size -1))

(setq list-faces-sample-text
      (concat
       "ABCDEFTHIJKLMNOPQRSTUVWXYZ abcdefghijklmnopqrstuvwxyz\n"
       "11223344556677889900       壹貳參肆伍陸柒捌玖零"))

(when window-system
  ;; setup change size font, base on emacs-font-size-pair-list
  (global-set-key (kbd "C-M-=") 'increase-emacs-font-size)
  (global-set-key (kbd "C-M--") 'decrease-emacs-font-size)

  ;; setup default english font and cjk font
  (setq emacs-english-font "Source Code Pro Semibold")
  (setq emacs-cjk-font "等距更纱黑体 SC")
  (setq emacs-font-size-pair '(20 . 22))
  (setq emacs-font-size-pair-list '(( 10 . 12) (12 . 14)
				    (14 . 16) (16 . 18) (18 . 20)
				    (20 . 22) (22 . 24) (24 . 26)
				    (26 . 28) (28 . 30) (30 . 32)
				    (32 . 34) (34 . 36) (36 . 38)))
  ;; Setup font size based on emacs-font-size-pair
  (set-font emacs-english-font emacs-cjk-font emacs-font-size-pair))


;; ===============================================================
;; session settings
;; ===============================================================

;; save and restore editor sessions between restarts

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600
      desktop-save t
      )
(desktop-save-mode 1)

(add-hook 'emacs-startup-hook 'desktop-read)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))


;; ===============================================================
;; IBuffer mode settings
;; ===============================================================

(setq ibuffer-default-sorting-mode 'recency)

(defun ibuffer-jump-to-last-buffer ()
  (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
(add-hook 'ibuffer-hook #'ibuffer-jump-to-last-buffer)


;; ===============================================================
;; Org-mode settings
;; ===============================================================

;; enable org-indent mode
(setq org-startup-indented t)


;; ===============================================================
;; others settings
;; ===============================================================

(save-place-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ;; use 'y/n' instead of 'yes/no'

(setq confirm-kill-emacs
      (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y"))) ;; prevent mis-operation

;; enabling control-c and control-v to copy and paste in Emacs
(define-key evil-visual-state-map (kbd "C-c") 'kill-ring-save)
(define-key evil-insert-state-map (kbd "C-c") 'kill-ring-save)
(define-key evil-insert-state-map (kbd "C-v") 'yank)
(define-key evil-visual-state-map (kbd "C-x") 'kill-region)
(define-key evil-insert-state-map (kbd "C-x") 'kill-region)
(define-key evil-insert-state-map (kbd "C-z") 'undo-fu-only-undo)
(define-key evil-insert-state-map (kbd "C-y") 'undo-fu-only-redo)

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


;; ===============================================================
;; footer
;; ===============================================================

;; stop adding "custom" fields to the end
;; variables configured via the interactive 'customize' interface
(when (file-exists-p custom-file)
  (load custom-file))

(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
