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

;; define site-lisp-dir
(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory))

;; add site-lisp-dir to load-path
(add-to-list 'load-path (symbol-value 'site-lisp-dir))

(setq custom-file (locate-user-emacs-file "custom.el"))


;; ===============================================================
;; local variables settings
;; ===============================================================

;; allow users to provide an optional "local-var" containing personal variables
(require 'local-var nil 'noerror)


;; ===============================================================
;; package management
;; ===============================================================

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;;; use-package initialization
;;; install use-package if not done
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
;;; use-package for all others
(require 'use-package)

(setq use-package-always-ensure t) ;; to install the package if it is not installed

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

(use-package counsel)

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

(use-package evil
  :init
  (setq evil-want-integration t) ;; this is optional since it's already set to t by default.
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

(use-package general)

(use-package helm)

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
  )

(use-package ox-hugo
  :after ox
  )

(use-package swiper)

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

(when (display-graphic-p)
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
;; keys settings
;; (https://github.com/noctuid/general.el)
;; ===============================================================

;; * Global Keybindings
;; `general-define-key' acts like `global-set-key' when :keymaps is not
;; specified (because ":keymaps 'global" is the default)
;; kbd is not necessary and arbitrary amount of key def pairs are allowed
(general-define-key
 "M-x" 'helm-M-x
 "C-s" 'swiper ;; having own history variable allows to get more use of M-p, M-n
	       ;; and C-r.
 "C-=" 'er/expand-region
 "C--" 'er/contract-region
 "C-'" 'imenu-list-smart-toggle
 "C-M-<left>" 'shrink-window-horizontally
 "C-M-<right>" 'enlarge-window-horizontally
 "C-M-<down>" 'shrink-window
 "C-M-<up>" 'enlarge-window
 ;; ...
 )
;; `general-def' can be used instead for `define-key'-like syntax
(general-def
 "<f8>" 'neotree-toggle
 ;; ...
 )

(when (display-graphic-p)
  (general-define-key
   ;; setup change size font, base on emacs-font-size-pair-list
   "C-M-=" 'increase-emacs-font-size
   "C-M--" 'decrease-emacs-font-size
   )
  )

;; * Mode Keybindings
;; `general-define-key' is comparable to `define-key' when :keymaps is specified
(general-define-key
 ;; NOTE: keymaps specified with :keymaps must be quoted
 :keymaps 'imenu-list-major-mode-map
 "g" 'evil-goto-first-line
 "G" 'evil-goto-line
 "j" 'evil-next-line
 "k" 'evil-previous-line
 )

(general-define-key
 ;; enabling control-c and control-v to copy and paste in Emacs
 :states '(insert visual)
 "C-c" 'kill-ring-save
 "C-v" 'yank
 "C-z" 'undo-fu-only-undo
 "C-y" 'undo-fu-only-redo
 )

(general-define-key
 :states '(normal visual)
 "ff" 'evil-scroll-page-down
 "bb" 'evil-scroll-page-up
 "be" 'ibuffer
 ;; ...
 )

(general-define-key
 :states 'normal
 :keymaps 'neotree-mode-map
 "TAB" 'neotree-enter
 "SPC" 'neotree-quick-look
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "g" 'neotree-refresh
 "n" 'neotree-next-line
 "p" 'neotree-previous-line
 "A" 'neotree-stretch-toggle
 "H" 'neotree-hidden-file-toggle
 ;; ...
 )

(unless (display-graphic-p)
  (general-define-key
   :states 'normal
   "M-i" 'evil-jump-forward
   "M-o" 'evil-jump-backward
   ;; ...
   )
  )

;; * Prefix Keybindings
;; :prefix can be used to prevent redundant specification of prefix keys
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture
 "f" 'ace-jump-char-mode
 ;; ...
 )

(general-define-key
 ;; for org-mode-map
 :prefix "C-c"
 :keymaps 'org-mode-map
 "C-q" 'counsel-org-tag
 "o" 'org-open-maybe ;; redefine file opening without clobbering universal
		     ;; argumnet ...
 ;; ...
 )

(general-define-key
 :prefix "C-x"
 "C-r" 'helm-recentf
 ;; ...
 )

(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix ",")

(my-leader-def
 :keymaps 'normal
 "b" 'bookmark-bmenu-list
 "d" 'dired
 "f" 'ace-jump-char-mode
 "o" 'helm-imenu
 "r" 'revert-buffer
 "w" 'windows-split-toggle
 ;; ...
 )


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

(defun org-force-open-current-window ()
  ;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
  (interactive)
  (let ((org-link-frame-setup (quote
			       ((vm . vm-visit-folder)
				(vm-imap . vm-visit-imap-folder)
				(gnus . gnus)
				(file . find-file)
				(wl . wl)))
			      ))
    (org-open-at-point)))
;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg)
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)
    )
  )

(setq org-todo-keywords
      ;; '((sequence "☛ TODO(t)" "➼ IN-PROGRESS" "⚑ WAIT(w@/!)" "|" "✔ DONE(d!)" "✘ CANCELED(c@)")
      '((sequence "TODO(t)" "IN-PROGRESS" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "IMPROVEMENT(m)" "ENHANCEMENT(e)" "FEATURE(a)" "|" "FIXED(f)")
	))

(setf org-todo-keyword-faces '(
			       ("CANCELED" . (:foreground "white" :background "#95A5A6"))
			       ("DONE" . (:foreground "white" :background "#2E8B57"))
			       ("WAIT" . (:foreground "white" :background "#F9BC41"))
			       ("IN-PROGRESS" . (:foreground "white" :background "#3498DB"))
			       ("TODO" . (:foreground "white" :background "#CD5C5C"))
			       ("REPORT" (:foreground "#C0C0C0" :background "#308014" :box t))
			       ("BUG" (:foreground "#E6DB74" :background "black" :box t))
			       ("KNOWNCAUSE" (:foreground "#9C91E4" :background "black" :box t))
			       ("IMPROVEMENT" (:foreground "#FF9900" :background "black" :box t))
			       ("ENHANCEMENT" (:foreground "#9900ff" :background "black" :box t))
			       ("FEATURE" (:foreground "#38761d" :background "black" :box t))
			       ("FIXED" (:foreground "#4B5556" :strike-through t :box t))
			       ))

(defun my/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
		      :strike-through t
		      :foreground "white")
  )
;; https://emacs.stackexchange.com/questions/10595/how-to-strike-out-done-items-in-org-mode
(eval-after-load "org"
  (add-hook 'org-add-hook 'my/modify-org-done-face))


;; ===============================================================
;; others settings
;; ===============================================================

(defun open-init-file()
  "open init.el."
  (interactive)
  (find-file (symbol-value 'user-init-file)))

(save-place-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ;; use 'y/n' instead of 'yes/no'

(setq confirm-kill-emacs
      (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y"))) ;; prevent mis-operation

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
      ;; Org converts line-break with space by default, remove this as this is not necessary for chinese characters
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
;; local configuration settings
;; ===============================================================

;; allow users to provide an optional "local-config" containing personal settings
(require 'local-config nil 'noerror)


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
