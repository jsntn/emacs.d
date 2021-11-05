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
  (define-key evil-normal-state-map (kbd "be") 'ibuffer)
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

(fset 'yes-or-no-p 'y-or-n-p) ;; use 'y/n' instead of 'yes/no'

(setq confirm-kill-emacs
      (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y"))) ;; prevent mis-operation


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
