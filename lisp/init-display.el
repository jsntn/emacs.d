;;; init-display.el --- display settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(add-hook 'emacs-lisp-mode-hook 'show-paren-mode) ; highlight matching
						  ; parenthesis
(global-hl-line-mode 1) ; highlight current line

(setq display-line-numbers-width-start t)
(global-display-line-numbers-mode 1)
(setq column-number-mode t) ; turn on column numbers

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

;; recent files
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs/50422#50422
(recentf-mode 1)

(progn
  ;; set font for emoji (if before emacs 28, should come after setting
  ;; symbols. emacs 28 now has 'emoji . before, emoji is part of 'symbol)
  ;; http://xahlee.info/emacs/emacs/emacs_set_font_emoji.html
  (set-fontset-font
   t
   (if (version< emacs-version "28.1")
       '(#x1f300 . #x1fad0)
     'emoji
     )
   (cond
    ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji")
    ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
    ;;<2022-10-31 NotoColorEmoji uses the CBDT/CBLC color font format, which is
    ;; supported by Android and Chrome/Chromium OS. Windows supports it starting
    ;; with Windows 10 Anniversary Update in Chrome and Edge.
    ;; Via https://github.com/googlefonts/noto-emoji/blob/f826707b28355f6cd1593f504427ca2b1f6c4c19/README.md#using-notocoloremoji
    ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
    ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
    ((member "Symbola" (font-family-list)) "Symbola"))) ; http://xahlee.info/comp/unicode_font_download.html
  )

(when (display-graphic-p)
  (unless (member "Symbola" (font-family-list))
    (yes-or-no-p "Symbola font is not installed, however, it is recommended to install for proper emoji display. Continue?")
    )
  )

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

;; FIXME: to be tested...
;; visualize trailing whitespace
(defun my/show-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  )

;; codes folding
(load-library "hideshow")
;; refer to https://sachachua.com/blog/2006/10/emacs-hideshow/
(defun jsntn/toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (set (make-variable-buffer-local 'my-hs-hide-all) (not my-hs-hide-all))
  (if my-hs-hide-all
      (hs-hide-all)
    (hs-show-all)))
(defun jsntn/toggle-hideshow-block ()
  "Toggle hideshow block."
  (interactive)
  (set (make-variable-buffer-local 'my-hs-hide-block) (not my-hs-hide-block))
  (if my-hs-hide-block
      (hs-hide-block)
    (hs-show-block)))

;; { -- START --
;; default inline image background in Org-mode
;; https://emacs.stackexchange.com/a/37927/29715
;; note: restart Emacs to make this change effective
(defcustom org-inline-image-background nil
  "The color used as the default background for inline images.
When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(defun create-image-with-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
	 (type (cadr args))
	 (data-p (caddr args))
	 (props (cdddr args)))
    ;; Get this return result style from `create-image'.
    (append (list file type data-p)
	    (list :background "white")
	    props)))

(advice-add 'create-image :filter-args
	    #'create-image-with-background-color)
;; -- END -- }


(provide 'init-display)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-display.el ends here
