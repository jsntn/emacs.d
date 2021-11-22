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


(provide 'init-display)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-display.el ends here
