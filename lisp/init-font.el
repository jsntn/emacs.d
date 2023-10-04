;;; init-font.el --- font settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq inhibit-compacting-font-caches t) ; don't compact font caches during GC.

(use-package cnfonts
  :if window-system ; only load this package when in graphical Emacs
  :config
  (cnfonts-mode 1)
  (setq cnfonts-profiles
	'("program" "org-mode" "read-book"))
  (setq cnfonts-use-system-type t) ; save profile config across different system-type
  )


;; {{ START: display the emojis
;; reference,
;; https://github.com/doomemacs/doomemacs/issues/3298
;; https://www.reddit.com/r/emacs/comments/4v7tcj/does_emacs_have_a_hook_for_when_the_theme_changes/

;; (defvar after-load-theme-hook nil
;;   "Hook run after a color theme is loaded using `load-theme'.")
;; (defadvice load-theme (after run-after-load-theme-hook activate)
;;   "Run `after-load-theme-hook'."
;;   (run-hooks 'after-load-theme-hook))
;; ...
;; (add-hook 'after-load-theme-hook #'my-set-emoji-font)

;; for debugging,
;; (set-fontset-font t 'emoji nil)
;; :smile:
;; 😄

(defun my-emoji-can-display ()
  (if (char-displayable-p ?😄)
      t
    nil))

;; 2023/08/29 enable this and this needs further investigation...
;; FIXME: to be fixed (GitHub Actions Pipeline). See error below,
;; Debugger entered--Lisp error: (void-function set-fontset-font)
(defun my-set-emoji-font ()
  (if (functionp 'set-fontset-font)
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
	  ;; 2022-10-31 NotoColorEmoji uses the CBDT/CBLC color font format, which is
	  ;; supported by Android and Chrome/Chromium OS. Windows supports it starting
	  ;; with Windows 10 Anniversary Update in Chrome and Edge.
	  ;; Via https://github.com/googlefonts/noto-emoji/blob/f826707b28355f6cd1593f504427ca2b1f6c4c19/README.md#using-notocoloremoji
	  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
	  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
	  ((member "Symbola" (font-family-list)) "Symbola")
	  ((message "No emoji font found."))
	  )) ; http://xahlee.info/comp/unicode_font_download.html
	)
    (message "set-fontset-font is not available in current %s" emacs-version))
  ;; (remove-hook 'focus-in-hook #'my-set-emoji-font)
  )

;; (my-set-emoji-font)

;; https://www.reddit.com/r/emacs/comments/6lxf9b/question_emacsclient_and_connection_hooks/
;; (add-hook 'focus-in-hook #'my-set-emoji-font)

(defun my-advice-cnfonts-set-font (&rest _)
  "Advice function to set emoji font when cnfonts-mode is activated."
  (my-set-emoji-font))

;; Advising cnfonts-set-font to include setting emoji font
(advice-add 'cnfonts-set-font :after 'my-advice-cnfonts-set-font)

(when (display-graphic-p)
  (my-check-for-font "Symbola" "Symbola font is not installed, however, it is recommended to install for proper emoji display. Press ENTER to continue."))
;; END: display the emojis }}



(provide 'init-font)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-font.el ends here
