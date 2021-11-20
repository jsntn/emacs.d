;;; init-font.el --- font settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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


(provide 'init-font)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-font.el ends here
