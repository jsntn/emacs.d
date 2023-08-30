;;; init-dict.el --- dict settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; {{ START: Look up text at-point/marked by GoldenDict in Emacs
;; https://github.com/jsntn/goldendict-emacs/tree/my
(defun my-look-up-dict (word)
  (let ((goldendict-executable (executable-find "goldendict")))
    (if goldendict-executable
        (start-process "goldendict" nil goldendict-executable word)
      (message "GoldenDict executable not found. Please make sure it is installed and in your PATH."))))

(defun my/look-up-dict ()
  "Look up text at-point/marked by GoldenDict in Emacs.

Version 2023-08-03"
  (interactive)
  (let ((word ""))
    (if (and (bound-and-true-p mark-active) (not (equal (point) (mark))))
        (setq word (buffer-substring (region-beginning) (region-end)))
      (setq word (thing-at-point 'word)))
    (if (not (string-blank-p word))
        (my-look-up-dict word)
      (message "No word found at point or in the marked region."))))

(global-set-key (kbd "C-c g") 'my/look-up-dict)
;; END }}




(provide 'init-dict)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-dict.el ends here