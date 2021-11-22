;;; init-org.el --- Org-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq org-startup-indented t) ; enable org-indent mode

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


(provide 'init-org)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-org.el ends here
