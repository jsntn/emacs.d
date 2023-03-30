;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun my/org-repeated-deadline (arg &optional time repeater)
  (interactive "P")
  (unless arg
    (setq repeater (read-string "Input the repeater: ")))
  ;;  +1m -> the date shift is always exactly 1 month
  ;; .+1m -> repeat in 1 month from the last time I marked it done
  ;; ++1m -> repeat in at least 1 month from the last time I marked it done, and
  ;; keep it on the same day of the week move the due date into the future by
  ;; increments of month.
  (let ((org-time-stamp-formats
	 (if repeater
	     `(,(concat "<%Y-%m-%d %a " repeater ">") .
	       ,(concat "<%Y-%m-%d %a %H:%M " repeater ">"))
	   org-time-stamp-formats)))
    (org-deadline arg time)))

(defun my/org-repeated-schedule (arg &optional time repeater)
  (interactive "P")
  (unless arg
    (setq repeater (read-string "Input the repeater: ")))
  (let ((org-time-stamp-formats
	 (if repeater
	     `(,(concat "<%Y-%m-%d %a " repeater ">") .
	       ,(concat "<%Y-%m-%d %a %H:%M " repeater ">"))
	   org-time-stamp-formats)))
    (org-schedule arg time)))

;; https://stackoverflow.com/a/10628109/4274775
;; keybinding: =C-k= -> [[./init-keybindings.el::my-dpap]]
;; <2023-03-23 Thu 10:22> In Emacs 25, you can do what you'd expect: in the
;; process list, hit d(M-x process-menu-delete-process) to "delete" the process
;; under point(D is binded in evil-mode). See
;; https://stackoverflow.com/a/31538514/4274775
(defun my/delete-process-at-point ()
  (interactive)
  (let ((process (get-text-property (point) 'tabulated-list-id)))
    (cond ((and process
                (processp process))
           (delete-process process)
           (revert-buffer))
          (t
           (error "no process at point!")))))

;; https://stackoverflow.com/a/7043786
(defun my/sudo-find-file (file-name)
  "like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;; { START: undo fill-paragraph
;; https://stackoverflow.com/a/2478549
(defun my/unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun my/unfill-region ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-region (region-beginning) (region-end) nil)))
;; END: undo fill-paragraph

(defun my/org-insert-src-block (src-code-type)
  "insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
	  '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
	    "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
	    "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
	    "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
	    "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code))
  )

(defun my/hide-dos-eol ()
  "do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (aset buffer-display-table ?\^M []))

(defun my/remove-dos-eol ()
  "replace DOS eolns CR LF with Unix eolns CR."
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))


(provide 'init-utils)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-utils.el ends here
