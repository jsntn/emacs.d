;;; init-misc.el --- miscellaneous settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; misc config - yet to be placed in separate files

;; Org-roam does not resolve symbolic links. One can however instruct Emacs to
;; always resolve symlinks, at a performance cost:
(setq find-file-visit-truename t)

(defun my/open-init-file()
  "open init.el."
  (interactive)
  (find-file (symbol-value 'user-init-file)))

(save-place-mode 1)

(fset 'yes-or-no-p 'y-or-n-p) ; use 'y/n' instead of 'yes/no'

(setq confirm-kill-emacs
      ;; prevent mis-operation
      (lambda (prompt) (y-or-n-p-with-timeout "Whether to quit Emacs:" 10 "y")))

;; to prevent kill and yank commands from accessing the clipboard
(setq x-select-enable-clipboard nil)






(defun my-monitor-clipboard-and-write-to-file (output-file-path x-seconds)
  "Monitor system clipboard and write new content to the specified file."
  (defun my-clipboard-monitor-task ()
    (let ((current-clipboard
	   (or (x-get-selection 'CLIPBOARD) "")))
      (unless (equal current-clipboard my-clipboard-text)
	(setq my-clipboard-text current-clipboard)
	(with-temp-file output-file-path
	  (insert current-clipboard)))))

  (setq my-clipboard-text nil)
  (my-schedule-task-every-x-secs x-seconds 'my-clipboard-monitor-task))
;; Call the function with the desired output file path
;; (my-monitor-clipboard-and-write-to-file "c:/x-clipboard.txt" 1)

(defun my-monitor-kill-and-write-to-file (register-name output-file-path x-seconds)
  "Monitor the specified kill ring for changes and write its content to a specified file."
  (defun my-kill-monitor-task ()
    (condition-case nil
	(let ((current-contents (current-kill (get-register register-name))))
	  (unless (equal current-contents my-previous-kill-contents)
	    (setq my-previous-kill-contents current-contents)
	    (with-temp-file output-file-path
	      (insert current-contents))))
    (error)))

  (setq my-previous-kill-contents "")
  (my-schedule-task-every-x-secs x-seconds 'my-kill-monitor-task))
;; (current-kill 0)
;; (my-monitor-kill-and-write-to-file "c:/emacs-clipboard.txt" 1)


(defun my-remove-file-suffix (filename)
  "Remove the file suffix from FILENAME."
  (if (string-match "\\(.*\\)\\..*" filename)
      (match-string 1 filename)
    filename))
;; (my-remove-file-suffix "abc.txt")
;; (file-name-nondirectory "/temp/abc.txt")

(defun my-monitor-file-and-copy-to-register (file-path register-name x-seconds)
  "Monitor the specified file for changes and copy its content to a specified register."
  (let ((previous-contents-alist ()))

    (defun my-file-monitor-task ()
      (let* ((base-filename
	     (my-remove-file-suffix (file-name-nondirectory file-path)))
	    (current-contents (when (file-readable-p file-path)
				(with-temp-buffer
				  (insert-file-contents file-path)
				  (buffer-string))))
	    (previous-contents (assoc base-filename previous-contents-alist)))

	(unless (equal current-contents (cdr previous-contents))
	  (set-register register-name current-contents)
	  (setq previous-contents-alist
		(cons
		 (cons base-filename current-contents)
		 (delq
		  (assoc base-filename previous-contents-alist)
		  previous-contents-alist)
		 )))))

    (let ((task-name (concat "my-file-monitor-task_"
			     (my-remove-file-suffix
			      (file-name-nondirectory file-path)))))
      (fset (intern task-name) #'my-file-monitor-task)
      (my-schedule-task-every-x-secs x-seconds (intern task-name)))))
;; Example usage:
;; (my-monitor-file-and-copy-to-register "c:/x-clipboard.txt" ?a 1)
;; (my-monitor-file-and-copy-to-register "c:/emacs-clipboard.txt" ?b 1)
;; Testing:
;; (get-register ?a)
;; (get-register ?b)
;; (w32-set-clipboard-data "Your content goes here")

(defun my-monitor-file-and-copy-to-w32-clipboard (file-path x-seconds)
  "Monitor the specified file for changes and copy its content to Windows clipboard."
  (if *is-win*
      (let ((previous-contents-alist ()))

	(defun my-file-monitor-task ()
	  (let* ((base-filename
		  (my-remove-file-suffix (file-name-nondirectory file-path)))
		 (current-contents (when (file-readable-p file-path)
				     (with-temp-buffer
				       (insert-file-contents file-path)
				       (buffer-string))))
		 (previous-contents (assoc base-filename previous-contents-alist)))

	    (unless (equal current-contents (cdr previous-contents))
	      (w32-set-clipboard-data current-contents)
	      (setq previous-contents-alist
		    (cons
		     (cons base-filename current-contents)
		     (delq
		      (assoc base-filename previous-contents-alist)
		      previous-contents-alist)
		     )))))

	(let ((task-name (concat "my-file-monitor-task_"
				 (my-remove-file-suffix
				  (file-name-nondirectory file-path)))))
	  (fset (intern task-name) #'my-file-monitor-task)
	  (my-schedule-task-every-x-secs x-seconds (intern task-name))))
    (message "Only Windows system is supported.")))

;; (my-monitor-file-and-copy-to-w32-clipboard "c:/emacs-clipboard.txt" 1)





;; via https://emacs.stackexchange.com/questions/13080/reloading-directory-local-variables
(defun my/reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my/reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (equal default-directory dir))
	(my/reload-dir-locals-for-current-buffer)))))

(add-hook 'emacs-lisp-mode-hook
	  (defun enable-autoreload-for-dir-locals ()
	    (when (and (buffer-file-name)
		       (equal dir-locals-file
			      (file-name-nondirectory (buffer-file-name))))
	      (add-hook 'after-save-hook
			'my/reload-dir-locals-for-all-buffer-in-this-directory
			nil t))))

(defun eh-org-clean-space (text backend info)
  "remove the space between chinese characters during exporting
to HTML files."
  ;; https://github.com/hick/emacs-chinese#%E4%B8%AD%E6%96%87%E6%96%AD%E8%A1%8C
  (when (org-export-derived-backend-p backend 'html)
    (let ((regexp "[[:multibyte:]]")
	  (string text))
      ;; Org converts line-break with space by default, remove this as this is
      ;; not necessary for chinese characters
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












(provide 'init-misc)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-misc.el ends here
