;;; init-misc.el --- miscellaneous settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; misc config - yet to be placed in separate files

(defun open-init-file()
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

(defun my/create-TAGS-with-absolute-paths-inside (dir-name)
  "create TAGS file with absolute paths recorded inside."
  (interactive "DDirectory: ")
  (shell-command
   (format "ctags -e -R --tag-relative=never %s" (directory-file-name dir-name)))
  )

;; { START: config for counsel-etags and company-ctags
;; <<config-ce-cc>>
(defun my/find-tags-file ()
  "recursively searches each parent directory for a file named 'TAGS' and returns the
path to that file or nil if a tags file is not found. Returns nil if the buffer is
not visiting a file"
  (progn
    (defun find-tags-file-r (path)
      "find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
	     (possible-tags-file (concat parent "TAGS")))
	(cond
	 ((file-exists-p possible-tags-file) (throw 'found-it possible-tags-file))
	 ((string= "/TAGS" possible-tags-file) (error "no tags file found"))
	 (t (find-tags-file-r (directory-file-name parent))))))

    (if (buffer-file-name)
	(catch 'found-it
	  (find-tags-file-r (buffer-file-name)))
      (error "buffer is not visiting a file"))))

(defun insert-into-my-tags-table-list()
  ;; if `my-tags-table-list' is void, then set it to empty list
  (unless (boundp 'my-tags-table-list)
    (setq my-tags-table-list '()))
  (setq existing-my-tags-table-list my-tags-table-list)
  (setq my-tags-table-list '()) ; initiate empty list
  (setq my-tags-table-list
	(delq nil (delete-dups ; delete nil and duplicates
		   (cons (my/find-tags-file) (symbol-value 'existing-my-tags-table-list)))))
  (setq counsel-etags-extra-tags-files my-tags-table-list)
  (setq company-ctags-extra-tags-files my-tags-table-list)
  (message "tags-table list for counsel-etags/company-ctags:\n%s" my-tags-table-list)
  )
(defun delete-from-my-tags-table-list ()
  (setq my-tags-table-list (delete (my/find-tags-file) my-tags-table-list))
  (setq counsel-etags-extra-tags-files my-tags-table-list)
  (setq company-ctags-extra-tags-files my-tags-table-list)
  (message "tags-table list for counsel-etags/company-ctags:\n%s" my-tags-table-list)
  )

;; keybinding -> [[./init-keybindings.el::m-ftf]]
(defun my/set-tags-table-list (&optional del)
  "calls `my/find-tags-file' to recursively search up the directory
tree to find a file named 'TAGS'. If found, add/delete(C-u) it
to/from 'counsel-etags-extra-tags-files' and
'company-ctags-extra-tags-files'."
  (interactive "P")
  (if del (delete-from-my-tags-table-list)
    (insert-into-my-tags-table-list))
  )
;; END: config for counsel-etags and company-ctags }

(defun my/tags-table-list ()
  "check and display my tags-table list through message."
  (interactive)
  (message "tags-table list for counsel-etags/company-ctags:\n%s" my-tags-table-list)
  )


(provide 'init-misc)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-misc.el ends here
