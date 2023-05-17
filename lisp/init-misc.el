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

(defun my/list-packages-and-versions ()
  (interactive)
  (package-initialize)
  (let ((pkgs (mapcar 'car package-alist)))
    (dolist (pkg pkgs)
      (message "%s - %s"
	       pkg (package-desc-version (cadr (assq pkg package-alist)))))))

(defun my/copy-org-id-at-point ()
  "Copy the ID property of the heading at point to the kill-ring."
  (interactive)
  (let ((id (org-entry-get nil "ID")))
    (when id
      (kill-new id)
      (message "Copied ID: %s" id))))

(defun my-get-heading-from-org-id-db (org-id)
  "Retrieve the heading title associated with an Org ID from the
current buffer's Org mode database."
  (org-with-point-at (org-id-find org-id 'marker)
    (org-get-heading)))

(defun my/insert-org-id-from-kill-ring ()
  "Insert a link to an Org ID from the kill-ring with a user-defined description.
The user is prompted to enter a description for the link.

If description is empty, retrieve the heading from the org-id
database using `my-get-heading-from-org-id-db` function."
  (interactive)
  (let ((id (current-kill 0)))
    (when id
      (let* ((org-id (replace-regexp-in-string "^id:" "" id))
             (description (read-string "Description: " nil 'my-history)))
        (if (string-empty-p description)
            (setq description (my-get-heading-from-org-id-db org-id)))
        (org-insert-link nil (concat "id:" org-id) description)))))

(defun my-parse-link-id (link)
  "Parse the ID from an org-mode link of the form `id:xxxxxxxxxxxx'."
  (when (string-match "id:\\(.+\\)" link)
    (match-string 1 link)))

(defun my/org-link-goto-at-point ()
  "Check if link at point is a file link or an ID link, and jump to
the appropriate location."
  (interactive)
  (if-let ((link (org-element-property :raw-link (org-element-context))))
      (cond ((string-prefix-p "file:" link)
             (org-open-at-point))
            ((string-prefix-p "id:" link)
	     (org-id-goto (my-parse-link-id link))))
    (message "No link at point.")))

(defun my/revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  ;; via https://emacs.stackexchange.com/a/24461/29715
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
		 (not (buffer-modified-p buf)))
	(if (file-readable-p filename)
	    ;; If the file exists and is readable, revert the buffer.
	    (with-current-buffer buf
	      (revert-buffer :ignore-auto :noconfirm :preserve-modes))
	  ;; Otherwise, kill the buffer.
	  (let (kill-buffer-query-functions) ; No query done when killing buffer
	    (kill-buffer buf)
	    (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

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

(defun my/create-TAGS (&optional sudo dir-name tag-relative)
  "Create a TAGS file with absolute or relative paths recorded inside. With a
prefix argument SUDO, run the command with sudo privilege. With a prefix
argument TAG-RELATIVE, create the TAGS file with relative paths recorded inside.

When called interactively, prompt the user for the directory name to create the
TAGS file. If no input is given, use the current working directory.

The `ctags` command is executed with the `--tag-relative` option set to `yes` if
the `tag-relative` prefix argument is set to 'y', or 'never' otherwise. The `*`
wildcard is included in the `ctags` command to create TAGS for all files in the
directory.

Example usage:
  - To create a TAGS file for the current directory:
      M-x my/create-TAGS RET RET
  - To create a TAGS file for a specific directory with relative paths recorded:
      M-x my/create-TAGS RET /path/to/directory RET y RET
  - To create a TAGS file for a specific directory with absolute paths recorded,
    using sudo privilege:
      C-u M-x my/create-TAGS RET /path/to/directory RET RET"

  ;; This function is improved by ChatGPT and Claude :)
  (interactive "P\nDEnter the directory to create TAGS file: \nMCreate TAGS file with relative paths (y/n):")

  (let* ((target-dir (if (string= "" dir-name)
			 default-directory
		       (expand-file-name dir-name)))
	 (tags-path (if (string= tag-relative 'y)
			nil
		      (read-file-name "Enter the path for TAGS file: ")))
	 (ctags-cmd (format "cd %s && ctags --options=%s -e -R --tag-relative=%s -f %s *"
			    target-dir
			    (expand-file-name ".ctags" user-emacs-directory)
			    (if (string-equal tag-relative 'y) "yes" "never")
			    (or tags-path (expand-file-name "TAGS" target-dir)))))
    (let ((command (if sudo
		       (concat "sudo sh -c '"
			       ctags-cmd
			       "'")
		     ctags-cmd)))
      (start-process-shell-command "create TAGS" nil command))))

(defun my/find-tags-file ()
  "recursively searches each parent directory for a file named
'TAGS' and returns the path to that file or nil if a tags file is
not found. Returns nil if the buffer is not visiting a file"
  (progn
    (defun find-tags-file-r (path)
      "find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
	     (possible-tags-file (concat parent "TAGS")))
	(cond
	 ((file-exists-p possible-tags-file)
	  (throw 'found-it possible-tags-file))
	 ((string= "/TAGS" possible-tags-file)
	  (error "no tags file found"))
	 (t (find-tags-file-r (directory-file-name parent))))))

    (if (buffer-file-name)
	(catch 'found-it
	  (find-tags-file-r (buffer-file-name)))
      (error "buffer is not visiting a file"))))

(defun my/file ()
  "prompt user to enter a file name, with completion and history
support."
  ;; http://xahlee.info/emacs/emacs/elisp_idioms_prompting_input.html
  (interactive)
  (setq my-file-value (read-file-name "Input file name: "))
  (message "my-file-value is %s" my-file-value)
  )

;; { START: config for counsel-etags and company-ctags
;; <<config-ce-cc>>
(defun my-tags-file (&optional select)
  "If SELECT is non-nil, set the value of `my-tags-file` to the user-selected file path
after prompting for it through `my/file`.
Otherwise, set `my-tags-file` to the value returned by `my/find-tags-file`.
-- generated by ChatGPT :)"
  (if select
      (progn (my/file)
	     (setq my-tags-file my-file-value))
    (setq my-tags-file (my/find-tags-file)))
  )

(defun my-set-extra-tags-files (my-tags-table-list)
  (setq counsel-etags-extra-tags-files my-tags-table-list)
  (setq company-ctags-extra-tags-files my-tags-table-list)
  (message "tags-table list for counsel-etags/company-ctags:\n%s"
	   my-tags-table-list)
  )

(defun my/insert-into-my-tags-table-list(&optional select)
  "automatically insert the TAGS file or select TAGS file to
insert(C-u), into `my-tags-table-list',
`counsel-etags-extra-tags-files' and
`company-ctags-extra-tags-files'."
  (interactive "P")
  (unless (boundp 'my-tags-table-list)
    ;; if `my-tags-table-list' is void, then set it to empty list
    (setq my-tags-table-list '()))
  (setq existing-my-tags-table-list my-tags-table-list)
  (setq my-tags-table-list '()) ; initiate empty list
  (my-tags-file select)
  (setq my-tags-table-list
	(delq nil (delete-dups ; delete nil and duplicates
		   (cons (symbol-value 'my-tags-file)
			 (symbol-value 'existing-my-tags-table-list)))))
  (my-set-extra-tags-files my-tags-table-list)
  )

(defun my/delete-from-my-tags-table-list (&optional select)
  "automatically delete the TAGS file or select TAGS file to
delete(C-u), from `my-tags-table-list',
`counsel-etags-extra-tags-files' and
`company-ctags-extra-tags-files'."
  (interactive "P")
  (my-tags-file select)
  (setq my-tags-table-list
	(delete (symbol-value 'my-tags-file) my-tags-table-list))
  (my-set-extra-tags-files my-tags-table-list)
  )

;; keybinding -> [[./init-keybindings.el::m-ftf]]
(defun my/set-tags-table-list (&optional del)
  "calls `my/find-tags-file' to recursively search up the directory
tree to find a file named 'TAGS'. If found, add/delete(C-u) it
to/from 'counsel-etags-extra-tags-files' and
'company-ctags-extra-tags-files'."
  (interactive "P")
  (if del (my/delete-from-my-tags-table-list)
    (my/insert-into-my-tags-table-list))
  )

(defun my/tags-table-list ()
  "check and display my tags-table list through message."
  (interactive)
  (message "tags-table list for counsel-etags/company-ctags:\n%s"
	   my-tags-table-list)
  )
;; END: config for counsel-etags and company-ctags }


(provide 'init-misc)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-misc.el ends here
