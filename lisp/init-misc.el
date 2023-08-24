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

(defun my/review-random-function ()
  "Review a random function defined in my Emacs configuration."
  (interactive)
  (let* ((config-functions '())
         (config-files (directory-files-recursively user-emacs-directory "\\.el$")))
    (dolist (file config-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(defun \\([^ ]+\\)" nil t)
          (push (match-string 1) config-functions))))
    (let* ((command (nth (random (length config-functions)) config-functions)))
      (describe-function (intern command)))))

(defun my/review-random-my-function ()
  "Review a random function that starts with 'my/' in my Emacs configuration."
  (interactive)
  (let* ((config-functions '())
         (config-files (directory-files-recursively user-emacs-directory "\\.el$")))
    (dolist (file config-files)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "(defun my/\\([^ ]+\\)" nil t)
          (push (match-string 1) config-functions))))
    (let* ((command (nth (random (length config-functions)) config-functions)))
      (describe-function (intern (concat "my/" command))))))


;; {{ START: my/open-link-at-point-as-gpg
(defun my/securely-delete-file (&optional filename)
  "Securely delete the specified file interactively or by providing FILENAME.
If secure deletion failed, then continue with the normal deletion."
  (interactive (list (when current-prefix-arg
		       (read-file-name "Choose file to securely delete: "))))
  (if filename
      (progn
	(message "Securely deleting %s..." (shell-quote-argument filename))
	(cond
	 ((eq system-type 'windows-nt)
	  ;; https://learn.microsoft.com/en-us/sysinternals/downloads/sdelete
	  (my-check-for-executable "SDelete" "sdelete")
	  (shell-command (concat "sdelete -p 3 " (shell-quote-argument filename))))
	 ((eq system-type 'gnu/linux)
	  (my-check-for-executable "shred" "shred")
	  (shell-command (concat "shred -v -z -u -n 10 " (shell-quote-argument filename))))
	 ((eq system-type 'darwin)
	  (my-check-for-executable "shred" "gshred")
	  (shell-command (concat "gshred -v -z -u -n 10 " (shell-quote-argument filename)))))
	(when (file-exists-p (shell-quote-argument filename))
	  (message "Securely deleting %s failed, and continue with the normal deletion." (shell-quote-argument filename))
	  (delete-file filename)))
    (user-error "No file specified for secure deletion.")))

(defun my/open-link-at-point-as-gpg ()
  "Open the link at point using Emacs epa in a temporary buffer,
and the decrypted file will be securely deleted after opening in buffer."
  (interactive)
  (require 'epa)
  (let* ((link-info (org-element-context))
         (path (org-element-property :path link-info))
         (abs-path (if (string-prefix-p "file:" path)
                       (file-truename (replace-regexp-in-string ":" "" path))
                     (file-truename path)))
         (decrypted-file (concat abs-path ".clear")))
    (if (file-exists-p abs-path)
        (progn
          (epa-decrypt-file abs-path decrypted-file)
          (find-file decrypted-file)
          (when (file-exists-p decrypted-file)
	    (my/securely-delete-file decrypted-file)))
      (message "File does not exist: %s" abs-path))))
;; END: my/open-link-at-point-as-gpg }}


;; {{ START: my/check-orphaned-org-ids-in-directory
(require 'org-element) ; this should be here before `org-add-link-type'
(require 'cl-lib)

;; From ChatGPT,
;; The message "Created id link." is printed by the `org-add-link-type` function
;; each time it is called.
;; Since you have the line `(org-add-link-type "id" #'my-org-id-link-follow)` in
;; your code, this function is called every time you load or reload your Emacs
;; configuration. It registers a new link type called `"id"` that is handled by
;; the `my-org-id-link-follow` function.

;; register new link type called "id"
(org-add-link-type "id" #'my-org-id-link-follow)

(defun my-org-id-link-follow (id)
  "Follow an `id' link."
  (message "Link ID: %s" id))

(defun my-org-id-links-in-buffer ()
  "Return a list of Org ID links in the current buffer."
  (let (org-id-links) ; creates a local variable called `org-id-links` with an
		      ; initial value of `nil` that is only visible within the
		      ; `let` block
    (org-element-map (org-element-parse-buffer) 'link
      (lambda (link)
        (when (string= (org-element-property :type link) "id")
          (push (org-element-property :path link) org-id-links)
          )))
    org-id-links))

(defun my-list-org-id-links-in-directory (directory)
  "Search all .org files in DIRECTORY for Org ID links, and return a list of unique IDs found."
  (interactive "DDirectory: ")
  (let (org-ids)
    (dolist (file (directory-files-recursively directory "\\.org$") org-ids)
      (with-temp-buffer
        (insert-file-contents file)
        (setq org-ids (append org-ids (my-org-id-links-in-buffer)))
        ))
    (delete-dups org-ids)
    ))

(defun my-list-org-ids-in-directory (directory)
  "List all org-ids in org-files in the given DIRECTORY and return them as a list."
  (interactive "DDirectory: ")
  (let ((org-files (directory-files-recursively directory "\\.org$"))
	(org-ids '()))
    (dolist (file org-files)
      (with-temp-buffer
	(insert-file-contents file)
	(org-mode)
	(org-element-map (org-element-parse-buffer) 'headline
	  (lambda (headline)
	    (when-let ((id (org-element-property :ID headline)))
	      (push id org-ids))))
	(goto-char (point-min))
	(while (re-search-forward "^:ID:\\s-+\\(\\S-+\\)" nil t)
	  (push (match-string 1) org-ids))))
    org-ids))

(defun my/check-orphaned-org-ids-in-directory (dir)
  "Find the difference between org-ids obtained by `my-list-org-ids-in-directory'
and org-ids obtained by `my-list-org-id-links-in-directory'.
DIRECTORY is the directory where the org files are located."
  (interactive "DDirectory: ")
  (let ((org-ids (my-list-org-ids-in-directory dir))
        (id-links (my-list-org-id-links-in-directory dir)))
    (let ((not-linked (cl-set-difference org-ids id-links :test #'string=))
          (invalid-links (cl-set-difference id-links org-ids :test #'string=)))
      (message "%d not-linked org-ids: %s"
               (length not-linked)
               (format "%s" not-linked))
      (message "%d invalid org-id links: %s"
               (length invalid-links)
               (format "%s" invalid-links)))))
;; END: my/check-orphaned-org-ids-in-directory }}


(defun my/org-list-entries-without-id-property ()
  "List all entries in the current buffer that don't have an ID property."
  (interactive)
  (with-output-to-temp-buffer "*Org Entries Without ID*"
    (let ((results nil))
      (org-map-entries
       (lambda ()
	 (unless (org-id-get)
	   (push (format "** LINE #%d:\n%s"
			 (line-number-at-pos)
			 (buffer-substring-no-properties
			  (line-beginning-position)
			  (line-end-position)))
		 results)))
       nil nil t)
      (princ (concat "#+TITLE: Org Entries Without ID\n\n"))
      (princ (concat "#+OPTIONS: toc:nil\n\n"))
      (princ (concat "* Entries without ID\n\n"))
      (dolist (result (nreverse results))
	(princ (concat result "\n\n")))))
  (with-current-buffer "*Org Entries Without ID*"
    (org-mode)))


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


(defun my/link-selected-text-with-org-id-from-kill-ring ()
  "Create an Org-mode link using the selected text and an Org ID from the kill ring.
Version 2023-04-28

The selected text is replaced with,
[[id:<Org ID unique identifier>][<selected text>]].

Usage: Select the text that you want to link to an Org ID, then
run `M-x my/link-selected-text-with-org-id-from-kill-ring`. The
function will take the Org ID from the kill ring, and create an
Org-mode link with the selected text and the Org ID. The link
will be inserted at the cursor position, replacing the selected
text."
  (interactive)
  (let* ((org-id (substring-no-properties (current-kill 0)))
         (text (buffer-substring-no-properties (region-beginning) (region-end)))
         (link (concat "[[id:" org-id "][" text "]]")))
    (delete-region (region-beginning) (region-end))
    (insert link)))


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


(defun my/switch-opened-org-files-to-org-mode ()
  "Switch all open buffers that end with .org to org-mode,
skipping buffers that are already in org-mode.
Version 2023-05-06"
  ;; See, https://stackoverflow.com/a/76187210/4274775
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (buffer-file-name)
                 (string= (file-name-extension (buffer-file-name)) "org")
                 (not (eq major-mode 'org-mode)))
        (org-mode)
        (message "Switched %s to org-mode." (buffer-name))))))

(defun my/strikethrough-current-line ()
  "Strikethrough the current line using +<striked text>+"
  (interactive)
  (back-to-indentation)
  (insert "+")
  (move-end-of-line nil)
  ;;  skips over any consecutive space or tab characters immediately before the
  ;;  end of the line, effectively moving the cursor to the last non-blank
  ;;  character on the line, rather than after any trailing whitespace. see,
  (skip-chars-backward " \t")
  (insert "+"))

(defun my/readonly-files ()
  "Check for a '.readonly' file in the directory of the current
buffer, and set the read-only status of any listed buffers. The
'.readonly' file should contain a list of buffer names, one per
line, that should be set to read-only.  Any buffers not listed in
the file will remain unaffected.
Version 2023-05-04

This function is intended to be used as a hook to automatically
set the read-only status of buffers when they are opened or
saved, based on the contents of the '.readonly' file. To use this
function as a hook, add it to the appropriate hook list, such as
'find-file-hook', 'after-save-hook' or 'switch-buffer-hook'."
  ;; (add-hook 'find-file-hook 'my/readonly-files)
  ;; (add-hook 'after-save-hook 'my/readonly-files)
  ;; (add-hook 'switch-buffer-hook 'my/readonly-files)
  (interactive)
  (let ((readonly-file (concat (file-name-directory (buffer-file-name)) ".readonly")))
    (when (file-exists-p readonly-file)
      (let ((readonly-bufs (split-string (with-temp-buffer
					   (insert-file-contents readonly-file)
					   (buffer-string))
					 "\n" t)))
        (message "read-only files list: %s" readonly-bufs)
	(dolist (buf readonly-bufs)
          (message "%s is read-only now" buf)
	  (let ((buf (find-buffer-visiting buf)))
	    (when buf
	      (with-current-buffer buf
		(toggle-read-only t)))))))))

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

;; TODO: append needs to be tested...
(defun my/create-TAGS (dir-name tag-relative tags-filename &optional append sudo process-name)
  "Create a TAGS file with absolute or relative symbols recorded inside. With a
prefix argument SUDO, run the command with sudo privilege.

When called interactively, prompt the user for the directory name to create the
TAGS file. If no input is given, use the current working directory.

The `ctags` command is executed with the `--tag-relative` option
set to `yes` if the `tag-relative` is set to 'y', or 'n'
indicates 'never'. The `*` wildcard is included in the `ctags`
command to create TAGS for all files in the directory.

Example usage:
  - To create a TAGS (with absolute symbols) file for the current directory:
    M-x my/create-TAGS RET /path/to/current/directory RET RET
  - To create a TAGS file for a specific directory with relative symbols recorded:
    M-x my/create-TAGS RET /path/to/directory RET y RET
  - To create a TAGS file for a specific directory with absolute symbols recorded,
    using sudo privilege:
    C-u M-x my/create-TAGS RET /path/to/directory RET RET

Version: 2023-03-17
Updated: 2023-08-17"

  ;; This function is improved by ChatGPT and Claude :)
  (interactive
   (let ((tag-relative (completing-read "Create TAGS file with relative symbols? (y/n)\n(Note: omit input indicates absolute symbols) "
			    '("y" "n"))))
     (list (read-directory-name "Enter the directory to create TAGS file: ")
	   tag-relative
	   (read-string "Enter the desired tags filename: "
			(if (string-equal tag-relative "y") "TAGS" "TAGS_ABS"))
       nil
	   current-prefix-arg ; if universal argument (sudo)
	   nil)))

  (let* ((target-dir (if (string= "" dir-name)
			 default-directory
		       (expand-file-name dir-name)))

	 (tag-relative-value (if (string-equal tag-relative 'y) "yes" "never"))
	 ;; yes   - relative symbols
	 ;; never - absolute symbols

	 (append-or-not (if append "--append=yes" ""))

	 (tags-path (expand-file-name tags-filename target-dir))

	 (ctags-cmd (format "cd %s && ctags --options=%s -e -R --tag-relative=%s %s -f %s *"
			    (if (eq system-type 'windows-nt)
				;; fix changing dir across different drives issue on Windows
				(concat "/d" target-dir)
			      target-dir)
			    (expand-file-name ".ctags" user-emacs-directory)
			    tag-relative-value
			    append-or-not

			    ;; if tags-path is non-nil, it will use that value
			    ;; as the result. if tags-path is nil, it will
			    ;; evaluate the expression (expand-file-name "tags"
			    ;; target-dir) and use the result of that evaluation
			    ;; as the final result.
			    tags-path))
         (command (if sudo
		       (concat "sudo sh -c '"
			       ctags-cmd
			       "'")
		     ctags-cmd)))

    (start-process-shell-command (or process-name "create TAGS") nil command)
    (message "Creating TAGS...")))

;; TODO: to be tested...
(defvar my/tags-file-name "TAGS"
  "The default name of the tags file to search for.")

(defun my/find-tags-file (&optional ask (tags-file-name my/tags-file-name))
  "Recursively searches each parent directory for a file named
'TAGS' and returns the path to that file or nil if a tags file is
not found. Returns nil if the buffer is not visiting a file.

Optional arguments:
  ASK (default nil) - If t, prompt for entering the tags file name.
  TAGS-FILE-NAME (default 'TAGS') - The name of the tags file to search for.
  
Version: 2023-08-23"
  (progn
    (defun find-tags-file-r (path tags-file)
      "Find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
             (possible-tags-file (concat parent tags-file)))
        (cond
         ((file-exists-p possible-tags-file)
          (throw 'found-it possible-tags-file))
         ((string= (concat "/" tags-file) possible-tags-file)
          (error "No tags file found"))
         (t (find-tags-file-r (directory-file-name parent) tags-file)))))

    (if (buffer-file-name)
        (catch 'found-it
          (if ask
              (let ((tags-file-name-input (read-from-minibuffer "Enter tags file name: " tags-file-name)))
                (find-tags-file-r (buffer-file-name) tags-file-name-input))
            (find-tags-file-r (buffer-file-name) tags-file-name)))
      (error "Buffer is not visiting a file"))))

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
(defun my-tags-file (&optional select tags-file)
  "If SELECT is non-nil, set the value of `my-tags-file` to
TAG-FILE. If TAGS-FILE is nil, use the user-selected file path
after prompting for it through `my/file`.
Otherwise, set `my-tags-file` to the value returned by
`my/find-tags-file`.  -- generated by ChatGPT :)

Updated: 2023-08-20"
  (if select
      (if tags-file
	  (setq my-tags-file tags-file)
	(progn (my/file)
	       (setq my-tags-file my-file-value)))
    (setq my-tags-file (my/find-tags-file)))
  )

(defun my-set-extra-tags-files (my-tags-table-list)
  (setq counsel-etags-extra-tags-files my-tags-table-list)
  (setq company-ctags-extra-tags-files my-tags-table-list)
  (message "tags-table list for counsel-etags/company-ctags:\n%s\n\nNote:
files in counsel-etags-extra-tags-files should have symbols with
absolute path only."  my-tags-table-list)
  )

(defun my/insert-into-my-tags-table-list(&optional select tags-file)
  "automatically insert the TAGS file or select TAGS file to
insert(C-u), into `my-tags-table-list',
`counsel-etags-extra-tags-files' and
`company-ctags-extra-tags-files'.

Updated: 2023-08-20"
  (interactive "P")
  (unless (boundp 'my-tags-table-list)
    ;; if `my-tags-table-list' is void, then set it to empty list
    (setq my-tags-table-list '()))
  (setq existing-my-tags-table-list my-tags-table-list)
  (setq my-tags-table-list '()) ; initiate empty list
  (my-tags-file select tags-file)
  (setq my-tags-table-list
	(delq nil (delete-dups ; delete nil and duplicates
		   (cons (symbol-value 'my-tags-file)
			 (symbol-value 'existing-my-tags-table-list)))))
  (my-set-extra-tags-files my-tags-table-list)
  )

(defun my/delete-from-my-tags-table-list (&optional select tags-file)
  "automatically delete the TAGS file or select TAGS file to
delete(C-u), from `my-tags-table-list',
`counsel-etags-extra-tags-files' and
`company-ctags-extra-tags-files'.

Updated: 2023-08-20"
  (interactive "P")
  (my-tags-file select tags-file)
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
  (message "tags-table list for counsel-etags/company-ctags:\n%s\n\nNote:
files in counsel-etags-extra-tags-files should have symbols with
absolute path only."  my-tags-table-list)
  )
;; END: config for counsel-etags and company-ctags }


(provide 'init-misc)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-misc.el ends here
