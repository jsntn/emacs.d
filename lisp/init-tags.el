;;; init-tags.el --- tags related config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; a tags file is an index to source-code definitions of functions, variables, and any other interesting syntactic feature.




(defun my/create-tags
    (dir-name tags-format tag-relative tags-filename
	      &optional tags-path append sudo process-name)
  "Create a tags file with absolute or relative symbols recorded inside. With a
prefix argument SUDO, run the command with sudo privilege.

When called interactively, prompt the user for the directory name to create the
tags file. If no input is given, use the current working directory.

The `ctags` command is executed with the `--tag-relative` option
set to `yes` if the `tag-relative` is set to 'y', or 'n'
indicates 'never'. The `*` wildcard is included in the `ctags`
command to create tags for all files in the directory.

Version: 2023-03-17
Updated: 2023-10-20"

  ;; This function is improved by ChatGPT and Claude :)
  (interactive
   (let* ((tags-format (completing-read "ctags or etags format? (ctags/etags)\n(Note: omit input indicates etags format) "
					'("ctags" "etags")
					nil t nil nil "etags"))
	  (tag-relative (completing-read "Create tags index file with relative symbols? (y/n)\n(Note: omit input indicates absolute symbols) "
					 '("y" "n")
					 nil t nil nil "n"))
	  (tags-filename (if (string-equal tags-format "etags")
			     (if (string-equal tag-relative "y") "TAGS" "TAGS_ABS")
			   (if (string-equal tag-relative "y") "tags" "tags_abs"))))
     (list (expand-file-name (read-directory-name "Enter the directory for creating tags file: "))
	   tags-format
	   tag-relative
	   (read-string "Enter the desired tags filename: " tags-filename)
	   (if (boundp 'tags-path) tags-path nil)
	   (completing-read "Append the tags to existing tags index file? (y/n)\n(Note: omit input indicates creating) "
			    '("y" "n")
			    nil t nil nil "n")
	   (if (boundp 'sudo)
	       sudo
	     (if current-prefix-arg t nil) ; if universal argument (sudo)
	     )
	   (if (boundp 'process-name) process-name "create tags"))))

  ;; debug,
  ;; (message "dir-name: %s" dir-name)

  (let* ((target-dir-value (if (string-empty-p dir-name)
			       default-directory
			     dir-name))

	 (tags-format-value (if (string-equal tags-format 'ctags) "" "-e"))

	 (tag-relative-value (if (string-equal tag-relative 'y) "yes" "never"))
	 ;; yes   - relative symbols
	 ;; never - absolute symbols

	 (append-t-or-not (if (string-equal append 'y) t nil))
	 (append-or-create (if (string-equal append 'y) "- APPEND: " "- CREATE: "))
	 (append-or-not (if (string-equal append 'y) "--append=yes" ""))

	 (tags-path-value
	  (if (string= tag-relative 'y)
	      (expand-file-name tags-filename target-dir-value)
	    (or tags-path
		(expand-file-name tags-filename
				  (read-directory-name
				   "Enter the path to store the tags file: "
				   nil default-directory)))))

	 (command-process-name process-name)

	 (ctags-cmd (format "cd %s && ctags %s %s -R --tag-relative=%s %s -f %s *"
			    (if (and
				 (eq system-type 'windows-nt)
				 (not (string-prefix-p "/d" target-dir-value)))
				;; On Windows, add "/d " if switching across drives
				(concat "/d " target-dir-value)
			      target-dir-value)
			    (let ((ctags-file (expand-file-name ".ctags" user-emacs-directory)))
			      (if (file-exists-p ctags-file)
				  (format "--options=%s" ctags-file)
				""))
			    tags-format-value
			    tag-relative-value
			    append-or-not
			    tags-path-value))
	 (command (if sudo
		      (concat "sudo sh -c '"
			      ctags-cmd
			      "'")
		    ctags-cmd)))

    ;; (message "command: %s" ctags-cmd)

    (if (get-process command-process-name)
	(message "Process (%s) already running..." command-process-name)
      (progn
	(start-process-shell-command command-process-name
				     (format "*%s*" command-process-name)
				     command)
	(message "Creating tags...")

	(when append-t-or-not
	  (my-insert-newline-at-end-of-file
	   (concat tags-path-value ".commands")))

	(my-write-to-file
	 (format-time-string "%Y-%m-%d %H:%M:%S")
	 (concat tags-path-value ".commands")
	 append-t-or-not
	 sudo)

	(my-insert-newline-at-end-of-file
	 (concat tags-path-value ".commands"))

	(my-write-to-file
	 (concat append-or-create command)
	 (concat tags-path-value ".commands")
	 t
	 sudo)

	(my-insert-newline-at-end-of-file
	 (concat tags-path-value ".commands"))

	(my-write-to-file
	 (concat append-or-create
		 (format "(my/create-tags \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" \"%s\" %s \"%s\")"
			 target-dir-value
			 tags-format
			 tag-relative
			 tags-filename
			 tags-path-value
			 append
			 sudo
			 process-name
			 ))
	 (concat tags-path-value ".commands")
	 t
	 sudo)

	(my-insert-newline-at-end-of-file
	 (concat tags-path-value ".commands"))

	(my-merge-duplicated-lines-in-file
	 (concat tags-path-value ".commands")
	 sudo)


	))
    ))

(defvar my/default-tags-file-name "TAGS"
  "The default name of the tags file to search for.")

(defun my/find-tags-file (&optional ask tags-file-name)
  "Recursively search for a 'TAGS' file in parent directories and return its path.

Optional arguments:
  ASK (default nil): If t, prompt for entering a custom tags file
  name.
  TAGS-FILE-NAME (default 'TAGS'): The name of the tags file to
  search for.

This function searches for a 'TAGS' file by recursively examining
parent directories starting from the directory of the currently
visited file (if any). If a 'TAGS' file is found, its full path
is returned. If no 'TAGS' file is found, or if the current buffer
is not visiting a file, it returns nil.

Usage examples:
  (my/find-tags-file)                  ; Search for default 'TAGS' file in parent
                                         directories
  (my/find-tags-file t)                ; Prompt for a custom tags file name
  (my/find-tags-file nil \"TAGS_ABS\") ; Search for a 'TAGS_ABS' file
  (my/find-tags-file nil \"TAGS\")     ; Search for the default 'TAGS' file

Version: 2023-08-23"
  (progn
    (unless tags-file-name
      (setq tags-file-name my/default-tags-file-name))
    (defun find-tags-file-r (path tags-file prev-parent)
      "Find the tags file from the parent directories"
      (let* ((parent (file-name-directory path))
	     (possible-tags-file (concat parent tags-file)))
	(message "Found tags file: %s" possible-tags-file)
	(cond
	 ((file-exists-p possible-tags-file)
	  (throw 'found-it possible-tags-file)
	  (message "Found tags file: %s" possible-tags-file))
	 ((equal parent prev-parent)
	  (error "No tags file found")) ; stop if no progress is made
	 (t (message "Checking %s" possible-tags-file)
	    (find-tags-file-r (directory-file-name parent) tags-file parent)))))

    (if (buffer-file-name)
	(catch 'found-it
	  (if ask
	      (let ((tags-file-name-input
		     (read-from-minibuffer
		      "Enter tags file name: " tags-file-name)))
		(find-tags-file-r (buffer-file-name) tags-file-name-input nil))
	    (find-tags-file-r (buffer-file-name) tags-file-name nil)))
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
    (setq my-tags-file (my/find-tags-file t)))
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

(defun my/sync-tags-table-list ()
  "sync `tags-table-list' with `my-tags-table-list'.

Read more,
https://www.gnu.org/software/emacs/manual/html_node/emacs/Select-Tags-Table.html

Some commands for checking the values:
  (symbol-value 'tags-table-list)
  (symbol-value 'tags-file-name)

Version: 2023-08-30"
  (interactive)
  (setq tags-table-list my-tags-table-list)
  (message "tags-table-list is set to %s" tags-table-list))



(provide 'init-tags)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-tags.el ends here
