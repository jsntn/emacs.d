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

(defun my-monitor-kill-and-write-to-file (output-file-path x-seconds)
  "Monitor current kill ring and write its content to the specified file."
  (defun my-kill-monitor-task ()
    (condition-case nil
	(let ((current-contents (current-kill 0)))
	  (unless (equal current-contents my-previous-kill-contents)
	    (setq my-previous-kill-contents current-contents)
	    (with-temp-file output-file-path
	      (insert current-contents))))
    (error (message "Kill ring is empty."))))

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
  "Monitor the specified file for changes and copy its content to a register."
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





(defun my-insert-newline-at-end-of-file (file-path)
  "Inserts a new line at the end of the file specified by FILE-PATH."
  (with-current-buffer (find-file-noselect file-path)
    (goto-char (point-max))
    (newline)
    (save-buffer)
    (kill-buffer)))

(defun my-write-to-file (content file &optional append sudo)
  "Write CONTENT to FILE. If APPEND is true, append the content to the file; otherwise, overwrite the file.
  If SUDO is provided and non-nil, execute the write operation with sudo."
  (let* ((tee-command (if append "tee -a" "tee"))
         (sudo-command (if sudo (concat "sudo " tee-command) tee-command))
         (cmd (concat "echo " (shell-quote-argument content) " | " sudo-command " " (shell-quote-argument file))))
    (if sudo
        (shell-command cmd)
      (write-region (point-min) (point-max) file append))
    ))

(defun my-merge-duplicated-lines-in-file (file &optional sudo)
  "Merge duplicated lines in FILE.
  If SUDO is provided and non-nil, execute the merge operation with sudo."
  (interactive "f")
  (with-temp-buffer
    (insert-file-contents file)
    (let ((lines (split-string (buffer-string) "\n" t)))
      (setq lines (delete-dups lines))
      (setq lines (sort lines 'string>)) ;; Sort the lines
      (erase-buffer)
      (insert (mapconcat 'identity lines "\n")))
    (if sudo
	(let* ((sudo-command (concat "sudo tee " (shell-quote-argument file)))
	      (cmd (concat "echo " (shell-quote-argument (buffer-string)) " | " sudo-command)))
	  (shell-command cmd))
      (write-region (point-min) (point-max) file))))


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
Updated: 2023-10-12"

  ;; This function is improved by ChatGPT and Claude :)
  (interactive
   (let ((tags-format (completing-read "ctags or etags format? (ctags/etags)\n(Note: omit input indicates etags format) "
				       '("ctags" "etags")))
	 (tag-relative (completing-read "Create tags index file with relative symbols? (y/n)\n(Note: omit input indicates absolute symbols) "
					'("y" "n"))))
     (list (read-directory-name "Enter the directory for creating tags file: ")
	   tags-format
	   tag-relative
	   (read-string "Enter the desired tags filename: "
			(if (string-equal tags-format "etags")
			    (if (string-equal tag-relative "y") "TAGS" "TAGS_ABS")
			  (if (string-equal tag-relative "y") "tags" "tags_abs")))
	   (if (boundp 'tags-path) tags-path nil)
	   (completing-read "Append the tags to existing tags index file? (y/n)\n(Note: omit input indicates creating) "
			    '("y" "n"))
	   (if (boundp 'sudo)
	       sudo
	     (if current-prefix-arg t nil) ; if universal argument (sudo)
	     )
	   (if (boundp 'process-name) process-name "create tags"))))

  (let* ((target-dir-value (if (string= "" dir-name)
			       default-directory
			     (if (eq system-type 'windows-nt)
				 ;; if the dir-name already start with "/d", just use it
				 (if (string-prefix-p "/d" dir-name)
				     dir-name
				   ;; fix changing dir across different drives issue on Windows
				   (concat "/d " dir-name))
			       (expand-file-name dir-name))))

	 (tags-format-value (if (string-equal tags-format 'ctags) "" "-e"))

	 (tag-relative-value (if (string-equal tag-relative 'y) "yes" "never"))
	 ;; yes   - relative symbols
	 ;; never - absolute symbols

	 (append-t-or-not (if (string-equal append 'y) t nil))
	 (append-or-create (if (string-equal append 'y) "APPEND: " "CREATE: "))
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

	 (ctags-cmd (format "cd %s && ctags --options=%s %s -R --tag-relative=%s %s -f %s *"
			    target-dir-value
			    (expand-file-name ".ctags" user-emacs-directory)
			    tags-format-value
			    tag-relative-value
			    append-or-not
			    tags-path-value))
         (command (if sudo
		       (concat "sudo sh -c '"
			       ctags-cmd
			       "'")
		     ctags-cmd)))


(when append-t-or-not
  (my-insert-newline-at-end-of-file
    (concat tags-path-value ".commands")))

    (my-write-to-file
     (concat append-or-create command)
     (concat tags-path-value ".commands")
     append-t-or-not
     sudo)

;; TODO: to be tested on macOS...
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


    (if (get-process command-process-name)
	(message "Process (%s) already running..." command-process-name)
      (progn
	(start-process-shell-command command-process-name nil command)
	(message "Creating tags...")))
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



(provide 'init-misc)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-misc.el ends here
