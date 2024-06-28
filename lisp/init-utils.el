;;; init-utils.el --- Elisp helper functions and commands -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; the my/xxx utils config - yet to be placed in dedicated init-xxx.el files



(defun my/highlight-selected-text (start end &optional color)
  "Highlight the selected region temporarily with the specified color.
If color is not provided, the default color is #5F87FF.

Version 2023-10-18"
  (interactive "r\nsEnter color (e.g., 'red', press ENTER for default #5F87FF): ")
  (let* ((overlay (make-overlay start end))
	 (color (if (string= color "") "#5F87FF" color))
	 (text-color (if (or (string= color "black") (string= color "#5F87FF"))
			 "white"
		       "black")))
    (overlay-put overlay 'face `((:background ,color :foreground ,text-color)))
    (add-hook 'before-revert-hook (lambda () (delete-overlay overlay)))))


(defun my/random-org-item ()
  "Go to a random org heading from all org files in `org-directory`."
  (interactive)
  (let* ((org-files (directory-files-recursively org-directory "\\.org$"))
         (random-file (nth (random (length org-files)) org-files)))
    (find-file random-file) ; open the random org file
    (org-mode)
    (org-overview)
    (goto-char (point-min))
    (org-next-visible-heading 1) ; move to the first visible heading
    ;; parse the buffer and collect all headline elements into `headings` list, see,
    (let* ((headings (org-element-map (org-element-parse-buffer) 'headline 'identity)))
      (if headings
	  (progn
	    ;; generate a random index within the length of `headings`, see,
	    (let* ((random-index (random (length headings)))
		   ;; select a random heading from `headings` using the random index, see,
		   (random-heading (nth random-index headings)))
	      ;; move the cursor to the beginning of the randomly selected heading. see,
	      (goto-char (org-element-property :begin random-heading))
	      (org-show-subtree))) ; expand the selected heading
	(goto-char (point-min))))) ; if no headings are found, move the cursor to the beginning of the buffer
  )


(defun my/eww-open-local-file ()
  "Open the local file at point in EWW."
  (interactive)
  (let ((file (thing-at-point 'filename)))
    (eww-open-file file)))

(defun my/org-repeated-deadline-schedule (type &optional arg time repeater)
  "Create repeated deadlines or scheduled tasks.
  TYPE is either 'deadline or 'schedule.

  With prefix argument(C-u), the existing deadline/schedule will
  be removed.

  Repeater string should be in the format of `+1m' (exactly 1 month),
  `.+1m' (1 month from last completion) or `++1m' (at least 1 month from
  last completion, and keep it on the same day of the week, moving the
  due date into the future by increments of month)."

  ;; This function is improved by ChatGPT :)
  (interactive
   (list (intern (completing-read "Type: "
				  '(("deadline") ("schedule"))))
	 current-prefix-arg nil))
  (unless arg
    (setq repeater (read-string "Input the repeater: ")))
  (let ((org-time-stamp-formats
	 (if repeater
	     `(,(concat "<%Y-%m-%d %a " repeater ">") .
	       ,(concat "<%Y-%m-%d %a %H:%M " repeater ">"))
	   org-time-stamp-formats)))
    (cl-case type
      (deadline (org-deadline arg time))
      (schedule (org-schedule arg time)))))

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
	  '("emacs-lisp" "python" "C" "shell" "java" "js" "clojure" "C++" "css"
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


(defun my/generate-current-time-string (&optional universal-arg silent)
  "Generate a string representing the current date and time in specific format.
(e.g., 230725192607 for July 25th, 2023 at 19:26:07).

When UNIVERSAL-ARG (C-u) is provided, copy the time string to the kill ring.

Usage:
M-x my/generate-current-time-string
C-u M-x my/generate-current-time-string

Version 2023-07-25"
  (interactive "P")
  (let* ((now (current-time))
	 (time-string (concat (substring (format-time-string "%Y" now) -2)
			      (format-time-string "%m%d%H%M%S" now))))
    (unless silent
      (insert time-string))
    (when universal-arg
      (kill-new time-string)
      (message "%s is copied." time-string))
    (message "Current time string generated: %s" time-string)
    time-string))


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
(defun my-org-id-link-pre ()
  "The precondition config to my org id link settings"
  (my-require 'org-element) ; this should be here before `org-add-link-type'
  (my-require 'cl-lib)

  ;; From ChatGPT,
  ;; The message "Created id link." is printed by the `org-add-link-type` function
  ;; each time it is called.
  ;; Since you have the line `(org-add-link-type "id" #'my-org-id-link-follow)` in
  ;; your code, this function is called every time you load or reload your Emacs
  ;; configuration. It registers a new link type called `"id"` that is handled by
  ;; the `my-org-id-link-follow` function.

  ;; register new link type called "id"
  (org-add-link-type "id" #'my-org-id-link-follow))

(defun my-org-id-link-follow (id)
  "Follow an `id' link."
  (message "Link ID: %s" id))

(defun my-org-id-links-in-buffer ()
  "Return a list of Org ID links in the current buffer."
  (my-org-id-link-pre)
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
  (my-org-id-link-pre)
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


(defun my/list-packages-and-versions (&optional filepath)
  "List installed packages and their versions.
If FILEPATH is provided, export the output to the specified file.
If the file is an org file, insert a title with the Emacs version at the beginning.
Otherwise, output to the message buffer."
  (interactive)
  (package-initialize)
  (let ((pkgs (mapcar 'car package-alist)))
    (with-temp-buffer
      ;; Insert title if the output is an org file
      (when (and filepath (string-suffix-p ".org" filepath))
        (insert (format "#+TITLE: My Packages and Versions on Emacs %s\n" emacs-version))
	(insert (format "#+DATE: %s\n" (format-time-string "%Y-%m-%d %H:%M:%S"))))
      ;; Insert package and version information
      (dolist (pkg pkgs)
        (insert (format "%s - %s\n"
                        pkg (package-desc-version (cadr (assq pkg package-alist))))))
      ;; Output to message buffer or write to file
      (if (or (called-interactively-p 'interactive) (not filepath))
          (message "%s" (buffer-string))
        (when filepath
          (set-buffer-file-coding-system 'utf-8-unix)
          (write-file filepath))))))


(defun my/copy-org-id-at-point ()
  "Copy the ID property of the heading at point to the kill-ring."
  (interactive)
  (let ((id (org-entry-get nil "ID")))
    (if id
	(progn
	  (kill-new id)
	  (message "Copied ID: %s" id))
      (message "No ID property in the heading at point."))))

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
will be killed. nov-mode buffers will be skipped."
  ;; via https://emacs.stackexchange.com/a/24461/29715
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
		 (not (buffer-modified-p buf))
		 (not ; skip nov-mode buffers
		  (eq (buffer-local-value 'major-mode buf) 'nov-mode))
		 )
	(if (file-readable-p filename)
	    ;; If the file exists and is readable, revert the buffer.
	    (with-current-buffer buf
	      (revert-buffer :ignore-auto :noconfirm :preserve-modes))
	  ;; Otherwise, kill the buffer.
	  (let (kill-buffer-query-functions) ; No query done when killing buffer
	    (kill-buffer buf)
	    (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))


(defun my/copy-current-buffer-to-another-buffer (target-buffer)
  "Copy the content of the current buffer to another buffer.
If the target buffer does not exist, it will be created.
If the target buffer exists, the content will be appended.

Version: 2023-08-31"
  (interactive "BTarget Buffer: ")
  (let ((source-buffer (current-buffer))
        (existing-buffer (get-buffer-create target-buffer)))
    (with-current-buffer existing-buffer
      (goto-char (point-max)) ; move to the end of the existing buffer
      (insert-buffer-substring source-buffer)
      (pop-to-buffer existing-buffer))))


(defun my/kill-buffers-by-pattern (pattern)
  "Kill buffers whose names match the specified pattern.

This function interactively prompts the user for a pattern and then searches
through the list of all buffers. Buffers whose names match the given pattern
are killed, effectively closing them. The pattern is a regular expression that
is compared against buffer names using 'string-match-p'.

Version: 2023-08-16"
  (interactive "sEnter a pattern: ")
  (dolist (buffer (buffer-list))
    (let ((buffer-name (buffer-name buffer)))
      (message "Processing buffer: %s" buffer-name)
      (when (string-match-p pattern buffer-name)
        (kill-buffer buffer)
        (message "Killed buffer '%s'" buffer-name)))))



(provide 'init-utils)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-utils.el ends here
