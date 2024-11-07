;;; init-veracrypt.el --- VeraCrypt/TrueCrypt settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




;; {{ START: VeraCrypt Volume mounting
(require 'org-element)

(defun my-drive-letter-in-use-p (drive-letter)
  "Check if a Drive Letter is in use on Windows."
  (let ((result (shell-command-to-string
                 (concat
		  "powershell -command \"Test-Path '" drive-letter":' -IsValid\""))))
    (if (string-match-p "True" result)
        t
      nil)))

(defun my-find-available-drive-letter ()
  "Find an available Drive Letter on Windows."
  (catch 'drive-letter-found
    (mapc (lambda (code)
            (let ((drive-letter (format "%c" code)))
              (message "Checking Drive %s -> %s"
		       drive-letter (my-drive-letter-in-use-p drive-letter))
              (unless (my-drive-letter-in-use-p drive-letter)
                (throw 'drive-letter-found drive-letter))))
          (number-sequence ?D ?Z)))) ; A, B, C are not available on TrueCrypt
				     ; and also they are reserved for floppy,
				     ; primary, secondary

(defun my/mount-veracrypt-volume (mode drive-option
				       &optional volume-path drive-letter use-point-link truecrypt
				       passphrase keyfiles)
  "Mount a VeraCrypt or TrueCrypt volume in the selected MODE and assign DRIVE-LETTER automatically.
If USE-POINT-LINK is non-nil, use the volume link at point instead of prompting.

MODE can be 'read-only' or 'write'.
DRIVE-OPTION can be 'auto' or 'specify'.
VOLUME-PATH is the path to the VeraCrypt/TrueCrypt volume.
DRIVE-LETTER is the desired drive letter (e.g., 'D').
USE-POINT-LINK is non-nil to use the volume link at point.
TRUECRYPT is non-nil to use TrueCrypt instead of VeraCrypt.
PASSPHRASE is the passphrase for the volume.
KEYFILES is a list of keyfiles for the volume.

Version 2023-08-07
Updated 2023-08-15"

  ;; Usage:
  ;; (my/mount-veracrypt-volume "read-only" "auto" "/path/to/volume")
  ;; (my/mount-veracrypt-volume "read-only" "specify" "/path/to/volume" "D")
  ;; (my/mount-veracrypt-volume "read-only" "auto" "/path/to/volume" nil nil t)
  ;; (my/mount-veracrypt-volume "read-only" "auto" "/path/to/volume" nil nil t "test" '("/path/to/keyfile"))
  ;; (my/mount-veracrypt-volume "read-only" "auto" "/path/to/volume" nil nil t "test" '("/path/to/keyfile1" "/path/to/keyfile2"))

  (interactive
   (let ((universal-arg current-prefix-arg)
         (mode (completing-read "Select mode: " '("read-only" "write")))
         (drive-option
          (completing-read "Select Drive Letter assignment option: "
                           '("auto" "specify"))))

     (if (equal '(4) universal-arg) ; Check for universal argument
         (list mode drive-option
	       nil ; volume-path
	       ;; drive-letter
	       (if (equal drive-option "specify")
		   (read-string "Specify the Drive Letter: ")
		 nil)
	       t ; pass t for use-point-link
	       )
       (list mode drive-option
	     ;; volume-path
             (read-file-name "Enter the path to the VeraCrypt/TrueCrypt volume: ")
	     ;; drive-letter
             (if (equal drive-option "specify")
                 (read-string "Specify the Drive Letter: ")
               nil)
             nil ; use-point-link
	     ))))

  (if (or (executable-find "veracrypt") (executable-find "truecrypt"))
      (let* ((is-windows (eq system-type 'windows-nt))
             (is-macos (eq system-type 'darwin))
             (is-linux (eq system-type 'gnu/linux))

             (mode-abbrev (if (equal mode "read-only") "ro"
                            (if (equal mode "write") "ts"
                              (error "Invalid mode"))))

             (abs-volume-path (if use-point-link
                                  (let* ((link-info (org-element-context))
                                         (path (org-element-property :path link-info))
                                         (abs-path (if (string-prefix-p "file:" path)
                                                       (file-truename
                                                        (replace-regexp-in-string "^file:" "" path))
                                                     (file-truename path))))
                                    (if (and abs-path (file-exists-p abs-path))
                                        abs-path
                                      (user-error "Invalid or non-existent link at point")))
                                volume-path))

             (final-drive-letter (if (equal drive-option "specify")
				     (when is-windows
				       (if (string-match-p "[D-Z]" drive-letter)
					   drive-letter
					 (user-error "Invalid Drive Letter format")))
                                   (if (equal drive-option "auto")
                                       (my-find-available-drive-letter)
                                     "auto")))

             (vera-or-true (if truecrypt "truecrypt" "veracrypt")))

        (if (and abs-volume-path (file-exists-p abs-volume-path))
	    (let* ((command (if is-windows
				(format "%s /q /m %s /v \"%s\" %s %s %s"
					vera-or-true
					mode-abbrev
					;; fix the path for Windows
					(subst-char-in-string ?/ ?\\ abs-volume-path)
					(if (not (equal final-drive-letter "auto"))
					    (format "/a /l %s" final-drive-letter)
					  (format "/a /l %s" (my-find-available-drive-letter)))
					(if passphrase (format "/p %s" passphrase) "")
					(if keyfiles
					    (mapconcat (lambda (file)
							 (format "/k \"%s\""
								 ;; fix the path for Windows
								 (subst-char-in-string ?/ ?\\ file)))
						       keyfiles
						       " ")
					  ""))
			      (if (or is-macos is-linux)
				  (format "%s -q -m %s -v \"%s\" %s %s %s"
					  vera-or-true
					  mode-abbrev
					  abs-volume-path
					  (if (not (equal final-drive-letter "auto"))
					      (format "-a -l %s" final-drive-letter)
					    "-a")
					  (if passphrase (format "-p %s" passphrase) "")
					  (if keyfiles
					      (mapconcat (lambda (file)
							   (format "-k \"%s\"" keyfiles))
							 keyfiles
							 " ")
					    ""))
				(user-error "Unknown platform"))))
		   (safe-command (if passphrase
				     (replace-regexp-in-string
				      (concat "/p " (regexp-quote passphrase) "\\s-")
				      "/p *******"
				      command)
				   ""))
		   )
              (message safe-command)
              (my-async-shell-command-with-unique-buffer-name command))
          (user-error "Volume does not exist")))
    (user-error "Neither VeraCrypt nor TrueCrypt is installed")))
;; END: VeraCrypt Volume mounting }}




(defun my/dismount-tc-vc-volume (volume-path &optional use-truecrypt)
  "Dismount a TrueCrypt or VeraCrypt volume at the given Drive Letter.
If called with a universal argument (C-u), TrueCrypt will be used; otherwise, VeraCrypt will be used.

Version 2023-08-10"

;; Usage:
;; (my/dismount-tc-vc-volume "A")
;; (my/dismount-tc-vc-volume "K" t)

  (interactive
   (let* ((use-truecrypt (if current-prefix-arg t nil))
          (prompt (if use-truecrypt "TrueCrypt" "VeraCrypt"))
          (volume-prompt (format "Enter the Drive Letter to the %s Volume to dismount: " prompt)))
     (list
      (read-string volume-prompt)
      use-truecrypt)))

  (let* ((crypt-command (if use-truecrypt "truecrypt" "veracrypt"))
         (command ""))

    (cond
     ((eq system-type 'windows-nt)
      (setq command (format "%s /q /d %s" crypt-command volume-path)))
     ((or (eq system-type 'darwin)
          (eq system-type 'gnu/linux))
      (setq command (format "%s -q -d %s" crypt-command volume-path)))
     (t
      (message "Unsupported system type")))

    (my-async-shell-command-with-unique-buffer-name command)))







(provide 'init-veracrypt)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-veracrypt.el ends here
