;;; init-dict.el --- dict settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; {{ START: Look up text at-point/marked by GoldenDict in Emacs
;; https://github.com/jsntn/goldendict-emacs/tree/my
(defun my-look-up-dict (word)
  (let ((goldendict-executable (executable-find "goldendict")))
    (if goldendict-executable
        (start-process "goldendict" nil goldendict-executable word)
      (message "GoldenDict executable not found. Please make sure it is installed and in your PATH."))))

(defun my/look-up-dict ()
  "Look up text at-point/marked by GoldenDict in Emacs.

Version 2023-08-03"
  (interactive)
  (let ((word ""))
    (if (and (bound-and-true-p mark-active) (not (equal (point) (mark))))
        (setq word (buffer-substring (region-beginning) (region-end)))
      (setq word (thing-at-point 'word)))
    (if (not (string-blank-p word))
        (my-look-up-dict word)
      (message "No word found at point or in the marked region."))))

(global-set-key (kbd "C-c g") 'my/look-up-dict)
;; END }}


(require 'company-english-helper)

(when *is-win*
  ;; only use this on Windows, as I am using
  ;; Andy Stewart's sdcv on my ArchLinux

  (require 'sdcv-pure)

  (defvar sdcv-simple-dict
    `(,(expand-file-name "misc/stardict-lazyworm-ec-2.4.2" user-emacs-directory))
    "Dictionary to search")

  (defvar sdcv-multiple-dicts
    `((,(expand-file-name "misc/stardict-lazyworm-ec-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-langdao-ce-gb-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-langdao-ec-gb-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-cedict-gb-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-quick_eng-zh_CN-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-DrEye4in1-2.4.2" user-emacs-directory))
      (,(expand-file-name "misc/stardict-ProECCE-2.4.2" user-emacs-directory)))
    "List of dictionaries to search.")

  (global-set-key (kbd "C-c d") 'sdcv-simple-definition)
  (global-set-key (kbd "C-c D") 'sdcv-complete-definition)
  )

(when *is-linux*
  (require 'sdcv)

  (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic/")
  (global-set-key (kbd "C-c d") 'sdcv-search-pointer)

  ;; extract my dictionaries of ~/misc/*.bz2 files to stardict dictionary folder
  ;; note: the extraction will not happen if ~/misc/extracted.txt exists
  (defun my-extract-stardict-bz2-files-on-linux ()
    (interactive)
    (let* ((dir (expand-file-name "misc" (getenv "HOME")))
	   (extracted-file (concat dir "/extracted.txt"))
	   (files
	    (if (file-directory-p dir)
		(directory-files dir nil "\\.bz2\\'")
	      nil)))
      (if (and files (not (null files)))
	  (if (or (not (file-exists-p extracted-file))
		  (not (my-file-contains-p extracted-file files)))
	      (progn
		(dolist (file files)
		  (let ((abs-file (concat dir "/" file)))
		    (shell-command
		     (format "sudo tar -xjvf %s -C /usr/share/stardict/dic" abs-file)))
		  (with-temp-buffer
		    (set-buffer-file-coding-system 'utf-8-unix)
		    (insert file)
		    (insert "\n")
		    (append-to-file (point-min) (point-max) extracted-file)
		    ))
		(my-merge-duplicated-lines-in-file extracted-file)
		(message "StarDict dictionaries extraction completed."))
	    (message "All StarDict dictionaries have already been extracted."))
	(message "The folder (misc) does not exist or does not contain any .bz2 files.")
	)))
  (my-extract-stardict-bz2-files-on-linux)

  )



(provide 'init-dict)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-dict.el ends here
