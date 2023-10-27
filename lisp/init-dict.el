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


(if *is-linux*
    (use-package sdcv
      :straight (:host github :repo "manateelazycat/sdcv")
      :config
      (setq sdcv-dictionary-data-dir "/usr/share/stardict/dic/")
      (global-set-key (kbd "C-c d") 'sdcv-search-pointer)

      ;; extract my dictionaries of ~/misc/*.bz2 files to stardict dictionary folder
      (defun my-extract-stardict-bz2-files-on-linux ()
	(interactive)
	(let* ((dir (expand-file-name "misc" (getenv "HOME")))
	       (extracted-file (concat dir "/extracted.txt"))
	       (files (directory-files dir nil "\\.bz2\\'")))
;; FIX: if misc folder and .bz2 files don't exist...
;; TODO: to be tested...
(when (and (file-directory-p dir) (not (null files)))
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
	    (message "All StarDict dictionaries have already been extracted.")))))
      (my-extract-stardict-bz2-files-on-linux)

      ))
 


(provide 'init-dict)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-dict.el ends here