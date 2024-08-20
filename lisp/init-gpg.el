;;; init-gpg.el --- GPG settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; {{ START: decrypt link at point
(require 'epa)
(require 'org-element)

(defun my/decrypt-gpg-link-at-point ()
  "Decrypt GPG link at point.

Version 2023-08-04"
  (interactive)
  (let* ((link-info (org-element-context))
	 (path (org-element-property :path link-info))
	 (abs-path (if (string-prefix-p "file:" path)
		       (file-truename (replace-regexp-in-string "^file:" "" path))
		     (file-truename path)))
	 (default-decrypt-path (concat abs-path ".clear")))
    (if (file-exists-p abs-path)
	(let ((decrypt-path (read-file-name
			     (format "Enter target path (default %s): " default-decrypt-path)
			     nil nil nil default-decrypt-path)))
	  (epa-decrypt-file abs-path decrypt-path)
	  (message "%s is decrypted to %s" abs-path decrypt-path))
      (message "File not found: %s" abs-path))))
;; END }}



(provide 'init-gpg)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-gpg.el ends here
