;;; init-pre.el --- pre-startup settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; TODO: to be tested...
(defun my-check-for-font (font-name &optional message)
  "Check if the given FONT-NAME is available. If it's not found, prompt the user with the optional MESSAGE
  to continue."
  (let* ((default-message (format "The '%s' font is recommended for this configuration. Press ENTER to continue." font-name))
         (prompt-msg (or message default-message)))
    (unless (member font-name (font-family-list))
      (unless (string= (read-string prompt-msg) "")
        (message "Continuing...")))))




(provide 'init-pre)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-pre.el ends here