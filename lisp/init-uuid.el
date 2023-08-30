;;; init-uuid.el --- UUID settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




;; <2023-04-18 Tue 17:55> now below functions are verified to work, but the
;; arbitrary position to other file is not working...

(defun my/quick-generate-uuid (&optional universal-arg)
  "Generate a UUID, insert at point, and copy to kill ring when UNIVERSAL-ARG (C-u) is provided.

Usage:
M-x my/quick-generate-uuid
C-u M-x my/quick-generate-uuid

Version 2023-04-18
Updated 2023-07-25"
  (interactive "P")
  (let* ((time (current-time))
         (random-bytes (make-vector 16 0))
         (hash (secure-hash 'sha256 (concat (format "%s" time)
                                            (format "%s" random-bytes))))
         (uuid (format "%08x-%04x-%04x-%04x-%012x"
                       (string-to-number (substring hash 0 8) 16)
                       (string-to-number (substring hash 8 12) 16)
                       (string-to-number (substring hash 12 16) 16)
                       (logior (string-to-number (substring hash 16 20) 16) #b01000000)
                       (string-to-number (substring hash 20 32) 16))))
    (insert uuid)
    (when universal-arg
      (kill-new uuid)
      (message "%s is copied." uuid))
    (message "UUID generated: %s" uuid)))

;; { -- START: for my/quick-insert-uuid-link
;; From ChatGPT,

;; The concat function in (let ((uuid (concat "id:" (current-kill 0)))) adds the
;; "id:" prefix only once, so there will be no situation where the link is
;; duplicated.

;; In addition, the org-insert-link function also checks for existing "id:"
;; prefixes, so it will not add another one if the UUID already starts with
;; "id:".

;; Here's the relevant code from the org-insert-link function:

;; (cond ((string-match-p (rx bos "id:") link)
       ;; (concat "[" desc "]" link))
      ;; ((string-match-p (rx bos "attachment:") link)
       ;; (concat "[" desc "]" link))
      ;; (t
       ;; (concat "[" desc "]" (org-make-link-string link type link))))

;; As you can see, if the link already starts with "id:", it simply concatenates
;; the description and the link and returns it. Otherwise, it constructs a link
;; string using org-make-link-string.

(defun my/quick-insert-uuid-link (&optional universal-arg)
  "Insert a link to the UUID on the kill ring.
   Prompt for a link name if UNIVERSAL-ARG is non-nil.

Usage:
M-x my/quick-insert-uuid-link  Insert anonymous link
C-u M-x my/quick-insert-uuid-link  Prompt for link name

Version 2023-04-18"
  (interactive "P")
  (let ((uuid (concat "id:" (current-kill 0))))
    (if universal-arg
        (org-insert-link "id" uuid (read-string "Link name: "))
      (org-insert-link "id" uuid nil))))

;; -- END: for my/quick-insert-uuid-link }


(provide 'init-uuid)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-uuid.el ends here