;;; init-messages.el --- *Messages* buffer settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq messages-buffer-max-lines 10000)

;; {{ START: add a timestamp to each entry in Emacs' *Messages* buffer
;; via https://emacs.stackexchange.com/a/33523
(defun sh/current-time-microseconds ()
  "Return the current time formatted to include microseconds."
  (let* ((nowtime (current-time))
	 (now-ms (format "%.3s" (nth 2 nowtime)))) ; use "%.3s" to limit to 3 characters
    (concat (format-time-string "[%Y-%m-%dT%T" nowtime) (format ".%s]" now-ms))))

(defun sh/ad-timestamp-message (FORMAT-STRING &rest args)
  "Advice to run before `message' that prepends a timestamp to each message.

Activate this advice with:
(advice-add 'message :before 'sh/ad-timestamp-message)"
  (unless (string-equal FORMAT-STRING "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
          (newline))
        (insert (sh/current-time-microseconds) " ")))))

(advice-add 'message :before 'sh/ad-timestamp-message)
;; END: add a timestamp to each entry in Emacs' *Messages* buffer }}


(provide 'init-messages)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-messages.el ends here
