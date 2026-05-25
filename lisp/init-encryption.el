;;; init-encryption.el --- encryption settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; `org-crypt` configurations
(require 'org-crypt)

;; NOTE: We don't use `org-crypt-use-before-save-magic' because it relies on
;; `org-scan-tags', which skips entries in files with #+filetags: :ARCHIVE:.
;; Instead, we use a regex-based scan to find :crypt: headings directly.
(add-hook 'before-save-hook
          (lambda ()
            (when (derived-mode-p 'org-mode)
              (org-with-wide-buffer
               (goto-char (point-min))
               (while (re-search-forward
                       (format "^\\*+ .*:%s:" org-crypt-tag-matcher) nil t)
                 (org-encrypt-entry))))))
(setq org-tags-exclude-from-inheritance '("crypt"))

(if (boundp 'org-crypt-key-mail)
    ;; GPG key to use for encryption
    ;; either the Key ID or set to nil to use symmetric encryption
    ;; org-crypt-key-mail can be set in your local-var.el, like,
    ;; (setq org-crypt-key-mail "test@example.com")
    (setq org-crypt-key (symbol-value 'org-crypt-key-mail))
  (setq org-crypt-key nil)
  )

(setq auto-save-default nil)
;; Auto-saving does not cooperate with org-crypt.el: so you need to turn it off
;; if you plan to use org-crypt.el quite often. Otherwise, you'll get an
;; (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;; # -*- buffer-auto-save-file-name: nil; -*-


(provide 'init-encryption)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-encryption.el ends here
