;;; init-encryption.el --- encryption settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; `org-crypt` configurations
(let ((straight-current-profile 'pinned))
  (straight-use-package 'org)
  (straight-use-package 'org-contrib)
  ;; pin org-mode version
  (add-to-list 'straight-x-pinned-packages
	       '("org" . "d34f551faf5d2b50c2bbb0fcaaaab05cdf93a001"))
  (require 'org-crypt) ; require org-crypt
  )

(org-crypt-use-before-save-magic)
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
