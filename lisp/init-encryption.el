;;; init-encryption.el --- encryption settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; reference: https://www.reddit.com/r/emacs/comments/10mk42s/issue_with_org_and_straight/
(message "%s" (org-version))
(use-package org :straight (:type built-in))
(message "%s" (org-version))
;; `org-crypt` configurations
(require 'org-crypt) ; require org-crypt

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
