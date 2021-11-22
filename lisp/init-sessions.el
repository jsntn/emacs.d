;;; init-sessions.el --- save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600
      desktop-save t
      desktop-restore-eager 10 ; the maximum number of 10 buffers to restore
			       ; immediately, and the remaining buffers are
			       ; restored lazily, when Emacs is idle.
      )
(desktop-save-mode 1)

(add-hook 'emacs-startup-hook 'desktop-read)

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 50)
        (compile-history          . 30)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 30)
        (face-name-history        . 20)
        (file-name-history        . 100)
        (grep-find-history        . 30)
        (grep-history             . 30)
        (ivy-history              . 100)
        (magit-revision-history   . 50)
        (minibuffer-history       . 50)
        (org-clock-history        . 50)
        (org-refile-history       . 50)
        (org-tags-history         . 50)
        (query-replace-history    . 60)
        (read-expression-history  . 60)
        (regexp-history           . 60)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 50)
        tags-file-name
        tags-table-list))


(provide 'init-sessions)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-sessions.el ends here
