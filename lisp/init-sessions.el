;;; init-sessions.el --- save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; https://web.archive.org/web/20240509043743/http://xahlee.info/emacs/emacs/emacs_save_restore_opened_files.html

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 6
      desktop-save t
      desktop-load-locked-desktop t ; no ask if crashed
      desktop-restore-frames t
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
	enable-local-variables
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


;; make Emacs remember cursor position,
;; by default, the position records are saved to ~/.emacs.d/places
;; via https://web.archive.org/web/20240509044026/http://xahlee.info/emacs/emacs/emacs_save_cursor_position.html
(save-place-mode 1)

;; https://web.archive.org/web/20240509044606/http://xahlee.info/emacs/emacs/emacs_save_command_history.html
(use-package savehist
  ;; from https://web.archive.org/web/20240509044708/https://emacs-china.org/t/emacs/17606/9
  :init (setq enable-recursive-minibuffers t ; allow commands in minibuffers
	      history-length 1000
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 6)
  :config
  ;; by default, the command histories are saved to ~/.emacs.d/history
  (savehist-mode 1))


(provide 'init-sessions)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-sessions.el ends here
