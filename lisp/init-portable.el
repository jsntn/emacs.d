;;; init-portable.el --- portable Emacs settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715

;; initiate 'lisp' folder to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(setq site-lisp-dir (expand-file-name "site-lisp/" user-emacs-directory)) ; define
									  ; site-lisp-dir

(setq auto-save-list-file-prefix
      (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

(setq abbrev-file-name
      ;; tell emacs where to read abbrev
      (expand-file-name "abbrev_defs" user-emacs-directory))

;; install into separate package dirs for each Emacs version, to prevent
;; bytecode incompatibility
;; https://github.com/purcell/emacs.d/blob/a8f2a45015bd8bca82f90747d40dcb593957ee01/lisp/init-elpa.el#L9
(setq package-user-dir
      (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			user-emacs-directory))

(setq custom-file (locate-user-emacs-file "custom.el"))


(provide 'init-portable)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-portable.el ends here
