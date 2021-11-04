;;; init.el --- A personal Emacs configuration       -*- lexical-binding: t; -*-

;; ===============================================================
;;                    hi@jsntn.com
;;                        2021
;; ===============================================================

;;; Commentary:
;;

;;; Code:

;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715
