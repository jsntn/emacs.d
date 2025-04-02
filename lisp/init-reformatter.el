;;; init-reformatter.el --- reformatter settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; START: reformatter config
(require 'reformatter)

(my-check-for-executable "Prettier" "prettier")
(reformatter-define my/css-yaml-format
  :program "prettier"
  :args (list "--write" buffer-file-name)
  ;; https://emacs.stackexchange.com/questions/24298/can-i-eval-a-value-in-quote
  )

(my-check-for-executable "shfmt" "shfmt")
(reformatter-define my/sh-format
  :program "shfmt"
  :args (list "-i" "4")
  ;; 4 spaces as indent, read more https://github.com/mvdan/sh/blob/master/cmd/shfmt/shfmt.1.scd
  ;; https://emacs.stackexchange.com/questions/24298/can-i-eval-a-value-in-quote
  )

(my-check-for-executable "Black" "black")
(reformatter-define my/python-format
  :program "black"
  :args '("-"))

;; END: reformatter config


(provide 'init-reformatter)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-reformatter.el ends here
