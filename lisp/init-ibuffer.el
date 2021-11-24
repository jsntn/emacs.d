;;; init-ibuffer.el --- IBuffer settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq ibuffer-default-sorting-mode 'recency)

(defun ibuffer-jump-to-last-buffer ()
  (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
(add-hook 'ibuffer-hook #'ibuffer-jump-to-last-buffer)


(provide 'init-ibuffer)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-ibuffer.el ends here
