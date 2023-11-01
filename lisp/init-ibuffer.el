;;; init-ibuffer.el --- IBuffer settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq ibuffer-default-sorting-mode 'recency)

(defun ibuffer-jump-to-last-buffer ()
  (ibuffer-jump-to-buffer (buffer-name (cadr (buffer-list)))))
(add-hook 'ibuffer-hook #'ibuffer-jump-to-last-buffer)

(with-eval-after-load 'ibuffer
  ;; use human readable size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 35 35 :left :nil) " "
              (size-h 9 -1 :right) " "
              (mode 12 12 :left :elide) " "
              filename-and-process)))


(provide 'init-ibuffer)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-ibuffer.el ends here
