;;; init-speed-up.el --- speed-up settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))


(provide 'init-speed-up)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-speed-up.el ends here
