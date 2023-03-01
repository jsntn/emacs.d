;;; init-speed-up.el --- speed-up settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; `garage-collection-messages is not an optimization, it will actually slow
;; down the garbage collector. only activate it for debugging purposes!
;; https://github.com/doomemacs/doomemacs/issues/3108#issuecomment-627537230
;; (setq garbage-collection-messages t)

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold
(defun my/minibuffer-setup-hook ()
  ;; http://clhs.lisp.se/Body/v_most_p.htm
  (setq gc-cons-threshold most-positive-fixnum))

(defun my/minibuffer-exit-hook ()
  ;; defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  ;; https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold 16777216))) ; 16mb
   )

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)


(provide 'init-speed-up)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-speed-up.el ends here
