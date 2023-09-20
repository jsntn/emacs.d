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
  (setq gc-cons-threshold most-positive-fixnum)
  )

(defun my/minibuffer-exit-hook ()
  ;; defer it (1sec) so that commands launched immediately after will enjoy the
  ;; benefits.
  ;; https://github.com/doomemacs/doomemacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
  (run-at-time 1 nil
	       (lambda () (setq gc-cons-threshold 16777216)) ; 16mb
	       )
  )

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)


;; { START: 优化 Emacs 的垃圾搜集行为
;; https://github.com/lujun9972/lujun9972.github.com/blob/81a7933b05495155a601b9b57991ca32d12c95a5/Emacs%E4%B9%8B%E6%80%92/%E4%BC%98%E5%8C%96Emacs%E7%9A%84%E5%9E%83%E5%9C%BE%E6%90%9C%E9%9B%86%E8%A1%8C%E4%B8%BA.org
;; original link https://akrl.sdf.org

;; (setq garbage-collection-messages t)
;; (setq garbage-collection-messages nil)

;; This macro measures the time it takes to evaluate a body of code.
(defmacro measure-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((start-time (current-time)))
     ,@body
     (float-time (time-since start-time))))

;; This variable sets a timer to run the garbage collector after 15 seconds
;; of idling.
(defvar gc-timer
  (run-with-idle-timer 15 t
    (lambda ()
      (message "Garbage collector has run for %.06f seconds"
               (measure-time (garbage-collect))))))

;; END: 优化 Emacs 的垃圾搜集行为 }



(provide 'init-speed-up)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-speed-up.el ends here
