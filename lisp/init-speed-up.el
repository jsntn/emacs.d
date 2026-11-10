;;; init-speed-up.el --- speed-up settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; `garage-collection-messages is not an optimization, it will actually slow
;; down the garbage collector. only activate it for debugging purposes!
;; https://github.com/doomemacs/doomemacs/issues/3108#issuecomment-627537230
;; (setq garbage-collection-messages t)

;; https://web.archive.org/web/20231109122321/http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
(defun my/minibuffer-setup-hook ()
  ;; https://web.archive.org/web/20231109123003/http://clhs.lisp.se/Body/v_most_p.htm
  (setq gc-cons-threshold most-positive-fixnum)
  )

(defun my/minibuffer-exit-hook ()
  ;; defer it (1sec) so that commands launched immediately after will enjoy the
  ;; benefits.
  ;; https://github.com/doomemacs/doomemacs/blob/35865ef5e89442e3809b8095199977053dd4210f/docs/faq.org#how-does-doom-start-up-so-quickly
  (run-at-time 1 nil
	       (lambda () (setq gc-cons-threshold 16777216)) ; 16mb
	       )
  )

(add-hook 'minibuffer-setup-hook #'my/minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my/minibuffer-exit-hook)


;; { START: 优化 Emacs 的垃圾搜集行为
;; 原来这里挂了一个 `run-with-idle-timer 15 t'，空闲每 15 秒强制跑一次
;; `garbage-collect' 并打印耗时。gcmh（init-packages.el）接管 GC 策略之后
;; 这段只剩副作用：每次完整 GC 约 2 秒，空闲时反复触发，还不断往
;; *Messages* 里写消息（该缓冲区曾涨到 1.4 MB）。保留宏以备手动测量。
;; (setq garbage-collection-messages t)
;; (setq garbage-collection-messages nil)

;; This macro measures the time it takes to evaluate a body of code.
(defmacro measure-time (&rest body)
  "Measure and return the time it takes evaluating BODY."
  `(let ((start-time (current-time)))
     ,@body
     (float-time (time-since start-time))))

;; END: 优化 Emacs 的垃圾搜集行为 }



(provide 'init-speed-up)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-speed-up.el ends here
