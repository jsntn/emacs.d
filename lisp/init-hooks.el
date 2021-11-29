;;; init-hooks.el --- hooks settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defun jsntn/hs-hide-all ()
  (hs-minor-mode 1)
  (hs-hide-all)
  (set (make-variable-buffer-local 'my-hs-hide-all) t)
  (set (make-variable-buffer-local 'my-hs-hide-block) t))

(dolist (hook '(
		prog-mode-hook
		sh-mode-hook
		lisp-mode-hook
		))
  (add-hook hook 'jsntn/hs-hide-all))

(dolist (hook '(
		prog-mode-hook
		text-mode-hook
		css-mode-hook
		lisp-mode-hook
		))
  (add-hook hook 'my/show-trailing-whitespace))

(dolist (hook '(
		css-mode-hook
		php-mode-hook
		html-mode-hook
		prog-mode-hook
		))
  (add-hook hook 'xah-syntax-color-hex))

(dolist (hook '(
		yaml-mode-hook
		python-mode-hook
		sh-mode-hook
		))
  (add-hook hook 'flycheck-mode))

(dolist (hook '(
		prog-mode-hook
		org-mode-hook
		))
  (add-hook hook 'hl-todo-mode))


(provide 'init-hooks)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-hooks.el ends here
