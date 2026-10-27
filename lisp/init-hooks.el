;;; init-hooks.el --- hooks settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; highlight current line
(dolist (hook '(
		prog-mode-hook
		;; text-mode-hook
		org-agenda-finalize-hook
		))
  (add-hook hook #'hl-line-mode))

(defun my/hs-hide-all ()
  "Enable hideshow and hide all, for hideshow-capable major modes only.
Silently no-op for modes without hideshow support (e.g. tree-sitter modes
such as `js-ts-mode'), so a startup `js-ts-mode' buffer no longer throws and
opens a `*Backtrace*' debugger."
  (condition-case nil
      (progn
        (hs-grok-mode-type)   ; throws for unsupported modes: bail before any state
        (hs-minor-mode 1)
        (hs-hide-all)
        (set (make-variable-buffer-local 'my-hs-hide-all) t)
        (set (make-variable-buffer-local 'my-hs-hide-block) t))
    (error nil)))

(dolist (hook '(
		prog-mode-hook
		sh-mode-hook
		lisp-mode-hook
		))
  (add-hook hook 'my/hs-hide-all))

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


(dolist (hook '(
		find-file-hook
		after-save-hook
		switch-buffer-hook
		))
  (add-hook hook 'my/readonly-files))


(dolist (my-org-mode-hook-settings '(
		my/modify-org-done-face
		))
  (add-hook 'org-mode-hook my-org-mode-hook-settings))

;; undo in non-file buffers
(add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)


(provide 'init-hooks)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-hooks.el ends here
