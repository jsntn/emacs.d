;;; init-python.el --- Python settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:




(use-package pyvenv
  :config
  ;; (pyvenv-mode t)

  ;; set correct Python interpreter
  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(if (equal system-type 'windows-nt)
		    (setq python-shell-interpreter (concat pyvenv-virtual-env "Scripts/python"))
		  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python"))
		  )
		)))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python")
		)))
  )


(defun my/python-mode-config ()
  (setq python-indent-offset 4
	python-indent 4
	indent-tabs-mode nil
	default-tab-width 4
	flycheck-select-checker "python-flake8")
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
  (auto-fill-mode 1)
  (set (make-local-variable 'electric-indent-mode) nil)
  )

(add-hook 'python-mode-hook 'my/python-mode-config)


(provide 'init-python)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-python.el ends here
