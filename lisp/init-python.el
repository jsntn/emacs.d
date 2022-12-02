;;; init-python.el --- Python settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
