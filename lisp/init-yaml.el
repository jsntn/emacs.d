;;; init-yaml.el --- YAML settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(unless (executable-find "js-yaml")
  (yes-or-no-p "Please be informed the js-yaml is used in this configuration, but the js-yaml executable file is not found.
You need to install it manually. Continue?")
  )
(defun my/yaml-mode-config ()
  (setq flycheck-select-checker "yaml-jsyaml")
  (setq auto-mode-alist
	(append
	 '(("\\.yml\\'" . yaml-mode))
	 '(("\\.yaml\\'" . yaml-mode))
	 auto-mode-alist)))

(add-hook 'yaml-mode-hook 'my/yaml-mode-config)


(provide 'init-yaml)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-yaml.el ends here
