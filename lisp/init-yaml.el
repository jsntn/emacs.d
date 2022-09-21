;;; init-yaml.el --- YAML settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
;; no-byte-compile: t
;; End:
;;; init-yaml.el ends here
