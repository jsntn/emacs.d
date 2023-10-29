;;; init-spelling.el --- spelling settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package ispell)

(my-check-for-executable "Aspell" "aspell")

(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  ;; please note ispell-extra-args contains ACTUAL parameters passed to aspell
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US"))
  )

(use-package ta
  :delight
  :config
  (mapc (lambda (mode-hook) (add-hook mode-hook 'ta-mode))
	'(org-mode-hook
	  markdown-mode-hook
	  rst-mode-hook))
  (define-key ta-mode-map (kbd "M-o") 'ta-next-homophony)
  )


(provide 'init-spelling)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-spelling.el ends here
