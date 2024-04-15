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


(use-package auto-capitalize
  :straight (:host github :repo "yuutayamada/auto-capitalize-el")
  :config
  (setq auto-capitalize-words `("I" "English"))
  ;; this configuration adds capitalized words of .aspell.en.pws
  (setq auto-capitalize-aspell-file (expand-file-name "misc/aspell.en.pws" user-emacs-directory))
  (auto-capitalize-setup)
  ;; (add-hook 'after-change-major-mode-hook 'auto-capitalize-mode)
  :hook (org-mode .  auto-capitalize-mode)
  )


(use-package ta
  :delight
  ;; :config
  ;; (mapc (lambda (mode-hook) (add-hook mode-hook 'ta-mode))
  ;; 	'(org-mode-hook
  ;; 	  markdown-mode-hook
  ;; 	  rst-mode-hook))
  ;; (define-key ta-mode-map (kbd "M-o") 'ta-next-homophony)
  )


(provide 'init-spelling)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-spelling.el ends here
