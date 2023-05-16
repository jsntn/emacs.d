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


(provide 'init-spelling)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-spelling.el ends here
