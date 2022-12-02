;;; init-spelling.el --- spelling settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(unless (executable-find "aspell")
  (yes-or-no-p "Please be informed the Aspell is used in this configuration file, but the Aspell executable file is not found.
You need to install it manually. Continue?")
  )

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
