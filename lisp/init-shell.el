;;; init-shell.el --- Shell settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(my-check-for-executable "ShellCheck" "shellcheck") ; flycheck

(defun my/shell-mode-config ()
  (setq flycheck-select-checker "sh-shellcheck"))

(add-hook 'sh-mode-hook 'my/shell-mode-config)


(provide 'init-shell)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-shell.el ends here
