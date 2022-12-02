;;; init-shell.el --- Shell settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(unless (executable-find "shellcheck")
  (yes-or-no-p "Please be informed the ShellCheck is used in this configuration, but the ShellCheck executable file is not found.
You need to install it manually. Continue?")
  )

(defun my/shell-mode-config ()
  (setq flycheck-select-checker "sh-shellcheck"))

(add-hook 'sh-mode-hook 'my/shell-mode-config)


(provide 'init-shell)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-shell.el ends here
