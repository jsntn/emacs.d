;;; init-deps.el --- dependencies management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(defcustom my-install-deps
  '((aspell
     :darwin-command "brew install aspell"
     :message "aspell is needed in this configuration file, check/install it manually."
     :enabled t)
    (shellcheck
     :darwin-command "brew install shellcheck"
     :message "shellcheck is needed in this configuration file, check/install it manually."
     :enabled t)
    (shfmt
     :darwin-command "brew install shfmt"
     :message "shfmt is needed in this configuration file, check/install it manually."
     :enabled t)
    (sqlite3
     :darwin-command "brew install sqlite"
     :message "sqlite3 is needed in this configuration file, check/install it manually."
     :enabled t)
    (ripgrep
     :darwin-command "brew install ripgrep"
     :message "ripgrep is needed in this configuration file, check/install it manually."
     :enabled t)
    (js-yaml
     :linux-command "sudo npm install -g js-yaml"
     :message nil ;; No message needed for js-yaml
     :enabled t)
    (pyright
     ;; :linux-command "sudo npm install -g pyright"
     :linux-command "pipx install pyright"
     :message nil ;; No message needed for pyright
     :enabled t)
    (prettier
     :linux-command "sudo npm install -g prettier"
     :message nil ;; No message needed for prettier
     :enabled t)))

(defun my-install-dependency (name command)
  "Install a dependency NAME using COMMAND if it is not already installed.
Display the MESSAGE if installation is skipped."
  (unless (executable-find name)
    (if (y-or-n-p (format "Install %s? " name))
        (progn
          (message (format "Installing %s..." name))
          (shell-command command))
      (message "Skipping %s installation." name))))

(defun my-install-all-deps ()
  "Install enabled Emacs dependencies."
  (interactive)
  (dolist (dep my-install-deps)
    (let* ((name (car dep))
           (command-darwin (plist-get dep :darwin-command))
           (command-linux (plist-get dep :linux-command))
           (command (plist-get dep :command))
           (message (plist-get dep :message))
           (enabled (plist-get dep :enabled)))
      (when enabled
        (cond
         ((and command-darwin (eq system-type 'darwin))
          (my-install-dependency name command-darwin))
         ((and command-linux (eq system-type 'gnu/linux))
          (my-install-dependency name command-linux))
         (unless (and command-darwin command-linux)
	   (when message
            (message message))))))))


(provide 'init-deps)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-deps.el ends here
