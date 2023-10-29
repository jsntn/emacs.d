;;; init-deps.el --- dependencies management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;;; TODO:
;; - dep: shred
;; - dep: truecrypt/veracrypt
;; - deps installation on Windows OS


;;; NOTE: specific package manager is required,
;; - macOS: brew, npm
;; - Linux: pacman, npm
;; - TODO: - Windows: scoop

;; (my-check-for-executable "Homebrew (macOS)" "brew")
;; (my-check-for-executable "npm (macOS/Linux)" "npm")


(defvar my-install-deps
  '((aspell
     :darwin-command "brew install aspell" ; TODO: is the en dictionary installed automatically by brew?
     :linux-command "sudo pacman -S --noconfirm aspell aspell-en"
     :message "aspell is needed in this configuration file, check/install it manually."
     :enabled t)
    (ctags
     :darwin-command "brew install universal-ctags"
     :linux-command "sudo pacman -S --noconfirm ctags"
     :message "ctags is needed in this configuration file, check/install it manually."
     :enabled t)
    (languagetool
     :darwin-command "brew install languagetool"
     :linux-command "sudo pacman -S --noconfirm languagetool"
     :message nil
     :enabled t)
    (less
     :darwin-command "brew install less"
     :linux-command "sudo pacman -S --noconfirm less"
     :message nil
     :enabled t)
    (npm
     :darwin-command "brew install node"
     :linux-command "sudo pacman -S --noconfirm nodejs npm"
     :message nil
     :enabled t)
    (sbcl
     :darwin-command "brew install sbcl"
     :linux-command "sudo pacman -S --noconfirm sbcl"
     :message nil
     :enabled t)
    (shellcheck
     :darwin-command "brew install shellcheck"
     :linux-command "sudo pacman -S --noconfirm shellcheck"
     :message "shellcheck is needed in this configuration file, check/install it manually."
     :enabled t)
    (shfmt
     :darwin-command "brew install shfmt"
     ;; :linux-command "sudo snap install shfmt"
     :linux-command "sudo pacman -S --noconfirm shfmt"
     :message "shfmt is needed in this configuration file, check/install it manually."
     :enabled t)
    (sqlite3
     :darwin-command "brew install sqlite"
     :linux-command "sudo pacman -S --noconfirm sqlite"
     :message "sqlite3 is needed in this configuration file, check/install it manually."
     :enabled t)
    (stardict
     :darwin-command "brew install stardict"
     :linux-command "sudo pacman -S --noconfirm stardict"
     :message nil
     :enabled t)
    (sdcv
     :darwin-command "brew install sdcv"
     :linux-command "sudo pacman -S --noconfirm sdcv"
     :message nil
     :enabled t)
    (rg
     :darwin-command "brew install ripgrep"
     :linux-command "sudo pacman -S --noconfirm ripgrep"
     :message "ripgrep (rg) is needed in this configuration file, check/install it manually."
     :enabled t)
    (js-yaml
     :darwin-command "npm install js-yaml"
     :linux-command "sudo npm install -g js-yaml"
     :message nil ;; No message needed for js-yaml
     :enabled t)
    (pyright
     :darwin-command "brew install pyright"
     ;; :linux-command "pipx install pyright"
     :linux-command "sudo pacman -S --noconfirm pyright"
     :message nil ;; No message needed for pyright
     :enabled t)
    (prettier
     :darwin-command "brew install prettier"
     :linux-command "sudo pacman -S --noconfirm prettier"
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
    (let* ((name (symbol-name (car dep)))
	   (value (cdr dep))
	   (command-darwin (plist-get value :darwin-command))
	   (command-linux (plist-get value :linux-command))
	   ;; TODO: to be tested...
	   (command-windows (plist-get value :windows-command))
	   (msg (plist-get value :message))
	   (enabled (plist-get value :enabled)))
      (when enabled
	(cond
	 ((and command-darwin (eq system-type 'darwin))
	  (my-install-dependency name command-darwin))
	 ((and command-linux (eq system-type 'gnu/linux))
	  (my-install-dependency name command-linux))
	  ;; TODO: to be tested...
	 ((and command-windows (eq system-type 'windows-nt))
	  (my-install-dependency name
	  (format "powershell -Command \"%s\"" command-windows)))
	 (unless (and command-darwin command-linux)
	   (when msg
	     (message msg))))))))


(progn
  (when *is-win*
    (yes-or-no-p "Windows OS is not supported currently, input yes to continue..."))
  (my-check-for-executable "Homebrew (macOS)" "brew")
  (my-check-for-executable "npm (macOS/Linux)" "npm")
  (my-install-all-deps))


(provide 'init-deps)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-deps.el ends here
