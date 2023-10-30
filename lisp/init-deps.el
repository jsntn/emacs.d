;;; init-deps.el --- dependencies management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;;; TODO:
;; - dep: shred
;; - dep: truecrypt/veracrypt


;;; NOTE: specific package manager is required,
;; - macOS: brew
;; - Linux: pacman
;; - Windows: scoop

;; (my-check-for-executable "Homebrew (macOS)" "brew")
;; (my-check-for-executable "npm (macOS/Linux)" "npm")


(defvar my-install-deps
  '((aspell
     :darwin-command "brew install aspell" ; TODO: is the en dictionary installed automatically by brew?
     :linux-command "sudo pacman -S --noconfirm aspell aspell-en"
     :windows-command "scoop install aspell"
     :message "aspell is needed in this configuration file, check/install it manually."
     :enabled t)
    (ctags
     :darwin-command "brew install universal-ctags"
     :linux-command "sudo pacman -S --noconfirm ctags"
     :windows-command "scoop install universal-ctags"
     :message "ctags is needed in this configuration file, check/install it manually."
     :enabled t)
    (languagetool
     :darwin-command "brew install languagetool"
     :linux-command "sudo pacman -S --noconfirm languagetool"
     ;; :windows-command ""
     :message nil
     :enabled t)
    (less
     :darwin-command "brew install less"
     :linux-command "sudo pacman -S --noconfirm less"
     :windows-command "scoop install less"
     :message nil
     :enabled t)
    (npm
     :darwin-command "brew install node"
     :linux-command "sudo pacman -S --noconfirm nodejs npm"
     :windows-command "scoop install nodejs"
     :message nil
     :enabled t)
    (sbcl
     :darwin-command "brew install sbcl"
     :linux-command "sudo pacman -S --noconfirm sbcl"
     :windows-command "scoop install sbcl"
     :message nil
     :enabled t)
    (shellcheck
     :darwin-command "brew install shellcheck"
     :linux-command "sudo pacman -S --noconfirm shellcheck"
     :windows-command "scoop install shellcheck"
     :message "shellcheck is needed in this configuration file, check/install it manually."
     :enabled t)
    (shfmt
     :darwin-command "brew install shfmt"
     ;; :linux-command "sudo snap install shfmt"
     :linux-command "sudo pacman -S --noconfirm shfmt"
     :windows-command "scoop install shfmt"
     :message "shfmt is needed in this configuration file, check/install it manually."
     :enabled t)
    (sqlite3
     :darwin-command "brew install sqlite"
     :linux-command "sudo pacman -S --noconfirm sqlite"
     :windows-command "scoop install sqlite"
     :message "sqlite3 is needed in this configuration file, check/install it manually."
     :enabled t)
    (stardict
     :darwin-command "brew install stardict"
     :linux-command "sudo pacman -S --noconfirm stardict"
     ;; :windows-command ""
     :message nil
     :enabled t)
    (sdcv
     :darwin-command "brew install sdcv"
     :linux-command "sudo pacman -S --noconfirm sdcv"
     ;; :windows-command ""
     :message nil
     :enabled t)
    (rg
     :darwin-command "brew install ripgrep"
     :linux-command "sudo pacman -S --noconfirm ripgrep"
     :windows-command "scoop install ripgrep"
     :message "ripgrep (rg) is needed in this configuration file, check/install it manually."
     :enabled t)
    (js-yaml
     :darwin-command "npm install js-yaml"
     :linux-command "sudo npm install -g js-yaml"
     :windows-command "npm install -g js-yaml"
     :message nil ;; No message needed for js-yaml
     :enabled t)
    (pyright
     :darwin-command "brew install pyright"
     ;; :linux-command "pipx install pyright"
     :linux-command "sudo pacman -S --noconfirm pyright"
     ;; :windows-command ""
     :message nil ;; No message needed for pyright
     :enabled t)
    (prettier
     :darwin-command "brew install prettier"
     :linux-command "sudo pacman -S --noconfirm prettier"
     :windows-command "npm install -g prettier"
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
	   (command-windows (plist-get value :windows-command))
	   (msg (plist-get value :message))
	   (enabled (plist-get value :enabled)))
      (when enabled
	(cond
	 ((and (eq system-type 'darwin)
	       command-darwin
	       (not (string-empty-p command-darwin)))
	  (my-install-dependency name command-darwin))
	 ((and (eq system-type 'gnu/linux)
	       command-linux
	       (not (string-empty-p command-linux)))
	  (my-install-dependency name command-linux))
	 ((and (eq system-type 'windows-nt)
	       command-windows
	       (not (string-empty-p command-windows)))
	  (my-install-dependency name
				 (format
				  "powershell -Command \"%s\""
				  command-windows)))
	 (t
	  (let* ((msg-content
		  (if msg
		      msg
		    (format "%s executable is needed in this configuration file,
check/install it manually." name)))
	       (prompt-msg (concat msg-content " Press ENTER to continue.")))
	    (when (string= (read-string prompt-msg) "")
	      (message "Continuing..."))
	    )))))))


(progn
  (when *is-win*
    (my-check-for-executable "Scoop (Windows)" "scoop"))
  (when *is-mac*
    (my-check-for-executable "Homebrew (macOS)" "brew"))
  (when *is-linux*
    (my-check-for-executable "npm (macOS/Linux)" "npm"))
  (my-install-all-deps))


(provide 'init-deps)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-deps.el ends here
