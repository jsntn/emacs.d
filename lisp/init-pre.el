;;; init-pre.el --- pre-startup settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; pre settings: needed by other configurations

(defconst *is-mac* (eq system-type 'darwin))
(defconst *is-win* (eq system-type 'windows-nt))
(defconst *is-linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )

;; { -- START --
;; check Linux distribution
;; https://emacs.stackexchange.com/questions/18205/how-can-i-distinguish-between-linux-distributions-in-emacs

(defun which-linux-release-info (info-type)
  "Get information about the Linux distributor or release.
   Information types: 'distributor', 'release'"

  ;; This function is improved by ChatGPT :)
  ;; Explanation from ChatGPT,

  ;; This function `which-linux-release-info` can be called interactively or
  ;; used in other functions, passing an argument to indicate which information
  ;; type ("distributor" or "release") to retrieve. For example, to get the
  ;; Linux distributor, call `(which-linux-release-info "distributor")`. The
  ;; function will return the output of running the `lsb_release` command with
  ;; the appropriate option based on the information type specified. Note that
  ;; this will only work on Linux systems and will not work on other operating
  ;; systems.
  (interactive "MInformation type (distributor/release): ")
  (when (eq system-type 'gnu/linux)
    (let ((command (pcase info-type
		     ("distributor" "lsb_release -si")
		     ("release" "lsb_release -sr")
		     (_ (error "Invalid information type: %s" info-type)))))
      (shell-command-to-string (concat "echo -n $(" command ")")))))
;; -- END -- }



(defun my/set-var (var &rest values)
  "Set VAR based on the operating system using a list of
VALUES. VALUES should be a list of pairs where the car is the
operating system identifier ('win', 'mac', 'linux') and the cdr
is the value associated with that operating system.

Version: 2023-09-24"
  (let* ((os (cond ((eq system-type 'windows-nt) 'win)
		   ((eq system-type 'gnu/linux) 'linux)
		   ((eq system-type 'darwin) 'mac)
		   (t (error "Unsupported operating system")))))
    (cl-loop for (os-id . value) in values
	     when (eq os os-id)
	     do (set var (if (stringp value)
			     value
			   (eval value))))))


(defun my-check-for-executable (executable-name executable-file &optional message)
  "Check if the given EXECUTABLE-FILE is available. If it's not found,
prompt the user with the optional MESSAGE (or a default message) to install it."
  (let* ((default-message
	   (format "Please be informed that %s is used in this configuration, \
but the %s executable file is not found. You need to install it manually."
		   executable-name executable-file))
	 (msg (or message default-message))
	 (noninteractive-msg msg)
	 (prompt-msg (concat msg " Press ENTER to continue.")))
    (unless (executable-find executable-file)
      (if noninteractive
	  (message noninteractive-msg)
	(unless (string= (read-string prompt-msg) "")
	  (message "Continuing..."))))))



(defun my-check-for-font (font-name &optional message)
  "Check if the given FONT-NAME is available. If it's not found, prompt the user with the optional MESSAGE
  to continue."
  (let* ((default-message
	   (format "The '%s' font is recommended for this configuration. \
Press ENTER to continue." font-name))
	 (prompt-msg (or message default-message)))
    (unless (member font-name (font-family-list))
      (unless (string= (read-string prompt-msg) "")
	(message "Continuing...")))))



(defun my-async-shell-command-with-unique-buffer-name (command)
  "Execute an asynchronous shell command and display its output in a unique buffer.

This function prompts the user for a shell command and then executes it
asynchronously. The output of the command is displayed in a buffer with a
unique name, incorporating the provided command and a timestamp. The buffer
name is of the form '*Async Command - COMMAND - TIMESTAMP*', where COMMAND is
the entered shell command and TIMESTAMP is the current date and time in the
format 'YYYY-MM-DD HH:MM:SS:NNN'.

Version: 2023-08-16"
  (interactive "sShell command: ")
  (let ((buffer-name
         (concat "*Async Command - " command " - "
                 (format-time-string "%Y-%m-%d %H:%M:%S:%3N") "*")))
    (async-shell-command command buffer-name)))



(provide 'init-pre)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-pre.el ends here
