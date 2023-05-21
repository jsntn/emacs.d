;;; init-pre.el --- pre-startup settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



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



(defun my-check-for-executable (executable-name executable-file &optional message)
  "Check if the given EXECUTABLE-FILE is available. If it's not found, prompt the user with the optional MESSAGE
  (or a default message) to install it."
  (let* ((default-message (format "Please be informed that %s is used in this configuration, but the %s executable file is not found. You need to install it manually."
                                  executable-name executable-file))
         (msg (or message default-message))
         (noninteractive-msg msg)
         (prompt-msg (concat msg " Press ENTER to continue.")))
    (unless (executable-find executable-file)
      (if noninteractive
          (message noninteractive-msg)
        (unless (string= (read-string prompt-msg) "")
          (message "Continuing..."))))))


 
;; TODO: to be tested...
(defun my-check-for-font (font-name &optional message)
  "Check if the given FONT-NAME is available. If it's not found, prompt the user with the optional MESSAGE
  to continue."
  (let* ((default-message (format "The '%s' font is recommended for this configuration. Press ENTER to continue." font-name))
         (prompt-msg (or message default-message)))
    (unless (member font-name (font-family-list))
      (unless (string= (read-string prompt-msg) "")
        (message "Continuing...")))))




(provide 'init-pre)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-pre.el ends here