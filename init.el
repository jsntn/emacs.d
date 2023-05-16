;;; init.el --- A Personal Emacs Configuration -*- lexical-binding: t; -*-

;; =============================================================================
;; hi@jsntn.com
;; 2020, 2021, 2022, 2023
;; =============================================================================

;;; Commentary:
;;

;;; Code:


(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "This Emacs configuration is based on v%s" minver))
  )


;; =============================================================================
;; initial settings
;; =============================================================================

;; this is used to initiate the load-path setting for further require config

;; alias emacs='emacs -q --load "/path/to/init.el"'
(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
;; refer to https://emacs.stackexchange.com/a/4258/29715

;; initiate 'lisp' folder to the load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))


;; =============================================================================
;; require settings
;; =============================================================================

(require 'init-portable) ; portable Emacs settings
(require 'init-load-path) ; load-path settings
(require 'local-var nil 'noerror) ; allow users to provide an optional
				  ; "local-var" containing personal variables

(require 'use-package) ; use-package initialization

(require 'local-packages nil 'noerror) ; allow users to provide an optional
				       ; "local-packages" containing local
				       ; packages
;; above must come before (require 'package) settings, as it involves package.el
;; which downloads packages from the package-archives

(require 'init-speed-up) ; speed-up settings


;; =============================================================================
;; package management
;; =============================================================================

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")))

;; install straight.el
;; https://github.com/radian-software/straight.el#getting-started
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(setq use-package-always-ensure t) ; to install the package if it is not
				   ; installed

;; enable the use-package extension to ensure system binaries exist alongside package declarations
(use-package use-package-ensure-system-package
  :ensure t ; install the package if not available
  :config
  (setq system-packages-use-sudo t)
  )

(message "%s" (org-version))

(require 'init-packages) ; package management by using use-package


;; =============================================================================
;; require settings
;; =============================================================================

(require 'init-display) ; display settings
(require 'init-encryption) ; encryption settings
(require 'init-font) ; font settings
(require 'init-ibuffer) ; IBuffer mode settings
(require 'init-keybindings) ; keybindings with general.el
(require 'init-org) ; Org-mode settings
(require 'init-plantuml) ; PlantUML settings
(require 'init-python) ; Python settings
(require 'init-sessions) ; session settings
(require 'init-shell) ; Shell settings
(require 'init-spelling) ; spelling settings
(require 'init-utils) ; utils configuration
(require 'init-yaml) ; YAML settings

(require 'init-hooks) ; hooks settings
(require 'init-misc) ; miscellaneous settings

(require 'local-config nil 'noerror) ; allow users to provide an optional
				     ; "local-config" containing personal
				     ; settings


;; =============================================================================
;; footer
;; =============================================================================

(when (file-exists-p custom-file)
  ;; stop adding "custom" fields to the end
  ;; variables configured via the interactive 'customize' interface
  (load custom-file))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init.el ends here
