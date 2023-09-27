;;; init-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; a utility package to collect various Icon Fonts and propertize them within Emacs.
(use-package all-the-icons
  :config
  ;; check if all-the-icons is installed
  ;; reference
  ;; https://github.com/domtronn/all-the-icons.el/issues/120
  (when (display-graphic-p) ; if not in terminal Emacs
    ;; if not on Windows and all-the-icons is not installed
    (unless (equal system-type 'windows-nt)
      (unless (member "all-the-icons" (font-family-list))
	(all-the-icons-install-fonts t)
	)
      )
    ;; all-the-icons configuration
    (require 'all-the-icons))
  (setq inhibit-compacting-font-caches t)
  )

;; a quick cursor jump mode for emacs
;; keybindings:
;; [[./init-keybindings.el::ajm-1]]
;; [[./init-keybindings.el::ajm-2]]
(use-package ace-jump-mode)

;; jump to Chinese character by pinyin with `avy' or `ace-jump-mode'
(use-package ace-pinyin
  :delight
  :config
  (setq ace-pinyin-use-avy nil) ; use `ace-jump-mode'
  (ace-pinyin-global-mode +1)
  )

(use-package annotate
  :config
  (custom-set-faces
   '(annotate-annotation ((t (:background "#ff7f4f" :foreground "white"))))
   '(annotate-annotation-secondary ((t (:background "#ff7f4f" :foreground "white"))))
   '(annotate-highlight ((t (:underline "white"))))
   '(annotate-highlight-secondary ((t (:underline "white"))))
   )
  ;; { START: my-annotate-mode-hook
  (defun my-set-default-annotation-file (annotate-mode-status)
    "set my default annotation-file, which is used in case the
`.annotations' in the directory of the current buffer does not
exist."
    (interactive)
    (setq annotate-file
	  (expand-file-name ".annotations" user-emacs-directory))
    (when (eq annotate-mode-status 'on)
      (annotate-load-annotations))
    (message "annotate-mode is %s, and the annotate-file is set to %s.annotations"
	     annotate-mode-status user-emacs-directory)
    )
  (defun my-annotate-mode-hook ()
    "my annotate-mode hook to check if `.annotations' exists in the
directory of the current buffer then use it as the
`annotate-file', otherwise call the
`my-set-default-annotate-file'."
    (interactive)
    (if (bound-and-true-p annotate-mode); if annotate-mode is on
	(if (file-exists-p ".annotations") ; if .annotations file exists
	    (progn (setq-local annotate-file ".annotations")
		   (annotate-load-annotations)
		   (message "annotate-mode is on, and the annotate-file is %s.annotations"
			    (file-name-directory (buffer-file-name))))
	  (my-set-default-annotation-file 'on) ; if .annotations file does not exist
	  )
      (my-set-default-annotation-file 'off) ; if annotate-mode is off
      ))
  (add-hook 'annotate-mode-hook 'my-annotate-mode-hook)
  ;; END: my-annotate-mode-hook }
  )

(use-package auto-capitalize
  :straight (:host github :repo "yuutayamada/auto-capitalize-el")
  :config
  (setq auto-capitalize-words `("I" "English"))
  ;; this configuration adds capitalized words of .aspell.en.pws
  (setq auto-capitalize-aspell-file (expand-file-name "misc/aspell.en.pws" user-emacs-directory))
  (auto-capitalize-setup)
  ;; (add-hook 'after-change-major-mode-hook 'auto-capitalize-mode)
  :hook (org-mode .  auto-capitalize-mode)
  )

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; swap buffers, keybindings -> [[./init-keybindings.el::bm-k]]
(use-package buffer-move)

(unless (display-graphic-p)
  (use-package clipetty
    ;; clipetty is aiming at sending text that you kill in Emacs to your
    ;; Operating System's clipboard, but not the reverse,
    ;; https://github.com/spudlyo/clipetty/issues/10
    :disabled ; disable for now as I am using Alacritty and it has its own
	      ; clipboard support
    :hook (after-init . global-clipetty-mode)
    )
  )

;; { START: citre
(unless (executable-find "ctags")
  (when (eq system-type 'darwin)
    (shell-command "brew install universal-ctags"))
  (when (string= (which-linux-release-info "distributor") "Ubuntu")
    (call-process "/bin/bash"
		  (expand-file-name "scripts/ctags.sh" user-emacs-directory)))
  (yes-or-no-p "Please be informed the ctags is started to install in the background...
The installation result can be checked later manually with ctags command. Continue?")
  )

(use-package citre
  :delight
  ;; ctags should be installed first, the Universal Ctags is recommended,
  ;; https://github.com/universal-ctags/ctags
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config))

(when (or (eq system-type 'darwin) (eq system-type 'windows-nt))
  (unless (executable-find "ctags")
    (yes-or-no-p "Please be informed the ctags is used in this configuration file, but the executable file is not found.
You need to install it manually. Continue?")
    ))
;; END: citre }

(use-package cnfonts
  :if window-system ; only load this package when in graphical Emacs
  :config
  (cnfonts-mode 1)
  (setq cnfonts-profiles
	'("program" "org-mode" "read-book"))
  (setq cnfonts-use-system-type t) ; save profile config across different system-type
  )

(use-package company-tabnine
  :config
  (setq company-tabnine-binaries-folder (expand-file-name ".TabNine/" user-emacs-directory))
  ;; (add-to-list 'company-backends #'company-tabnine)
  )

(use-package company
  :delight
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  ;; number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; show suggestions after entering 3 character.
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers nil)
  (setq company-tooltip-align-annotations t)
  ;; when the list of suggestions is shown, and you go through the list of
  ;; suggestions and reach the end of the list, the end of the list of
  ;; suggestions does not wrap around to the top of the list again. This is a
  ;; minor inconvenience that can be solved:
  (setq company-selection-wrap-around t)
  ;; use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)

  (setq company-transformers '(delete-dups
			       company-sort-by-occurrence))

  (setq company-backends '(
			   (company-capf company-keywords company-dabbrev-code)
			   ;; commented below to speed up the completion
			   ;; (company-tabnine)
			   company-files)
	)

  ;; add yasnippet support for all company backends.
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; set the backends for org-mode
  (defun my-company-backends-org-mode-hook ()
    (setq-local company-backends '(
				   (company-dabbrev company-ispell)
				   ;; commented below to speed up the completion
				   ;; (company-tabnine)
				   company-files)
		))
  (add-hook 'org-mode-hook 'my-company-backends-org-mode-hook)

  ;; add `company-elisp' backend for elisp.
  ;; (add-hook 'emacs-lisp-mode-hook
  ;; 	    #'(lambda ()
  ;; 		(require 'company-elisp)
  ;; 		(push 'company-elisp company-backends)))
  ;; via https://github.com/manateelazycat/lazycat-emacs/blob/8f3dee8a6fe724ec52cd2b17155cfc2cefc8066b/site-lisp/config/init-company-mode.el




  ;; { START: company-candidates from abo-abo
  ;; if candidate list was ("var0" "var1" "var2"), then entering 1 means:
  ;; select the first candidate (i.e. "var0"), instead of:
  ;; insert "1", resulting in "var1", i.e. the second candidate
  ;; via,
  ;; - https://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    ;; via https://github.com/abo-abo/oremacs/blob/d217e22a3b8dc88d10f715b32a7d1facf1f7ae18/modes/ora-company.el#L22-L39
    (interactive)
    (let* ((k (this-command-keys))
	   (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
			  company-candidates)
	      (> (string-to-number k)
		 (length company-candidates))
	      (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
	  (self-insert-command 1)
	(company-complete-number
	 (if (equal k "0")
	     10
	   (string-to-number k))))))

  (let ((map company-active-map))
    ;; via https://github.com/abo-abo/oremacs/blob/d217e22a3b8dc88d10f715b32a7d1facf1f7ae18/modes/ora-company.el#L46-L53
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
	  (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  ;; END: company-candidates from abo-abo }
  )

(use-package company-english-helper
  :straight (:host github :repo "manateelazycat/company-english-helper")
  )


;; use this package to fix tooltip alignment issue below,
;; https://github.com/company-mode/company-mode/issues/1388
(use-package company-posframe
  :delight
  :straight (:type git :host github :repo "tumashu/company-posframe")
  :config
  (company-posframe-mode 1)
  )


(use-package counsel)

(use-package delight)

(use-package diminish)

(use-package doom-themes
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t) ; if nil, bold is universally disabled
  ;; corrects (and improves) org-mode's native fontification
  ;; (doom-themes-org-config) ; disable this as it is not compatible with
					; org-modern horizontal line, see,
					; https://github.com/jsntn/emacs.d/issues/13
  ;; personal modified version of doom-monokai-classic
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
  (load-theme 'doom-monokai-classic t)
  (set-background-color "black")
  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
  )

(use-package eglot)

;; M-x elgrep: search a single directory
;; C-u M-x elgrep: search the directory recursively
(use-package elgrep)

(use-package elpa-mirror)

(use-package emacsql-sqlite3 ; for org-roam
  :straight (:host github :repo "cireu/emacsql-sqlite3"))

;; (use-package elpy
;;   :config
;;   (elpy-enable)
;;   ;; use flycheck instead of flymake
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )

;; evil-collection assumes evil-want-keybinding is set to nil and
;; evil-want-integration is set to t before loading evil and evil-collection.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil
  :init
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil)
    )
  :after undo-tree
  :config
  (evil-set-undo-system 'undo-tree) ; https://github.com/emacs-evil/evil/issues/1372#issuecomment-712611291
  (global-undo-tree-mode)
  (evil-mode 1)
  ;; change the cursor color in terms of evil mode
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  )

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-leader
  :init
  (global-evil-leader-mode)
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package evil-visualstar
  :config
  (global-evil-visualstar-mode)
  )

(when (memq window-system '(mac ns))
  (use-package exec-path-from-shell
    :config
    (exec-path-from-shell-initialize)
    )
  )

(use-package expand-region)

(use-package flycheck)

(use-package general)

(use-package git-messenger)

(use-package git-timemachine)

(use-package helm)

;; { -- START --
;; use helm-dash and language-detection
;; ... https://emacs-china.org/t/topic/5851/2

;; uses Dash docsets inside emacs to browse documentation
(use-package helm-dash
  :config
  (setq helm-dash-browser-func 'eww)
  (setq dash-docs-enable-debugging nil)
  (setq helm-dash-docsets-path (expand-file-name ".docsets" user-emacs-directory))
  (my-check-for-executable "sqlite3" "sqlite3") ; sqlite3 is required for `helm-dash'
  )

;; automatically detects the programming language in a buffer or string
(use-package language-detection
  :config
  (require 'cl-lib)

  (defun eww-tag-pre (dom)
    (let ((shr-folding-mode 'none)
	  (shr-current-font 'default))
      (shr-ensure-newline)
      (insert (eww-fontify-pre dom))
      (shr-ensure-newline)))

  (defun eww-fontify-pre (dom)
    (with-temp-buffer
      (shr-generic dom)
      (let ((mode (eww-buffer-auto-detect-mode)))
	(when mode
	  (eww-fontify-buffer mode)))
      (buffer-string)))

  (defun eww-fontify-buffer (mode)
    (delay-mode-hooks (funcall mode))
    (font-lock-default-function mode)
    (font-lock-default-fontify-region (point-min)
				      (point-max)
				      nil))

  (defun eww-buffer-auto-detect-mode ()
    (let* ((map '((ada ada-mode)
		  (awk awk-mode)
		  (c c-mode)
		  (cpp c++-mode)
		  (clojure clojure-mode lisp-mode)
		  (csharp csharp-mode java-mode)
		  (css css-mode)
		  (dart dart-mode)
		  (delphi delphi-mode)
		  (emacslisp emacs-lisp-mode)
		  (erlang erlang-mode)
		  (fortran fortran-mode)
		  (fsharp fsharp-mode)
		  (go go-mode)
		  (groovy groovy-mode)
		  (haskell haskell-mode)
		  (html html-mode)
		  (java java-mode)
		  (javascript javascript-mode)
		  (json json-mode javascript-mode)
		  (latex latex-mode)
		  (lisp lisp-mode)
		  (lua lua-mode)
		  (matlab matlab-mode octave-mode)
		  (objc objc-mode c-mode)
		  (perl perl-mode)
		  (php php-mode)
		  (prolog prolog-mode)
		  (python python-mode)
		  (r r-mode)
		  (ruby ruby-mode)
		  (rust rust-mode)
		  (scala scala-mode)
		  (shell shell-script-mode)
		  (smalltalk smalltalk-mode)
		  (sql sql-mode)
		  (swift swift-mode)
		  (visualbasic visual-basic-mode)
		  (xml sgml-mode)))
	   (language (language-detection-string
		      (buffer-substring-no-properties (point-min) (point-max))))
	   (modes (cdr (assoc language map)))
	   (mode (cl-loop for mode in modes
			  when (fboundp mode)
			  return mode)))
      (message (format "%s" language))
      (when (fboundp mode)
	mode)))

  (setq shr-external-rendering-functions
	'((pre . eww-tag-pre)))
  )
;; -- END -- }

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  )

(use-package highlight-parentheses
  :delight
  :config
  (add-hook 'prog-mode-hook 'highlight-parentheses-mode)
  (setq highlight-parentheses-colors
    '("#3498DB" "#FF9900" "#38761D" "#9900FF"))
  )

;; automatic and manual symbol highlighting
;; cycle through the locations of any symbol at point
(use-package highlight-symbol
  :delight
  :config
  (add-hook 'prog-mode-hook 'highlight-symbol-mode)
  (add-hook 'prog-mode-hook 'highlight-symbol-nav-mode)
  ;; keybindings for navigation in highlight-symbol-nav-mode:
  ;; M-p highlight-symbol-prev
  ;; M-n highlight-symbol-next
  )

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(
	  ;; align with the org-todo-keyword-faces
	  ("TODO" :foreground "white" :background "#5F87FF")
	  ("DONE" :foreground "white" :background "#2E8B57")
	  ("CANCELED" :foreground "white" :background "#95A5A6")
	  ("WAIT" :foreground "white" :background "#F9BC41")
	  ("IN-PROGRESS" :foreground "white" :background "#3498DB")
	  ("REPORT" :foreground "#C0C0C0" :background "#308014" :box t)
	  ("BUG" :foreground "#E6DB74" :background "black" :box t)
	  ("KNOWNCAUSE" :foreground "#9C91E4" :background "black" :box t)
	  ("IMPROVEMENT" :foreground "#FF9900" :background "black" :box t)
	  ("ENHANCEMENT" :foreground "#9900ff" :background "black" :box t)
	  ("FEATURE" :foreground "#38761d" :background "black" :box t)
	  ("FIXED" :foreground "#4B5556" :strike-through t :box t)
	  ;; my own highlight keywords
	  ("FIXME" :foreground "white" :background "red")
	  ("DEBUG" :foreground "#E6DB74" :background "black" :box t)
	  ("HACK" :foreground "#9C91E4" :background "black" :box t)
	  ("REVIEW" :foreground "#F02660" :background "black" :box t)
	  ("NOTE" :foreground "#C0C0C0" :background "#308014" :box t)
	  ("DEPRECATED" font-lock-doc-face :strike-through t :box t)
	  ("FOLLOWUP" :foreground "white" :background "#808A87" :box t)
	  ("ANSWER" :foreground "white" :background "#808A87" :box t)
	  ("MARK" :foreground "black" :background "#FFFFFF" :box t)
	  ("IMPROVEMENT" :foreground "white" :background "#FF9900" :box t)
	  ("ENHANCEMENT" :foreground "white" :background "#9900FF" :box t)
	  ("FEATURE" :foreground "white" :background "#38761d" :box t)
	  ("Linode" :foreground "white" :background "#999DF7")
	  ("GitHub" :foreground "black" :background "#FFFFFF")
	  ("via" :foreground "#5F87FF" :background "black" :box t)
	  ("Via" :foreground "#5F87FF" :background "black" :box t)
	  ("VIA" :foreground "#5F87FF" :background "black" :box t)
	  ("Jason" :foreground "white" :background "#38761d" :box t)
	  ("ChatGPT" :foreground "white" :background "#19C37D")
	)
      )
  ;; global-hl-todo-mode doesn't seem to work
  ;; https://github.com/tarsius/hl-todo/issues/19
  ;; https://github.com/tarsius/hl-todo/issues/45
  (define-globalized-minor-mode my-global-hl-todo-mode hl-todo-mode
    (lambda ()
      (when (not (derived-mode-p 'magit-mode))
	(hl-todo-mode 1))))
  (my-global-hl-todo-mode 1)
  )

(use-package imenu-list)

(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1)
  (setq keyfreq-file (expand-file-name ".emacs.keyfreq" user-emacs-directory))
  (setq keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" user-emacs-directory))
  )


(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-bullets-bullet-list '("◼️" "○" "¶" "►"))
  )

(use-package org-drill
  :config
  ;; make all agenda files with any archive files associated with them as the
  ;; source of items for drill sessions(scope)
  (setq org-drill-scope 'agenda-with-archives)
  )

(use-package org-modern
  :config
  (global-org-modern-mode)
  (set-face-attribute 'org-modern-label nil
		      :height 0.95) ; adjusted the height, and this style is
				    ; inherited by many other places
  (setq
   org-modern-star nil
   org-modern-hide-stars nil
   org-modern-todo nil
   org-modern-faces nil
   org-modern-label-border nil
   org-modern-block-name '("‣ " . "‣ ")
   ;; for some more parameters configuration, refer to
   ;; https://github.com/minad/org-modern/blob/main/org-modern.el
   )
  )

(use-package org-super-agenda ; <<org-super-agenda>>
  :after org-agenda
  :config
  (setq org-agenda-compact-blocks t
	org-agenda-start-day "+0d")
  (org-super-agenda-mode 1)

  (setq org-agenda-custom-commands
	;; these org-agenda-custom-commands configurations here cannot be
	;; included in the org-agenda-mode-hook together with the Eisenhower
	;; Matrix configuration, as it should be loaded before the hook.

	;; an Emacs configuration reference https://sachachua.com/dotemacs/index.html
	`(
	  ("g" "GTD Method - Critical/Priority/Effort"
	   (
	    (agenda "" ((org-agenda-overriding-header "")
			(org-super-agenda-groups
			 '(
			   (:name "Time Driven - Critical & High Priority (within 2 days)"
				  :and (:priority "A" :deadline today :not (:habit t) :not (:effort> "0") :not (:todo ("WAIT" "CANCEL")))
				  :and (:priority "A" :scheduled today :not (:habit t) :not (:effort> "0") :not (:todo ("WAIT" "CANCEL")))
				  :and (:priority "A" :deadline (before
								 ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 2 86400)))
								 ) :not (:habit t) :not (:effort> "0") :not (:todo ("WAIT" "CANCEL")))
				  :and (:priority "A" :scheduled (before
								  ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 1 86400)))
								  ) :not (:habit t) :not (:effort> "0") :not (:todo ("WAIT" "CANCEL")))
				  :order 0)
			   (:name "Energy Driven - Critical & Low Effort (<= 15 mins)"
				  :and (:priority "A" :effort< "15" :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
				  :order 5)
			   (:name "Critical & High Effort (> 15 mins)"
				  :and (:priority "A" :effort> "16" :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
				  :order 10)
			   (:discard (:habit t))
			   ;; After the last group, the agenda will display items that didn't
			   ;; match any of these groups, with the default order position of 99
			   ))
			))))
	  ("h" "Habit Tracker"
	   (
	    (agenda "" ((org-agenda-overriding-header "")
			(org-super-agenda-groups
			 '(
			   (:name "Habit(s) to be done today :)"
				  :and (:scheduled today :habit t :not (:todo ("WAIT" "CANCEL")))
				  :order 0)
			   (:name "Habit(s) that missed in the past :("
				  :and (:scheduled past :habit t :not (:todo ("WAIT" "CANCEL")))
				  :order 5)
			   (:discard (:not (:habit t)))
			   ;; After the last group, the agenda will display items that didn't
			   ;; match any of these groups, with the default order position of 99
			   ))
			))))

	  ))
  )

(use-package orglink
  :delight
  :config
  (global-orglink-mode))

(use-package org-drill
  :config
  (setq org-drill-leech-method "warn")
  )

(use-package ox-hugo
  :after ox
  )

;; play animated GIF or PNG inline in Org buffers
;; M-x org-inline-anim-animate (or C-c C-x m) when the point is on the image
;; with a single prefix (C-u), the animation will play and loop
;; you can stop it with a double prefix (C-u C-u)
(use-package org-inline-anim
  :config
  (add-hook 'org-mode-hook #'org-inline-anim-mode)
  )

;; { START: Org-roam
(unless (executable-find "rg")
  (when (string= (which-linux-release-info "distributor") "Ubuntu")
    (if (string< (which-linux-release-info "release") "18.10")
	(shell-command "sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb && sudo dpkg -i ripgrep_13.0.0_amd64.deb && sudo rm -rf ripgrep_13.0.0_amd64.deb")
      (shell-command "sudo apt-get install ripgrep")
      )
    )
  )

(use-package org-roam
  :if window-system ; for graphical Emacs
  :after emacsql-sqlite3
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-database-connector 'sqlite3)
  (setq org-roam-mode-sections
  (list #'org-roam-backlinks-section
	    #'org-roam-reflinks-section
	    ;; ripgrep (rg) is used for unlinked references below - (executable-find "rg")
	    #'org-roam-unlinked-references-section
	    ))
  )

(my-check-for-executable "ripgrep (rg)" "rg")
;; END: Org-roam }

(use-package org-roam-ui
  :delight
  :if window-system ; for graphical Emacs
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
	org-roam-ui-follow t
	org-roam-ui-update-on-save t
	org-roam-ui-open-on-start nil)
  ;; [BUG] Org-roam-ui doesn't show the node's text if in a .dir-locals.el
  ;; location, see https://github.com/org-roam/org-roam-ui/issues/236
  )

(use-package pangu-spacing
  :config
  (global-pangu-spacing-mode 1)
  (setq pangu-spacing-real-insert-separtor t)
  )

(use-package pinyinlib
  :config
  ;; TL; DR
  ;; C-s : -> search with pinyin
  ;; C-s / -> search camel case word
  ;; this config is via
  ;; https://app.raindrop.io/my/0/#pinyinlib
  (defun re-builder-extended-pattern (str)
    (let* ((len (length str)))
      (cond
       ;; do nothing
       ((<= (length str) 0))

       ;; If the first charater of input in ivy is ":",
       ;; remaining input is converted into Chinese pinyin regex.
       ((string= (substring str 0 1) ":")
	(setq str (pinyinlib-build-regexp-string (substring str 1 len) t)))

       ;; If the first charater of input in ivy is "/",
       ;; remaining input is converted to pattrn to search camel case word
       ((string= (substring str 0 1) "/")
	(let* ((rlt "")
	       (i 0)
	       (subs (substring str 1 len))
	       c)
	  (when (> len 2)
	    (setq subs (upcase subs))
	    (while (< i (length subs))
	      (setq c (elt subs i))
	      (setq rlt (concat rlt (cond
				     ((and (< c ?a) (> c ?z) (< c ?A) (> c ?Z))
				      (format "%c" c))
				     (t
				      (concat (if (= i 0) (format "[%c%c]" (+ c 32) c)
						(format "%c" c))
					      "[a-z]+")))))
	      (setq i (1+ i))))
	  (setq str rlt))))
      (ivy--regex-plus str)))

  (eval-after-load 'ivy
    '(progn
       ;; better performance on everything (especially windows), ivy-0.10.0 required
       ;; @see https://github.com/abo-abo/swiper/issues/1218
       (setq ivy-dynamic-exhibit-delay-ms 250)

       ;; Press C-p and Enter to select current input as candidate
       ;; https://oremacs.com/2017/11/30/ivy-0.10.0/
       (setq ivy-use-selectable-prompt t)

       (setq ivy-re-builders-alist
	     '((t . re-builder-extended-pattern)))
       ;; set actions when running C-x b
       ;; replace "frame" with window to open in new window
       (ivy-set-actions
	'ivy-switch-buffer-by-pinyin
	'(("j" switch-to-buffer-other-frame "other frame")
	  ("k" kill-buffer "kill")
	  ("r" ivy--rename-buffer-action "rename")))))

  (with-eval-after-load "swiper-isearch"
    (setq ivy-re-builders-alist
	  '((t . re-builder-extended-pattern)
	    (t . ivy-prescient-re-builder))))
  )

(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :config
  (setq projectile-completion-system 'default)
  (setq projectile-enable-caching t)
  ;; the alien indexing method uses external tools (e.g. git, find, etc) to speed up the indexing process.
  (setq projectile-indexing-method 'alien)
  (add-to-list 'projectile-globally-ignored-files "node_modules")
  (add-to-list 'projectile-globally-ignored-files ".cache")
  (add-to-list 'projectile-globally-ignored-files "_cache")
  )

(use-package pyim
  :config
  ;; 用 THUOCL：清华大学开放中文词库数据建立的 pyim 输入法的词库
  (use-package pyim-tsinghua-dict
    :straight (:host github :repo "redguardtoo/pyim-tsinghua-dict" :files ("*.el" "*.pyim"))
    :config
    (pyim-tsinghua-dict-enable)
    )

  (setq default-input-method "pyim")

  (setq pyim-default-scheme 'quanpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; +1. 光标只有在注释里面时，才可以输入中文。+ -> 2021/10/01 commented this `pyim-probe-program-mode` below
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-i 快捷键，强制将光标前的拼音字符串转换为中文。-> [[./init-keybindings.el::pyim-csap]]
  (setq-default pyim-english-input-switch-functions
		'(pyim-probe-dynamic-english
		  pyim-probe-isearch-mode
		  ;; pyim-probe-program-mode
		  pyim-probe-org-structure-template)
		)

  (setq-default pyim-punctuation-half-width-functions
		'(pyim-probe-punctuation-line-beginning
		  pyim-probe-punctuation-after-punctuation)
		)

  ;; 开启代码搜索中文功能（比如拼音，五笔码等）
  (pyim-isearch-mode 1)
  ;; 激活以上这个 mode 后，可以使用下面的方式强制关闭 isearch 搜索框中文输入
  ;; （即使 在 pyim 激活的时候）。
  ;; (setq-default pyim-english-input-switch-functions '(pyim-probe-isearch-mode))

  (setq pyim-page-tooltip 'popup) ; 使用 pupup-el 来绘制选词框

  (setq pyim-page-length 9) ; 选词框显示 9 个候选词

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
	    #'(lambda () (pyim-restart-1 t)))

  ;; pyim-indicator-with-cursor-color 这个 indicator 很容易和其它设置 cursor 颜
  ;; 色的包冲突，因为都调用 set-cursor-color，遇到这种情况后，用户需要自己解决冲
  ;; 突，pyim-indicator 提供了一个简单的机制：
  (setq pyim-indicator-list
	(list #'my-pyim-indicator-with-cursor-color #'pyim-indicator-with-modeline))

  (defun my-pyim-indicator-with-cursor-color (input-method chinese-input-p)
    (if (not (equal input-method "pyim"))
	(progn
	  ;; 用户在这里定义 pyim 未激活时的光标颜色设置语句
	  (set-cursor-color "green"))
      (if chinese-input-p
	  (progn
	    ;; 用户在这里定义 pyim 输入中文时的光标颜色设置语句
	    (set-cursor-color "blue"))
	;; 用户在这里定义 pyim 输入英文时的光标颜色设置语句
	(set-cursor-color "red"))))
  )





(use-package savehist
  ;; from https://emacs-china.org/t/emacs/17606/9
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; allow commands in minibuffers
	      history-length 1000
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 300)
	      )

(use-package super-save
  :delight
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-exclude '(".gpg"))
  )

(use-package swiper)

;; { -- start: if emacs is running in a terminal
(unless (display-graphic-p)

  (add-to-list 'package-archives
	       '("cselpa" . "https://elpa.thecybershadow.net/packages/"))

  (use-package term-keys
    :config
    (term-keys-mode t)
    ;; to configure alacritty for term-keys, use term-keys/alacritty-config to generate a alacritty.yml fragment:
    ;; (require 'term-keys-alacritty)
    ;; (with-temp-buffer
    ;;   (insert (term-keys/alacritty-config))
    ;;   (write-region (point-min) (point-max) "~/alacritty-for-term-keys.yml"))
    ;; then, add the output to your main alacritty.yml file.
    ;; via https://github.com/CyberShadow/term-keys#alacritty
    )

  (setq package-archives (delete '("cselpa" . "https://elpa.thecybershadow.net/packages/") package-archives))

  (defun term-keys-reminder-messages ()
    (yes-or-no-p "term-keys is used to handle keyboard input involving any combination of keys and modifiers in emacs through supported terminal emulator(Alacritty is recommended on Windows), refer to term-keys README for configuration. Continue?")
    )

  (unless noninteractive
    (if (boundp 'term-keys-reminder)
	(when (symbol-value 'term-keys-reminder) (term-keys-reminder-messages))
      (term-keys-reminder-messages)
      )
    )

  )
;; -- end: if emacs is running in a terminal }

(use-package toc-org)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package vertico
  :config
  (vertico-mode))

(use-package vlf
  :config
  (require 'vlf-setup)
  ;; without this package,
  ;; Alt+x global-font-lock-mode and Alt+x global-linum-mode
  ;; then, open the large file.
  ;; another way to solve the opening large file problem is by using Alt+x find-file-literally. It'll open the file without syntax coloring, and without interpreting Unicode.
  ;; via http://xahlee.info/emacs/misc/emacs_open_large_file_slow.html
  )

(use-package vline
  ;; make vline package load from local site-lisp folder
  :load-path (lambda () (symbol-value 'load-path))
  :config
  (set-face-background vline-face "#283639")
  )

(use-package which-key
  :config
  ;; allow C-h to trigger which-key before it is done automatically
  (setq which-key-show-early-on-C-h t)
  (which-key-mode 1)
  (which-key-setup-side-window-bottom)
  )

(use-package window-numbering
  :config
  (window-numbering-mode)
  )

(use-package workgroups2
  :config
  (setq wg-session-load-on-start t) ; default: (not (daemonp))

  ;; change workgroups session file
  (setq wg-session-file
	(expand-file-name ".emacs_workgroups" user-emacs-directory))

  (workgroups-mode 1)

  ;; display workgroups in mode line
  (setq wg-mode-line-display-on t) ; default: (not (featurep 'powerline))
  )

(use-package yaml-mode)

(use-package yasnippet
  :config
  (add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode 1)
  :hook (after-init . yas-global-mode)
  )

(use-package yasnippet-snippets
  :after (yasnippet)
  )


(provide 'init-packages)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-packages.el ends here
