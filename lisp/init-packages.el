;;; init-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; { -- START --
;; check Linux distribution
;; https://emacs.stackexchange.com/questions/18205/how-can-i-distinguish-between-linux-distributions-in-emacs
(defun which-linux-distributor ()
  "from lsb_release"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "lsb_release -si")))

(defun which-linux-release ()
  "from lsb_release"
  (interactive)
  (when (eq system-type 'gnu/linux)
    (shell-command-to-string "lsb_release -sr")))
;; -- END -- }

(use-package all-the-icons
  :config
  ;; check if all-the-icons is installed
  ;; reference
  ;; https://github.com/domtronn/all-the-icons.el/issues/120
  (when (and (not (member "all-the-icons" (font-family-list)))
	     (equal system-type 'windows-nt))
    (yes-or-no-p "The 'all-the-icons' fonts are recommended for this configuration with lsp-mode package. Continue and install it later?")
    )
  (when (and (not (member "all-the-icons" (font-family-list)))
		  (not (equal system-type 'windows-nt)))
    (all-the-icons-install-fonts t)
    )
  ;; all-the-icons configuration
  (when (display-graphic-p)
    (require 'all-the-icons))
  (setq inhibit-compacting-font-caches t)
  )

(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package ace-jump-mode)

(use-package annotate
  :config
  (annotate-mode 1)
  (custom-set-faces
   '(annotate-annotation ((t (:background "#ff7f4f" :foreground "white"))))
   '(annotate-annotation-secondary ((t (:background "#ff7f4f" :foreground "white"))))
   '(annotate-highlight ((t (:underline "white"))))
   '(annotate-highlight-secondary ((t (:underline "white"))))
   )
  )

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

(use-package company
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  ;; number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; show suggestions after entering 3 character.
  (setq company-minimum-prefix-length 3)
  ;; when the list of suggestions is shown, and you go through the list of
  ;; suggestions and reach the end of the list, the end of the list of
  ;; suggestions does not wrap around to the top of the list again. This is a
  ;; minor inconvenience that can be solved:
  (setq company-selection-wrap-around t)
  ;; use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)

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

(use-package cnfonts
  :if window-system ; only load this package when in graphical Emacs
  :config
  (cnfonts-mode 1)
  (setq cnfonts-profiles
	'("program" "org-mode" "read-book"))
  )

(use-package counsel)

(use-package doom-themes
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t) ; if nil, bold is universally disabled
  (doom-themes-org-config)
  ;; personal modified version of doom-monokai-classic
  (add-to-list 'custom-theme-load-path (expand-file-name "themes/" user-emacs-directory))
  (load-theme 'doom-monokai-classic t)
  (set-background-color "black")
  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
   `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6))))))
  )

;; M-x elgrep: search a single directory
;; C-u M-x elgrep: search the directory recursively
(use-package elgrep)

(use-package elpa-mirror)

;; (use-package elpy
;;   :config
;;   (elpy-enable)
;;   ;; use flycheck instead of flymake
;;   (when (load "flycheck" t t)
;;     (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;     (add-hook 'elpy-mode-hook 'flycheck-mode))
;;   )

(use-package evil
  :init
  (setq evil-want-integration t) ; this is optional since it's already set to t
				 ; by default.
  (setq evil-want-keybinding nil)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil)
    )
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

(use-package flycheck
  ;; to be tested...
  :ensure-system-package
  ((shellcheck . shellcheck)
   (js-yaml    . "sudo npm install -g js-yaml"))
  )

(use-package general)

(use-package helm)

(use-package highlight-indent-guides
  :config
  (setq highlight-indent-guides-method 'column)
  :hook (prog-mode-hook . highlight-indent-guides-mode)
  )

(use-package hl-todo)

(use-package imenu-list)

(use-package lsp-mode
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  :hook
  (lsp-mode . lsp-enable-which-key-integration) ; which-key integration
  )

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp)))) ; or lsp-deferred

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-position 'top)
  )

(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  )

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  )

(use-package org-drill
  :config
  ;; make all agenda files with any archive files associated with them as the
  ;; source of items for drill sessions(scope)
  (setq org-drill-scope 'agenda-with-archives)
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
  :config
  (global-orglink-mode)
  )

(use-package ox-hugo
  :after ox
  )

;; { START: Org-roam
;; to be tested...
(unless (executable-find "rg")
  (when (eq which-linux-distributor 'Ubuntu)
    (if (string< which-linux-release '18.10)
	(shell-command "sudo curl -LO https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep_13.0.0_amd64.deb && sudo dpkg -i ripgrep_13.0.0_amd64.deb && sudo rm -rf ripgrep_13.0.0_amd64.deb")
      (shell-command "sudo apt-get install ripgrep")
      )
    )
  )

(use-package org-roam
  :ensure-system-package
  (rg . ripgrep)
  :config
  (org-roam-db-autosync-mode)
  (setq org-roam-mode-sections
	(list #'org-roam-backlinks-section
	      #'org-roam-reflinks-section
	      ;; ripgrep (rg) is used for unlinked references below - (executable-find "rg")
	      #'org-roam-unlinked-references-section
	      ))
  )

(unless (executable-find "rg")
  (yes-or-no-p "Please be informed the ripgrep (rg) is used by Org-roam in this configuration file, but the rg executable file is not found.
You need to install it manually. Continue?")
  )
;; END: Org-roam }

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

(use-package pyvenv
  :config
  ;; (pyvenv-mode t)

  ;; set correct Python interpreter
  (setq pyvenv-post-activate-hooks
	(list (lambda ()
		(if (equal system-type 'windows-nt)
		    (setq python-shell-interpreter (concat pyvenv-virtual-env "Scripts/python"))
		  (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python"))
		  )
		)))
  (setq pyvenv-post-deactivate-hooks
	(list (lambda ()
		(setq python-shell-interpreter "python")
		)))
  )

;; START: reformatter config
;; to be tested...
(unless (executable-find "shfmt")
  (when (eq system-type 'gnu/linux)
    (shell-command "sudo curl -sS https://webi.sh/shfmt | sh")
    )
  (when (eq system-type 'darwin)
    (shell-command "curl -sS https://webi.sh/shfmt | sh")
    )
  (when (eq system-type 'windows-nt)
    (shell-command "curl.exe https://webi.ms/shfmt | powershell")
    )
  )
(use-package reformatter
  :ensure-system-package
  (prettier . "sudo npm install -g prettier")
  :config
  (reformatter-define css-yaml-format
    :program "prettier"
    :args (list "--write" buffer-file-name)
    ;; https://emacs.stackexchange.com/questions/24298/can-i-eval-a-value-in-quote
    )
  (reformatter-define sh-format
    :program "shfmt"
    :args (list "-l" "-w" "-i" "4" buffer-file-name)
    ;; 4 spaces as indent, read more https://github.com/mvdan/sh/blob/master/cmd/shfmt/shfmt.1.scd
    ;; https://emacs.stackexchange.com/questions/24298/can-i-eval-a-value-in-quote
    )
  )
(unless (executable-find "prettier")
  (yes-or-no-p "Please be informed the Prettier is used in this configuration file, but the Prettier executable file is not found.
You need to install it manually. Continue?")
  )
(unless (executable-find "shfmt")
  (yes-or-no-p "Please be informed the shfmt is used in this configuration file, but the shfmt executable file is not found.
You need to install it manually. Continue?")
  )
;; END: reformatter config

(use-package super-save
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)
  (setq auto-save-default nil)
  (setq super-save-exclude '(".gpg"))
  )

(use-package swiper)

;; { -- start: if emacs is running in a terminal
;; to be tested...
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

  (if (boundp 'term-keys-reminder)
      (when (symbol-value 'term-keys-reminder) (term-keys-reminder-messages))
    (term-keys-reminder-messages)
    )

  )
;; -- end: if emacs is running in a terminal }

(use-package toc-org)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

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


(provide 'init-packages)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-packages.el ends here
