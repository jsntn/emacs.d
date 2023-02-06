;;; init-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


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
  ;; No delay in showing suggestions.
  (setq company-idle-delay 0)
  ;; Show suggestions after entering one character.
  (setq company-minimum-prefix-length 2)
  ;; When the list of suggestions is shown, and you go through the list of
  ;; suggestions and reach the end of the list, the end of the list of
  ;; suggestions does not wrap around to the top of the list again. This is a
  ;; minor inconvenience that can be solved:
  (setq company-selection-wrap-around t)
  ;; Use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)
  )

(use-package cnfonts
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

(use-package flycheck)

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

(use-package ox-hugo
  :after ox
  )

(unless (executable-find "prettier")
  (yes-or-no-p "Please be informed the Prettier is used in this configuration file, but the Prettier executable file is not found.
You need to install it manually. Continue?")
  )

(unless (executable-find "shfmt")
  (yes-or-no-p "Please be informed the shfmt is used in this configuration file, but the shfmt executable file is not found.
You need to install it manually. Continue?")
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

(use-package reformatter
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
  (which-key-setup-side-window-right)
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
