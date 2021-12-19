;;; init-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package ace-jump-mode)

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

(use-package elpa-mirror)

(use-package elpy
  :config
  (elpy-enable)
  ;; use flycheck instead of flymake
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))
  )

(use-package evil
  :init
  (setq evil-want-integration t) ; this is optional since it's already set to t
				 ; by default.
  (setq evil-want-keybinding nil)
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil)
    )
  :config
  (evil-mode 1)
  ;; change the cursor color in terms of evil mode
  (setq evil-emacs-state-cursor '("red" box))
  (setq evil-normal-state-cursor '("green" box))
  (setq evil-visual-state-cursor '("orange" box))
  (setq evil-insert-state-cursor '("red" bar))
  (setq evil-replace-state-cursor '("red" bar))
  (setq evil-operator-state-cursor '("red" hollow))
  (setq evil-undo-system 'undo-tree)
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

(use-package plantuml-mode)

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

(use-package toc-org)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package vline
  ;; vline package will be loaded from local site-lisp folder
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

(use-package yaml-mode)


(provide 'init-packages)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-packages.el ends here
