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

(use-package annotate)



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







(use-package counsel)



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
	  ("CLOSED" :foreground "white" :background "#2E8B57")
	  ("CANCELED" :foreground "white" :background "#95A5A6")
	  ("WAIT" :foreground "white" :background "#F9BC41")
	  ("IN-PROGRESS" :foreground "white" :background "#3498DB")
	  ("REPORT" :foreground "#C0C0C0" :background "#308014" :box (:line-width (-1 . -1)))
	  ("BUG" :foreground "#E6DB74" :background "black" :box (:line-width (-1 . -1)))
	  ("KNOWNCAUSE" :foreground "#9C91E4" :background "black" :box (:line-width (-1 . -1)))
	  ("IMPROVEMENT" :foreground "#FF9900" :background "black" :box (:line-width (-1 . -1)))
	  ("ENHANCEMENT" :foreground "#9900ff" :background "black" :box (:line-width (-1 . -1)))
	  ("FEATURE" :foreground "#38761d" :background "black" :box (:line-width (-1 . -1)))
	  ("FIXED" :foreground "#4B5556" :strike-through t :box (:line-width (-1 . -1)))
	  ;; my own highlight keywords
	  ("FIXME" :foreground "white" :background "red")
	  ("DEBUG" :foreground "#E6DB74" :background "black" :box (:line-width (-1 . -1)))
	  ("HACK" :foreground "#9C91E4" :background "black" :box (:line-width (-1 . -1)))
	  ("REVIEW" :foreground "#F02660" :background "black" :box (:line-width (-1 . -1)))
	  ("NOTE" :foreground "#C0C0C0" :background "#308014" :box (:line-width (-1 . -1)))
	  ("DEPRECATED" font-lock-doc-face :strike-through t :box (:line-width (-1 . -1)))
	  ("FOLLOWUP" :foreground "white" :background "#808A87" :box (:line-width (-1 . -1)))
	  ("ANSWER" :foreground "white" :background "#808A87" :box (:line-width (-1 . -1)))
	  ("MARK" :foreground "black" :background "#FFFFFF" :box (:line-width (-1 . -1)))
	  ("IMPROVEMENT" :foreground "white" :background "#FF9900" :box (:line-width (-1 . -1)))
	  ("ENHANCEMENT" :foreground "white" :background "#9900FF" :box (:line-width (-1 . -1)))
	  ("FEATURE" :foreground "white" :background "#38761d" :box (:line-width (-1 . -1)))
	  ("Linode" :foreground "white" :background "#999DF7")
	  ("GitHub" :foreground "black" :background "#FFFFFF")
	  ("via" :foreground "#5F87FF" :background "black" :box (:line-width (-1 . -1)))
	  ("Via" :foreground "#5F87FF" :background "black" :box (:line-width (-1 . -1)))
	  ("VIA" :foreground "#5F87FF" :background "black" :box (:line-width (-1 . -1)))
	  ("Jason" :foreground "white" :background "#38761d" :box (:line-width (-1 . -1)))
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

(use-package marginalia
  :init
  (marginalia-mode)
  :config
  (setq marginalia-field-width 9999999) ; maximize the width of marginalia field
  )

(straight-use-package
 '(mr-poker :type git :host github :repo "jsntn/mr-poker.el"))

(use-package neotree
  :config
  (setq neo-smart-open t)
  (setq neo-window-fixed-size nil)
  )

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
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
  (setq org-drill-leech-method "warn")
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
  ;; :if window-system ; for graphical Emacs
  :after emacsql-sqlite3
  :config
  (add-hook 'emacs-startup-hook #'my-activate-org-roam-db-autosync)
  (setq org-roam-database-connector 'sqlite3)
  (setq org-roam-mode-sections
  (list #'org-roam-backlinks-section
	    #'org-roam-reflinks-section
	    ;; ripgrep (rg) is used for unlinked references below - (executable-find "rg")
	    ;; #'org-roam-unlinked-references-section
	    ))
  )

(defun my-activate-org-roam-db-autosync ()
  "Activate `org-roam-db-autosync-mode` after a delay"
  (run-with-idle-timer 7 nil 'org-roam-db-autosync-mode))


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



(use-package smooth-scroll
  :delight
  :straight (:type git :host github :repo "k-talo/smooth-scroll.el")
  :config
  (smooth-scroll-mode t)
  (global-set-key [next] #'smooth-scroll/scroll-up)
  (global-set-key [prior] #'smooth-scroll/scroll-down)
  )

(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1))


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
  (use-package term-keys
    :straight (:type git :host github :repo "CyberShadow/term-keys")
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
  )
;; -- end: if emacs is running in a terminal }

(use-package toc-org)

(use-package undo-tree
  :config
  (global-undo-tree-mode)
  )

(use-package vertico
  :init
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
