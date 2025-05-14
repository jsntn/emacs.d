;;; init-packages.el --- package management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; a utility package to collect various Icon Fonts and propertize them within Emacs.
(require 'all-the-icons)
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

;; a quick cursor jump mode for emacs
;; keybindings:
;; [[./init-keybindings.el::ajm-1]]
;; [[./init-keybindings.el::ajm-2]]
(require 'ace-jump-mode)

;; jump to Chinese character by pinyin with `avy' or `ace-jump-mode'
(require 'ace-pinyin)
(setq ace-pinyin-use-avy nil) ; do not use `avy' but `ace-jump-mode'
(ace-pinyin-global-mode +1)

(require 'annotate)



(require 'benchmark-init)
  ;; To disable collection of benchmark data after init is done.
(add-hook 'after-init-hook 'benchmark-init/deactivate)

;; swap buffers, keybindings -> [[./init-keybindings.el::bm-k]]
(require 'buffer-move)









(require 'counsel)




(require 'eglot)
(setq eglot-stay-out-of '(company)) ; see https://web.archive.org/web/20240731123602/https://github.com/joaotavora/eglot/issues/324

;; M-x elgrep: search a single directory
;; C-u M-x elgrep: search the directory recursively
(require 'elgrep)


(require 'envrc)


(require 'expand-region)

(require 'flycheck)

(require 'general)





;; uses Dash docsets inside emacs to browse documentation
(require 'helm-dash)
(setq helm-dash-browser-func 'eww)
(setq dash-docs-enable-debugging nil)
(setq helm-dash-docsets-path (expand-file-name ".docsets" user-emacs-directory))
(my-check-for-executable "sqlite3" "sqlite3") ; sqlite3 is required for `helm-dash'



(require 'highlight-indent-guides)
(setq highlight-indent-guides-auto-enabled nil)
(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "#555555")
(setq highlight-indent-guides-method 'character)
(add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
;; In some configurations, the following error might show up when emacs starts:
;; Error: highlight-indent-guides cannot auto set faces: `default' face is not set properly
;; This is meant as a warning for when the faces can't be set, but in some
;; situations the error might show up even when the faces are set properly. If
;; this happens regularly, the error can be suppressed by,
;; (setq highlight-indent-guides-suppress-auto-error t)


(require 'hl-todo)
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

(require 'imenu-list)

(require 'ivy)
;; better performance on everything (especially windows), ivy-0.10.0 required
;; @see https://github.com/abo-abo/swiper/issues/1218
(setq ivy-dynamic-exhibit-delay-ms 250)
;; Press C-p and Enter to select current input as candidate
;; https://oremacs.com/2017/11/30/ivy-0.10.0/
(setq ivy-use-selectable-prompt t)

(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
(setq keyfreq-file (expand-file-name ".emacs.keyfreq" user-emacs-directory))
(setq keyfreq-file-lock (expand-file-name ".emacs.keyfreq.lock" user-emacs-directory))

(require 'marginalia)
(marginalia-mode)
(setq marginalia-field-width 9999999) ; maximize the width of marginalia field

(require 'mr-poker)

(require 'neotree)
(setq neo-smart-open t)
(setq neo-window-fixed-size nil)

(require 'orderless)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(setq org-bullets-bullet-list '("◼️" "○" "¶" "►"))

(require 'org-drill)
;; make all agenda files with any archive files associated with them as the
;; source of items for drill sessions(scope)
(setq org-drill-scope 'agenda-with-archives)
(setq org-drill-leech-method "warn")

(require 'org-modern)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
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

(require 'org-super-agenda) ; <<org-super-agenda>>

(setq org-agenda-compact-blocks t
      org-agenda-start-day "+0d")
(add-hook 'org-agenda-mode-hook #'org-super-agenda-mode)

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

(require 'orglink)
(global-orglink-mode)





;; play animated GIF or PNG inline in Org buffers
;; M-x org-inline-anim-animate (or C-c C-x m) when the point is on the image
;; with a single prefix (C-u), the animation will play and loop
;; you can stop it with a double prefix (C-u C-u)
(require 'org-inline-anim)
(add-hook 'org-mode-hook #'org-inline-anim-mode)

;; { START: Org-roam

(require 'org-roam)
(setq org-roam-mode-sections
      (list #'org-roam-backlinks-section
	    #'org-roam-reflinks-section
	    ;; ripgrep (rg) is used for unlinked references below - (executable-find "rg")
	    ;; #'org-roam-unlinked-references-section
	    ))
(with-eval-after-load 'org-roam
  (run-with-idle-timer 30 nil #'org-roam-db-autosync-mode))

(my-check-for-executable "ripgrep (rg)" "rg")
;; END: Org-roam }

(require 'org-roam-ui)
(when (display-graphic-p)
  (with-eval-after-load 'org-roam
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save t
	  org-roam-ui-open-on-start nil)
    ;; [BUG] Org-roam-ui doesn't show the node's text if in a .dir-locals.el
    ;; location, see https://github.com/org-roam/org-roam-ui/issues/236
    ))

(require 'pangu-spacing)
(global-pangu-spacing-mode 1)
(setq pangu-spacing-real-insert-separtor t)

(require 'pinyinlib)
;; TL; DR
;; C-s : -> search with pinyin
;; C-s / -> search camel case word
;; this config is via
;; https://app.raindrop.io/my/0/#pinyinlib
(defun re-builder-extended-pattern (str)
  (require 'ivy)
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

(defun my/swiper (arg)
  "Use Swiper with custom regex builder for Pinyin and Camel-case search when called with prefix argument.
This sets `ivy-re-builders-alist' to use extended pattern matching
for better Pinyin and Camel-case characters search experience when ARG is non-nil.
Otherwise, uses default Swiper behavior based on `ivy-re-builders-alist'."
  (interactive "P")
  (if arg
      (let ((ivy-re-builders-alist
	     '((t . re-builder-extended-pattern))))
	(swiper))
    (swiper)))



(require 'popper)
(setq popper-reference-buffers
      '("\\*Messages\\*"
	help-mode
	eat-mode))
;; Popper popup placement controlled using shackle.el,
(require 'shackle)
(setq shackle-rules
      '((eat-mode :align 'below :select t :size 0.8)
	(help-mode :select t)))
(shackle-mode +1)
(popper-mode +1)
(setq popper-display-control nil) ; the placement is controlled by shackle.el
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "C-x p") 'popper-toggle))



(require 'pyim)

(setq default-input-method "pyim")

;; 用 THUOCL：清华大学开放中文词库数据建立的 pyim 输入法的词库
(require 'pyim-tsinghua-dict)
(pyim-tsinghua-dict-enable)

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
(require 'pyim-cregexp-utils)
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

;; M-x my-pyim/simp2trad
(autoload 'my-pyim/simp2trad (concat user-emacs-directory "misc/pyim-simp2trad.el")
  "Load pyim-simp2trad.el on demand." t)




(require 'sr-speedbar)


(require 'super-save)
(super-save-mode +1)
(setq super-save-auto-save-when-idle t)
(setq auto-save-default nil)
(setq super-save-exclude '(".gpg"))

(require 'swiper)

;; { -- start: if emacs is running in a terminal
(unless (display-graphic-p)
  (require 'term-keys)
  (term-keys-mode t)
  ;; to configure alacritty for term-keys, use term-keys/alacritty-config to generate a alacritty.yml fragment:
  ;; (require 'term-keys-alacritty)
  ;; (with-temp-buffer
  ;;   (insert (term-keys/alacritty-config))
  ;;   (write-region (point-min) (point-max) "~/alacritty-for-term-keys.yml"))
  ;; then, add the output to your main alacritty.yml file.
  ;; via https://github.com/CyberShadow/term-keys#alacritty
  )
;; -- end: if emacs is running in a terminal }



(require 'vertico)
(vertico-mode)
;; Sorting and filtering for `vertico
(require 'vertico-prescient)
(vertico-prescient-mode 1)
;; Save recency and frequency rankings to disk, which let them become better
;; over time.
(prescient-persist-mode 1)



(require 'which-key)
;; allow C-h to trigger which-key before it is done automatically
(setq which-key-show-early-on-C-h t)
(which-key-mode 1)
(which-key-setup-side-window-bottom)


(require 'workgroups2)
(setq wg-session-load-on-start t) ; default: (not (daemonp))
;; change workgroups session file
(setq wg-session-file
      (expand-file-name ".emacs_workgroups" user-emacs-directory))
(workgroups-mode 1)
;; display workgroups in mode line
(setq wg-mode-line-display-on t) ; default: (not (featurep 'powerline))


(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" user-emacs-directory))
(yas-global-mode 1)
(add-hook 'after-init-hook #'yas-global-mode)

(require 'yasnippet-snippets)


(provide 'init-packages)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-packages.el ends here
