;;; init-keybindings.el --- keybindings with general.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; regarding general.el, visit the link below:
;; https://github.com/noctuid/general.el


;; C-x Keybindings:
;; Commands starting with C-x are often used for operations related to files,
;; buffers, windows, and frames.
;; For example, C-x C-f is used to open a file, C-x C-s is used to save a
;; buffer, and C-x 2 is used to split the current window into two vertical
;; windows.

;; C-c Keybindings:
;; Commands starting with C-c are typically user-defined or related to major and
;; minor modes specific to a particular programming language or context.
;; Users and mode developers can define their own keybindings under this prefix
;; to tailor Emacs to their specific needs.
;; For example, in a programming mode like Python mode, C-c C-r can be bound to
;; run a Python script.


;; global keybindings
(general-define-key
 "C-s" 'swiper ; having own history variable allows to get more use of M-p, M-n
	       ; and C-r.
 "C-'" 'helm-imenu
 "C-M-<left>" 'shrink-window-horizontally
 "C-M-<right>" 'enlarge-window-horizontally
 "C-M-<down>" 'shrink-window
 "C-M-<up>" 'enlarge-window
 "M-i" 'pyim-convert-string-at-point ; <<pyim-csap>>
 "C-;" 'pyim-delete-word-from-personal-buffer
 ;; ...
 )

;; `general-def' can be used instead for `define-key'-like syntax
(general-def
 "<f8>" 'neotree-toggle
 ;; ...
 )

;; global keybindings for graphic display Emacs
(when (display-graphic-p)
  (general-define-key
   ;; setup change size font, base on emacs-font-size-pair-list
   "C-M-=" 'cnfonts-increase-fontsize
   "C-M--" 'cnfonts-decrease-fontsize
   )
  )

;; mode keybindings for imenu-list-major-mode-map
(general-define-key
 :keymaps 'imenu-list-major-mode-map
 "g" 'evil-goto-first-line
 "G" 'evil-goto-line
 "j" 'evil-next-line
 "k" 'evil-previous-line
 )

(general-define-key
 :keymaps 'process-menu-mode-map
 "C-k" 'my/delete-process-at-point ; <<my-dpap>>
 )

(general-define-key
 ;; keybindings for evil normal and visual mode
 :states '(normal visual)
 "ff" 'evil-scroll-page-down ; <<page down>>
 "bb" 'evil-scroll-page-up ; <<page-up>>
 "br" 'ibuffer
 "SPC" 'my/toggle-hideshow-block
 "++" 'er/expand-region
 "--" 'er/contract-region
 ;; ...
 )

(general-define-key
 ;; keybindings for evil normal mode with neotree-mode-map
 :states 'normal
 :keymaps 'neotree-mode-map
 "TAB" 'neotree-enter
 "SPC" 'neotree-quick-look
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "n" 'neotree-next-line
 "p" 'neotree-previous-line
 "A" 'neotree-stretch-toggle
 "H" 'neotree-hidden-file-toggle
 "+" 'neotree-create-node
 "d" 'neotree-delete-node
 "r" 'neotree-rename-node
 "c" 'neotree-copy-node
 "^" 'neotree-select-up-node
 ;; page up -> [[./init-keybindings.el::page-up]]
 ;; page down -> [[./init-keybindings.el::page-down]]
 ;; ...
 )

;; for terminal Emacs
(unless (display-graphic-p)
  (general-define-key
   ;; for evil normal mode
   :states 'normal
   "M-i" 'evil-jump-forward
   "M-o" 'evil-jump-backward
   ;; ...
   )
  )

(general-define-key
 :states '(insert)
 ;; override any existing binding here while in insert mode
 :keymaps 'override
 ;; bind C-c C-t to `company-tabnine` in insert mode
 "C-c C-t" 'company-tabnine
 )

;; prefix keybindings
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture
 "f" 'ace-jump-char-mode ;; <<ajm-1>>
 "C-/" 'company-files
 ;; ...
 )

(general-define-key
 ;; for org-mode-map
 :prefix "C-c"
 :keymaps 'org-mode-map
 "C-q" 'counsel-org-tag
 ;; <oom> | definition -> [[./init-org.el::oom]]
 "o" 'org-open-maybe ; redefine file opening without clobbering universal
		     ; argumnet ...
 ;; ...
 )

(general-define-key
 ;; for yaml-mode-map
 :prefix "C-c"
 :keymaps 'yaml-mode-map
 "C-f" 'css-yaml-format-buffer
 )

(general-define-key
 ;; for css-mode-map
 :prefix "C-c"
 :keymaps 'css-mode-map
 "C-f" 'css-yaml-format-buffer
 )

(general-define-key
 ;; for sh-mode-map
 :prefix "C-c"
 :keymaps 'sh-mode-map
 "C-f" 'sh-format-buffer
 )

(general-define-key
 :prefix "C-c"
 :states 'normal
 "SPC" 'my/toggle-hideshow-all
 ;; ...
 )

;; prefix keybindings
(general-define-key
 :prefix "C-x"
 "C-r" 'helm-recentf
 ;; for buffer-move package | <<bm-k>>
 "<up>" 'buf-move-up
 "<down>" 'buf-move-down
 "<left>" 'buf-move-left
 "<right>" 'buf-move-right
 ;; ...
 )

;; evil leader key
(general-create-definer my-leader-def
  ;; :prefix my-leader
  :prefix ",")

;; evil leader keybindings in evil normal mode
(my-leader-def
 :keymaps 'normal
 "b" 'bookmark-bmenu-list
 "d" 'dired
 "f" 'ace-jump-char-mode ;; <<ajm-2>>
 "'" 'imenu-list-smart-toggle
 "r" 'revert-buffer
 "R" 'my/revert-all-file-buffers
 "t" 'my/set-tags-table-list ; <<m-ftf>>
 "w" 'my/windows-split-toggle
 "l" 'my/org-link-goto-at-point
 "L" 'my/org-link-jump-back
 ;; for workgroups2
 "c" 'wg-create-workgroup
 "k" 'wg-kill-workgroup
 "o" 'wg-open-workgroup
 ;; ...
 )


(provide 'init-keybindings)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-keybindings.el ends here
