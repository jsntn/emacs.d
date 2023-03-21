;;; init-keybindings.el --- keybindings with general.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; regarding general.el, visit the link below:
;; https://github.com/noctuid/general.el


;; global keybindings
(general-define-key
 "M-x" 'helm-M-x
 "C-s" 'swiper ; having own history variable allows to get more use of M-p, M-n
	       ; and C-r.
 "C-=" 'er/expand-region
 "C--" 'er/contract-region
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
 :keymaps 'ibuffer-mode-map
 "g" 'evil-goto-first-line
 "G" 'evil-goto-line
 "j" 'evil-next-line
 "k" 'evil-previous-line
 )

(general-define-key
 ;; keybindings for evil normal and visual mode
 :states '(normal visual)
 "ff" 'evil-scroll-page-down ; <<page down>>
 "bb" 'evil-scroll-page-up ; <<page-up>>
 "be" 'ibuffer
 "SPC" 'my/toggle-hideshow-block
 "C-]" 'counsel-etags-find-tag-at-point ; <<ftap>>
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

;; prefix keybindings
(general-define-key
 :prefix "C-c"
 ;; bind "C-c a" to 'org-agenda
 "a" 'org-agenda
 "b" 'counsel-bookmark
 "c" 'org-capture
 "f" 'ace-jump-char-mode ;; <<ajm-1>>
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
 "t" 'my/set-tags-table-list ; <<m-ftf>>
 "w" 'my/windows-split-toggle
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
