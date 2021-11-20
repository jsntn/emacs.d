;;; init-keybindings.el --- keybindings with general.el -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; regarding general.el, visit the link below:
;; https://github.com/noctuid/general.el


;; global keybindings
(general-define-key
 "M-x" 'helm-M-x
 "C-s" 'swiper ;; having own history variable allows to get more use of M-p, M-n
	       ;; and C-r.
 "C-=" 'er/expand-region
 "C--" 'er/contract-region
 "C-'" 'imenu-list-smart-toggle
 "C-M-<left>" 'shrink-window-horizontally
 "C-M-<right>" 'enlarge-window-horizontally
 "C-M-<down>" 'shrink-window
 "C-M-<up>" 'enlarge-window
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
   "C-M-=" 'increase-emacs-font-size
   "C-M--" 'decrease-emacs-font-size
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
 :keymaps 'ibuffer-mode-map
 "g" 'evil-goto-first-line
 "G" 'evil-goto-line
 "j" 'evil-next-line
 "k" 'evil-previous-line
 )

(general-define-key
 ;; enabling control-c and control-v to copy and paste in Emacs
 ;; keybindings for evil insert and visual mode
 :states '(insert visual)
 "C-c" 'kill-ring-save
 "C-v" 'yank
 "C-z" 'undo-tree-undo
 "C-y" 'undo-tree-redo
 )

(general-define-key
 ;; keybindings for evil normal and visual mode
 :states '(normal visual)
 "ff" 'evil-scroll-page-down
 "bb" 'evil-scroll-page-up
 "be" 'ibuffer
 "SPC" 'jsntn/toggle-hideshow-block
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
 "g" 'neotree-refresh
 "n" 'neotree-next-line
 "p" 'neotree-previous-line
 "A" 'neotree-stretch-toggle
 "H" 'neotree-hidden-file-toggle
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
 "f" 'ace-jump-char-mode
 ;; ...
 )

(general-define-key
 ;; for org-mode-map
 :prefix "C-c"
 :keymaps 'org-mode-map
 "C-q" 'counsel-org-tag
 "o" 'org-open-maybe ;; redefine file opening without clobbering universal
		     ;; argumnet ...
 ;; ...
 )

(general-define-key
 :prefix "C-c"
 :states 'normal
 "SPC" 'jsntn/toggle-hideshow-all
 ;; ...
 )

;; prefix keybindings
(general-define-key
 :prefix "C-x"
 "C-r" 'helm-recentf
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
 "f" 'ace-jump-char-mode
 "o" 'helm-imenu
 "r" 'revert-buffer
 "w" 'windows-split-toggle
 ;; ...
 )


(provide 'init-keybindings)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-keybindings.el ends here
