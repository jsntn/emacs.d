;;; init-evil.el --- evil related config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:





(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t)
(setq undo-tree-visualizer-timestamps t)
(let ((undo-dir (expand-file-name "undo" user-emacs-directory)))
  (unless (file-exists-p undo-dir)
    (make-directory undo-dir t))
  (setq undo-tree-history-directory-alist `(("." . ,undo-dir))))

;; evil-collection assumes evil-want-keybinding is set to nil and
;; evil-want-integration is set to t before loading evil and evil-collection.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

;; In case you run Emacs in a terminal, you might find that TAB does not work as
;; expected (being bound to evil-jump-forward instead of org-tab). You can add
;; this to your configuration to get org-tab instead at the cost of losing C-i
;; to jump.
(unless (display-graphic-p)
  (setq evil-want-C-i-jump nil)
  )

(require 'evil)
(evil-mode 1)

(evil-set-undo-system 'undo-tree) ; https://github.com/emacs-evil/evil/issues/1372#issuecomment-712611291

(if (display-graphic-p)
    (progn
      ;; change the cursor color in terms of evil mode
      (setq evil-emacs-state-cursor '("red" box))
      (setq evil-normal-state-cursor '("green" box))
      (setq evil-visual-state-cursor '("orange" box))
      (setq evil-insert-state-cursor '("red" bar))
      (setq evil-replace-state-cursor '("red" bar))
      (setq evil-operator-state-cursor '("red" hollow)))
  (progn
    (add-hook 'evil-insert-state-entry-hook (lambda ()
					      (unless buffer-read-only
						(send-string-to-terminal "\033[5 q"))))
    (add-hook 'evil-insert-state-exit-hook (lambda ()
					     (unless buffer-read-only
					       (send-string-to-terminal "\033[2 q"))))
    ;; via https://www.reddit.com/r/emacs/comments/4rd44a/how_to_change_cursor_in_evil_mode/
    (send-string-to-terminal "\033[2 q") ; the default cursor state
    )
  )


(require 'evil-collection)
(evil-collection-init)

(require 'evil-surround)
(global-evil-surround-mode 1)




;; START: MAKE SELECTIONS (BY JUMPING THROUGH SEARCHING) INTUITIVELY
(defun my-evil-visual-select-to-search-line ()
  "Select the current line, search forward with `swiper`, and extend selection to include the target line."
  (interactive)
  (call-interactively #'swiper)
  (evil-visual-line))
(with-eval-after-load 'evil
  (with-eval-after-load 'swiper
    (define-key evil-visual-state-map (kbd "C-s") 'my-evil-visual-select-to-search-line)))
;; END: MAKE SELECTIONS (BY JUMPING THROUGH SEARCHING) INTUITIVELY






(provide 'init-evil)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-evil.el ends here
