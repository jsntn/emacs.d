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

(require 'evil)
(evil-mode 1)

(unless (display-graphic-p)
  (setq evil-want-C-i-jump nil)
  )

(evil-set-undo-system 'undo-tree) ; https://github.com/emacs-evil/evil/issues/1372#issuecomment-712611291

;; change the cursor color in terms of evil mode
(setq evil-emacs-state-cursor '("red" box))
(setq evil-normal-state-cursor '("green" box))
(setq evil-visual-state-cursor '("orange" box))
(setq evil-insert-state-cursor '("red" bar))
(setq evil-replace-state-cursor '("red" bar))
(setq evil-operator-state-cursor '("red" hollow))

(require 'evil-collection)
(evil-collection-init)

(use-package evil-leader
  :config
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



(provide 'init-evil)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-evil.el ends here
