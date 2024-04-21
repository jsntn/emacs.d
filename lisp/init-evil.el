;;; init-evil.el --- evil related config -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; evil-collection assumes evil-want-keybinding is set to nil and
;; evil-want-integration is set to t before loading evil and evil-collection.
(setq evil-want-integration t)
(setq evil-want-keybinding nil)

(use-package evil
  :init
  (unless (display-graphic-p)
    (setq evil-want-C-i-jump nil)
    )
  :after undo-tree
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