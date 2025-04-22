;;; init-load-path.el --- load-path settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; add both site-lisp and its subdirs to `load-path'
(push site-lisp-dir load-path)

(let ((default-directory site-lisp-dir))
  (normal-top-level-add-subdirs-to-load-path))
;; https://web.archive.org/web/20250422152913/https://manateelazycat.github.io/2022/03/02/emacs-load-directory-recursively/



(provide 'init-load-path)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-load-path.el ends here
