;;; init-load-path.el --- load-path settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; cl - Common Lisp Extension
(require 'cl-lib) ; https://emacs.stackexchange.com/questions/48109/require-cl-or-require-cl-lib

(defun sanityinc/add-subdirs-to-load-path (parent-dir)
  "Add every non-hidden subdir of PARENT-DIR to `load-path'."
  (let ((default-directory parent-dir))
    (setq load-path
          (append
           (cl-remove-if-not
            #'file-directory-p
            (directory-files (expand-file-name parent-dir) t "^[^\\.]"))
           load-path))))

;; add both site-lisp and its immediate subdirs to `load-path'
(push site-lisp-dir load-path)
(sanityinc/add-subdirs-to-load-path site-lisp-dir)


;; TODO: to be updated...
;; for `transient special structure
(let ((transient-lisp-dir (expand-file-name "transient/lisp/" site-lisp-dir)))
  (push transient-lisp-dir load-path))
;; for `notmuch special structure
(let ((notmuch-emacs-dir (expand-file-name "notmuch/emacs/" site-lisp-dir)))
  (push notmuch-emacs-dir load-path))
;; for `org-roam special structure
(let ((org-roam-extentions-dir (expand-file-name "org-roam/extensions/" site-lisp-dir)))
  (push org-roam-extentions-dir load-path))
;; for `workgroups2 special structure
(let ((transient-lisp-dir (expand-file-name "workgroups2/src/" site-lisp-dir)))
  (push transient-lisp-dir load-path))



(provide 'init-load-path)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-load-path.el ends here
