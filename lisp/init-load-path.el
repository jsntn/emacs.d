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
(let ((symbol-value 'site-lisp-dir))
  (push site-lisp-dir load-path)
  (sanityinc/add-subdirs-to-load-path site-lisp-dir))


(provide 'init-load-path)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-load-path.el ends here
