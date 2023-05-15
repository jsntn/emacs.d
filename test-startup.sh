#!/bin/sh -e
echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --eval '(progn
                        (defvar url-show-status)
                        (let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (message "%s" (org-version))
                           (load-file user-init-file)
                           (message "%s" (org-version))
                           (run-hooks (quote after-init-hook))))'
echo "Startup successful"
