#!/bin/sh -e
echo "Attempting startup..."
${EMACS:=emacs} -nw --batch \
                --debug-init \
                --eval '(progn
                        (defvar url-show-status)
                        (setq eval-expression-print-level 10)
                        (setq eval-expression-print-length 100)
                        (let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "init.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file)
                           (my/list-packages-and-versions)
                           (straight-freeze-versions)
                           (run-hooks (quote after-init-hook))))'

ls -lrt ./straight/versions

directory="./straight/versions"
for file in "$directory"/*
do
  if [ -f "$file" ]; then
    cat "$file"
    echo "-------------------"
  fi
done

echo "Startup successful"
