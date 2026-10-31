;;; init-plantuml.el --- PlantUML settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; check the configuration for plantuml-mode
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-plantuml.html
;; These checks only inform; `yes-or-no-p' blocked init whenever a PlantUML
;; buffer was restored at startup.  Report instead of waiting for input.
(defun my/plantuml-checking ()
  (unless (file-exists-p org-plantuml-jar-path)
    (message "PlantUML: `org-plantuml-jar-path' not found, set it manually."))
  (unless (or (executable-find "dot") (getenv "GRAPHVIZ_DOT"))
    (message "PlantUML: Graphviz executable not found, install it manually."))
  (unless (image-type-available-p 'svg)
    (message "PlantUML: this Emacs was built without SVG support.")))

(add-hook 'plantuml-mode-hook 'my/plantuml-checking)

;; integration with Org-mode by registering it with the PlantUML language
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))


(provide 'init-plantuml)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-plantuml.el ends here
