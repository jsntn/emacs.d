;;; init-plantuml.el --- PlantUML settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;; check the configuration for plantuml-mode
;; https://orgmode.org/worg/org-contrib/babel/languages/ob-doc-plantuml.html
(add-hook 'plantuml-mode-hook
	  (lambda ()
	    (unless (boundp 'plantuml-jar-path)
	      (yes-or-no-p "Please be informed the plantuml-jar-path is not found.
You might need to set it manually. Continue?")
	      )
	    (unless (boundp 'org-plantuml-jar-path)
	      (yes-or-no-p "Please be informed the org-plantuml-jar-path is not found.
You might need to set it manually. Continue?")
	      )
	    (unless (or (executable-find "dot") (getenv "GRAPHVIZ_DOT"))
	      (yes-or-no-p "Please be informed the Graphviz executable file is not found.
You need to install it manually. Continue?")
	      )
	    ))

;; https://github.com/skuro/plantuml-mode#execution-modes
(setq plantuml-default-exec-mode 'jar)

;; enable plantuml-mode for PlantUML files
(add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))

;; integration with Org-mode by registering it with the PlantUML language
(add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

(setq plantuml-output-type "svg") ; the default output format


(provide 'init-plantuml)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
;;; init-plantuml.el ends here
