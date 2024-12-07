;;; init-company.el --- company completion related settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



;; {{ START: PComplete - Context-Sensitive Completion in Emacs
;; reference,
;; - https://web.archive.org/web/20231102054827/http://www.masteringemacs.org:80/article/pcomplete-context-sensitive-completion-emacs
;; - https://web.archive.org/web/20231102145724/https://timmydouglas.com/2020/12/18/eshell-complete.html
(defconst pcmpl-git-commands
  '("add"
    "bisect" "branch"
    "checkout" "clone" "commit"
    "diff"
    "fetch"
    "grep"
    "init"
    "log"
    "merge" "mv"
    "pull" "push"
    "rebase" "remote" "reset" "restore" "rm"
    "show"
    "stash"
    "status"
    "submodule"
    "tag")
  "List of `git' commands.")

(defconst pcmpl-git-submodule-commands
  '(
    "foreach"
    "init"
    "status"
    "sync"
    "update"
    ))

(defconst pcmpl-git-diff-args
  '(
    "--cached"
    "--staged"
    ))

(defconst pcmpl-git-log-args
  '(
    "--oneline"
    ))

(defconst pcmpl-git-stash-commands
  '(
    "clear"
    "drop"
    "list"
    "pop"
    "save"
    ))

(defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
  "The `git' command to run to get a list of refs.")

(defun pcmpl-git-get-refs (type)
  "Return a list of `git' refs filtered by TYPE."
  (with-temp-buffer
    (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
    (goto-char (point-min))
    (let ((ref-list))
      (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
        (add-to-list 'ref-list (match-string 1)))
      ref-list)))

(defun pcmpl-git-remotes ()
  "Return a list of remote repositories."
  (split-string (shell-command-to-string "git remote")))

(defun pcomplete/git ()
  "Completion for `git'."
  ;; Completion for the command argument.
  (pcomplete-here* pcmpl-git-commands)
  ;; complete files/dirs forever if the command is `add' or `rm'
  (cond
   ((pcomplete-match "help" 1)
    (pcomplete-here* pcmpl-git-commands))
   ((pcomplete-match (regexp-opt '("pull" "push")) 1)
    (pcomplete-here (pcmpl-git-remotes)))
   ((pcomplete-match (regexp-opt '("add" "rm")) 1)
    (while (pcomplete-here (pcomplete-entries))))
   ;; provide branch completion for the command `checkout'.
   ((pcomplete-match "checkout" 1)
    (pcomplete-here* (append (pcmpl-git-get-refs "heads")
			     (pcmpl-git-get-refs "tags"))))
   ((pcomplete-match "submodule" 1)
    (pcomplete-here* pcmpl-git-submodule-commands))
   ((pcomplete-match "diff" 1)
    (pcomplete-here* pcmpl-git-diff-args))
   ((pcomplete-match "log" 1)
    (pcomplete-here* pcmpl-git-log-args))
   ((pcomplete-match "stash" 1)
    (pcomplete-here* pcmpl-git-stash-commands))
   (t
    (while (pcomplete-here (pcomplete-entries))))
   ))
;; END: PComplete - Context-Sensitive Completion in Emacs }}




;; {{ START: pcomplete company completion
;; via https://web.archive.org/web/20231102031110/https://xenodium.com/eshell-pcomplete-company-completion/
(defun company-pcomplete--overlap-tail (a b)
  "When A is \"SomeDev\" and B is \"Developer\", return \"eloper\"."
  (let ((prefix a)
        (remaining nil))
    (while (and (not remaining) (> (length prefix) 0))
      (when (s-starts-with? prefix b)
        (setq remaining (substring b (length prefix))))
      (setq prefix (substring prefix 1)))
    remaining))

(defun company-pcomplete--candidates (prefix)
  "Get candidates for PREFIX company completion using `pcomplete'."
  ;; When prefix is: "~/Down" and completion is "Downloads", need
  ;; to find common string and join into "~/Downloads/".
  (-map (lambda (item)
          (if (s-starts-with? prefix item)
              item
            (concat prefix (company-pcomplete--overlap-tail prefix item))))
        (all-completions prefix (pcomplete-completions))))

(defun company-pcomplete (command &optional arg &rest ignored)
  "Complete using pcomplete. See `company''s COMMAND ARG and IGNORED for details."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-pcomplete))
    (prefix (company-grab-symbol))
    (candidates
     (company-pcomplete--candidates arg))))
;; END: pcomplete company completion }}



(use-package company-tabnine
  :config
  (setq company-tabnine-binaries-folder (expand-file-name ".TabNine/" user-emacs-directory))
  ;; (add-to-list 'company-backends #'company-tabnine)
  )

(use-package company
  :delight
  :init
  (global-company-mode)
  :config
  (setq company-idle-delay 0.2)
  ;; number the candidates (use M-1, M-2 etc to select completions).
  (setq company-show-numbers t)
  ;; show suggestions after entering 3 character.
  (setq company-minimum-prefix-length 3)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers nil)
  (setq company-tooltip-align-annotations t)
  ;; when the list of suggestions is shown, and you go through the list of
  ;; suggestions and reach the end of the list, the end of the list of
  ;; suggestions does not wrap around to the top of the list again. This is a
  ;; minor inconvenience that can be solved:
  (setq company-selection-wrap-around t)
  ;; use tab key to cycle through suggestions.
  ;; ('tng' means 'tab and go')
  (company-tng-configure-default)

  (setq company-transformers '(delete-dups
			       company-sort-by-occurrence))

  (setq company-backends '(
			   (company-capf company-keywords company-dabbrev-code)
			   ;; commented below to speed up the completion
			   ;; (company-tabnine)
			   company-files)
	)

  ;; add yasnippet support for all company backends.
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
	backend
      (append (if (consp backend) backend (list backend))
	      '(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

  ;; Add `company-elisp' backend for elisp,
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (require 'company-elisp)
	      (unless (memq 'company-elisp company-backends)
		(push 'company-elisp company-backends))))


  ;; set the backends for writing in text related mode
  (defun my-company-backends-text-mode-hook ()
    (setq-local company-backends '(
				   (company-dabbrev company-ispell)
				   ;; commented below to speed up the completion
				   ;; (company-tabnine)
				   company-files)
		))
  (dolist (hook '(
		markdown-mode-hook
		org-mode-hook
		text-mode-hook
		))
  (add-hook hook 'my-company-backends-text-mode-hook))

  ;; set the backends for shell-mode
  (defun my-company-backends-shell-mode-hook ()
    (setq-local company-backends '(
				   (company-capf company-files company-pcomplete)
				   )))
  (add-hook 'shell-mode-hook 'my-company-backends-shell-mode-hook)

  ;; add `company-elisp' backend for elisp.
  ;; (add-hook 'emacs-lisp-mode-hook
  ;; 	    #'(lambda ()
  ;; 		(require 'company-elisp)
  ;; 		(push 'company-elisp company-backends)))
  ;; via https://github.com/manateelazycat/lazycat-emacs/blob/8f3dee8a6fe724ec52cd2b17155cfc2cefc8066b/site-lisp/config/init-company-mode.el




  ;; { START: company-candidates from abo-abo
  ;; if candidate list was ("var0" "var1" "var2"), then entering 1 means:
  ;; select the first candidate (i.e. "var0"), instead of:
  ;; insert "1", resulting in "var1", i.e. the second candidate
  ;; via,
  ;; - https://oremacs.com/2017/12/27/company-numbers/
  (defun ora-company-number ()
    "Forward to `company-complete-number'.
Unless the number is potentially part of the candidate.
In that case, insert the number."
    ;; via https://github.com/abo-abo/oremacs/blob/d217e22a3b8dc88d10f715b32a7d1facf1f7ae18/modes/ora-company.el#L22-L39
    (interactive)
    (let* ((k (this-command-keys))
	   (re (concat "^" company-prefix k)))
      (if (or (cl-find-if (lambda (s) (string-match re s))
			  company-candidates)
	      (> (string-to-number k)
		 (length company-candidates))
	      (looking-back "[0-9]+\\.[0-9]*" (line-beginning-position)))
	  (self-insert-command 1)
	(company-complete-number
	 (if (equal k "0")
	     10
	   (string-to-number k))))))

  (let ((map company-active-map))
    ;; via https://github.com/abo-abo/oremacs/blob/d217e22a3b8dc88d10f715b32a7d1facf1f7ae18/modes/ora-company.el#L46-L53
    (mapc (lambda (x) (define-key map (format "%d" x) 'ora-company-number))
	  (number-sequence 0 9))
    (define-key map " " (lambda ()
			  (interactive)
			  (company-abort)
			  (self-insert-command 1)))
    (define-key map (kbd "<return>") nil))
  ;; END: company-candidates from abo-abo }
  )




;; use this package to fix tooltip alignment issue below,
;; https://github.com/company-mode/company-mode/issues/1388
(use-package company-posframe
  :delight
  :straight (:type git :host github :repo "tumashu/company-posframe")
  :config
  (company-posframe-mode 1)
  )



(provide 'init-company)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-company.el ends here
