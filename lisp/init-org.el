;;; init-org.el --- Org-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq org-startup-indented t) ; enable org-indent mode

(setq org-log-done 'time) ; keep track of when a certain TODO item was marked as
			  ; done

(setq org-log-into-drawer t) ; log into LOGBOOK drawer

(setq org-src-fontify-natively t) ; highlight the code in Org-mode

;; { START: temp WA to fix bug #52587
(setq org-fontify-whole-block-delimiter-line t) ; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=52587
(custom-set-faces
 ;; https://emacs.stackexchange.com/questions/52324/prevent-org-source-block-face-from-bleeding-out-in-fold#comment80865_52377
 ;; https://www.reddit.com/r/emacs/comments/cw0499/comment/ifcq16w/?utm_source=share&utm_medium=web2x&context=3
`(org-block-begin-line ((t (:background ,(face-attribute 'default :background)))))
)
;; END: temp WA to fix bug #52587 }

;; inline images display in Org-mode
(setq org-startup-with-inline-images t)

;; https://emacs.stackexchange.com/a/18483/29715
(setq org-startup-truncated nil)

(setq org-tags-column -77) ; https://emacs.stackexchange.com/a/10526/29715
;; reference https://www.reddit.com/r/orgmode/comments/ww1d7x/i_updated_my_orgtagscolumn_variable_now_how_to/
;; read more
;; - https://www.reddit.com/r/orgmode/comments/jss5rp/tags_does_not_align/
;; - https://stackoverflow.com/questions/6210840/tag-position-in-org-mode

(defun org-force-open-current-window ()
  ;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
  (interactive)
  (let ((org-link-frame-setup (quote
			       ((vm . vm-visit-folder)
				(vm-imap . vm-visit-imap-folder)
				(gnus . gnus)
				(file . find-file)
				(wl . wl)))
			      ))
    (org-open-at-point)))
;; Depending on universal argument try opening link
(defun org-open-maybe (&optional arg) ; <<oom>> | keybinding -> [[./init-keybindings.el::oom]]
  (interactive "P")
  (if arg
      (org-open-at-point)
    (org-force-open-current-window)
    )
  )

(setq org-todo-keywords
      ;; '((sequence "☛ TODO(t)" "➼ IN-PROGRESS" "⚑ WAIT(w@/!)" "|" "✔ DONE(d!)" "✘ CANCELED(c@)")
      '((sequence "TODO(t)" "IN-PROGRESS" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)" "CLOSED")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "IMPROVEMENT(m)" "ENHANCEMENT(e)" "FEATURE(a)" "|" "FIXED(f)")
	))

(setf org-todo-keyword-faces '(
			       ("CANCELED" . (:foreground "white" :background "#95A5A6"))
			       ("DONE" . (:foreground "white" :background "#2E8B57"))
			       ("CLOSED" . (:foreground "white" :background "#2E8B57"))
			       ("WAIT" . (:foreground "white" :background "#F9BC41"))
			       ("IN-PROGRESS" . (:foreground "white" :background "#3498DB"))
			       ("TODO" . (:foreground "white" :background "#5F87FF"))
			       ("REPORT" (:foreground "#C0C0C0" :background "#308014" :box (:line-width (-1 . -1))))
			       ("BUG" (:foreground "#E6DB74" :background "black" :box (:line-width (-1 . -1))))
			       ("KNOWNCAUSE" (:foreground "#9C91E4" :background "black" :box (:line-width (-1 . -1))))
			       ("IMPROVEMENT" (:foreground "#FF9900" :background "black" :box (:line-width (-1 . -1))))
			       ("ENHANCEMENT" (:foreground "#9900ff" :background "black" :box (:line-width (-1 . -1))))
			       ("FEATURE" (:foreground "#38761d" :background "black" :box (:line-width (-1 . -1))))
			       ("FIXED" (:foreground "#4B5556" :strike-through t :box (:line-width (-1 . -1))))
			       ))

(defun my/modify-org-done-face (&optional disable)
  "enable or disable(C-u) the strike style for `org-done' item."
  ;; https://emacs.stackexchange.com/questions/10595/how-to-strike-out-done-items-in-org-mode

  ;; This function is improved by ChatGPT :)
  ;; Explanation from ChatGPT,

  ;; In the my/modify-org-done-face function, there is indeed what appears to be
  ;; two else clauses.

  ;; However, this is not actually two separate else clauses. It is a single if
  ;; statement with two branches in the then clause.

  ;; In this if statement, disable is the condition being tested. If disable is
  ;; non-nil (i.e., a prefix argument was given when the function was called),
  ;; it disables the strike-through style for both org-done and
  ;; org-headline-done faces.

  ;; If disable is nil, it enables the strike-through style for both faces.

  ;; The foreground white color form set-face-attribute will always be executed,
  ;; regardless of the value of disable. It sets the foreground color of the
  ;; org-headline-done face to white.
  (interactive "P")
  ;; `org-headline-done' is used to indicate that a headline is DONE. This face
  ;; is only used if `org-fontify-done-headline' is set.
  ;; `org-done' is the face used for todo keywords that indicate DONE items.
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-headline-done nil
		      :foreground "white")
  (if disable
      (mapc (lambda (face) (set-face-attribute face nil :strike-through nil))
	    '(org-done org-headline-done))
    (mapc (lambda (face) (set-face-attribute face nil :strike-through t))
	  '(org-done org-headline-done))))

(defcustom my-terminal-supports-strike-through 'unknown
  "Whether the terminal supports strike-through rendering."
  :type '(choice (const :tag "Yes" t)
		 (const :tag "No" nil)
		 (const :tag "Unknown (ask)" unknown))
  :group 'convenience)

(defun my--check-strike-through-support ()
  "Prompt about strike-through support if not yet answered.
Only prompts in a real interactive terminal frame.  Under a daemon the
startup frame has no tty, so defer the question to the first terminal
client frame via `server-after-make-frame-hook' instead of blocking
daemon startup."
  (unless (display-graphic-p)
    (when (eq my-terminal-supports-strike-through 'unknown)
      (let ((choice (read-char-choice
		     "Does this terminal support strike-through text? (y)es, (n)o, (u)nknown: "
		     '(?y ?n ?u))))
	(pcase choice
	  (?y (customize-save-variable 'my-terminal-supports-strike-through t))
	  (?n (customize-save-variable 'my-terminal-supports-strike-through nil)
	      (message "Note: strike-through text will not render properly in this terminal."))
	  (?u nil))))))

(defun my--maybe-check-strike-through-support ()
  "Run `my--check-strike-through-support' once a usable frame exists.
Removes itself from `server-after-make-frame-hook' after it has an
answer so later client frames are not re-prompted."
  (when (frame-live-p (selected-frame))
    (my--check-strike-through-support)
    (unless (eq my-terminal-supports-strike-through 'unknown)
      (remove-hook 'server-after-make-frame-hook
		   #'my--maybe-check-strike-through-support))))

(unless noninteractive
  (if (daemonp)
      ;; Daemon startup has no tty: ask when a terminal client attaches.
      (add-hook 'server-after-make-frame-hook
		#'my--maybe-check-strike-through-support)
    (add-hook 'after-init-hook #'my--check-strike-through-support)))

;; ===============================================================
;; Org agenda settings ｜ <<org-agenda-settings>>
;; ===============================================================
;; Reference
;; https://sainathadapa.github.io/emacs-spacemacs-config/org-config
;; https://sachachua.com/dotemacs/
(setq org-agenda-span 1) ; max number of days to show in agenda by default(C-a a a)

;; don't show tasks in agenda if they are done
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-timestamp-if-done t)

(setq org-agenda-compact-blocks t
      org-agenda-start-day "+0d")
(setq org-agenda-start-on-weekday 6) ; starting my weeks on Saturday
;; (setq org-agenda-start-on-weekday nil) ; agenda starts on the current day

(setq org-agenda-sorting-strategy
      ;; sorting strategy
      (quote
       ((agenda priority-down alpha-up)
	(todo priority-down alpha-up)
	(tags priority-down alpha-up)))
      )

(setq
 ;; org-agenda styling
 org-agenda-block-separator ?─
 org-agenda-time-grid
 '((daily today require-timed)
   (800 1000 1200 1400 1600 1800 2000)
   " ..... "
   "---------------")
 org-agenda-current-time-string
 "<- now -------------------------------------------------"
 )

(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  ;; http://lists.gnu.org/archive/html/emacs-orgmode//2010-12/msg00410.html
  (redisplay)
  ;; Remove double colons at the end of inherited tags
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "::$" nil t)
      (replace-match ":" nil nil)))
  (setq org-agenda-tags-column (- 15 (window-width)))
  (org-agenda-align-tags)
  )
(add-hook 'org-agenda-finalize-hook 'place-agenda-tags)

;; { -- BEGIN -- org-agenda custom commands
;; https://www.reddit.com/r/orgmode/comments/6ybjjw/aligned_agenda_view_anyway_to_make_this_more/
(setq org-agenda-prefix-format ; the display format
      ;; http://doc.endlessparentheses.com/Var/org-agenda-prefix-format.html
      (quote
       ((agenda . "%5e %27s %12t")
	;; (timeline . "  % s")
	(todo . " %12t")
	(tags . " %12t")
	(search . " %12t"))
       ))
(setq org-agenda-deadline-leaders
      ;; http://doc.endlessparentheses.com/Var/org-agenda-deadline-leaders.html
      (quote
       (" Deadline " "In %3d d. " "%2d d. ago ")
       ))
(setq org-agenda-scheduled-leaders
      ;; http://doc.endlessparentheses.com/Var/org-agenda-scheduled-leaders.html
      (quote
       (" Scheduled " "Sched.%2dx ")
       ))
;; -- END -- org-agenda custom commands }

;; teach Org where to look for all of the files you wish to include in your agenda
;; https://stackoverflow.com/a/41969519/4274775
(when (and (file-directory-p org-directory) (not (string= org-directory "")))
  (setq org-agenda-files
	(seq-filter (lambda (f) (not (string-match-p "/\\.Trash-" f)))
		    (directory-files-recursively org-directory "\\.org$"))))
;; (length org-agenda-files)

;; fix issue like below,
;; Non-existent agenda file /path/to/.#xxx.org.  [R]emove from list or [A]bort?
(setq org-agenda-skip-unavailable-files t)

;; how to truncate the long task name in the agenda custom view?
;; https://stackoverflow.com/a/16285673/4274775
(defun my-org-agenda-mode-hook ()
  (setq truncate-lines t)

  ;; prevent issue below,
  ;; org-agenda-get-day-entries: Agenda file xxx.org is not in Org mode
  (my/switch-opened-org-files-to-org-mode)

  ;; `org-agenda-files' is built once when this file loads (see the
  ;; `directory-files-recursively' above).  Re-globbing the whole Org tree on
  ;; every agenda open cost 1.03 s here (314 files); refresh on demand with
  ;; M-x my/rescan-org-agenda-files instead.
  )

(add-hook 'org-agenda-mode-hook
          'my-org-agenda-mode-hook)

;; Keep the GLOBAL `org-super-agenda-groups' empty so the plain time-line
;; agenda (`a' / `C-a < a') shows no grouping -- it is a pure day view, per
;; org-agenda's design. Grouping is opt-in per command via a `let' binding
;; in the command's settings (see the "u" urgency classifier in init.el and
;; the "g"/"h" commands in init-packages.el). This line also clears any
;; value a previous session's hook may have left behind.
(setq org-super-agenda-groups nil)

;; { -- START --
;; <<4osa-start>> | the link anchor to the end: [[./init-org.el::4osa-end]]
;;
;; org-super-agenda urgency/importance grouping.
;;
;; This is NOT hung on `org-agenda-mode-hook' (which would force the
;; grouping onto every agenda, including the plain time-line view `a').
;; Following org-agenda's own design, the grouping belongs to a dedicated
;; TODO-selection command: `org-agenda-list' (span-bound time-line) and a
;; `alltodo' urgency classifier are different view types.  The custom
;; command in init.el let-binds `org-super-agenda-groups' to the value
;; returned here, so `alltodo' pulls ALL todos (span-independent) and the
;; `before today+Nd' clauses actually match tomorrow/day-after items.
;; org-super-agenda -> [[./init-packages.el::org-super-agenda]]
(defun my/urgency-super-agenda-groups ()
  "Return `org-super-agenda-groups' for the urgency/importance classifier.
Computed at call time so the date boundaries track the current day."
  (require 'org-super-agenda)
  ;; { -- START --
  ;; <<calculate-date-start>> | the link anchor to the end: [[./init-org.el::calculate-date-end]]
  ;; Return day of week: Sun=0, Mon=1, Tues=2, ..., Sat=6
  ;; refer to 14.7 org-super-agenda from,
  ;; https://web.archive.org/web/20220928105159/https://cmower.github.io/dotemacs/Emacs.html
  ;; https://stackoverflow.com/a/67741229/4274775
  (cl-flet ((get-day-from-now (n)
	      (-let* (((_sec _minute _hour _day _month _year dow _dst _utcoff)
		       (decode-time (+ (* n 86400) (float-time)))))
		dow)))             ;; dow <=> day-of-week
    (let* ((day-of-week-today (get-day-from-now 0))
	   (org-end-of-week
	    (-let* (((_sec _minute _hour day month year _dow _dst _utcoff)
		     (decode-time (+ (* (- 8 day-of-week-today) 86400) (float-time)))))
	      (format "%d-%02d-%02d" year month day))))
      ;; <<calculate-date-end>> | the link anchor to the start: [[./init-org.el::calculate-date-start]]
      `(
	(:name "🔥 [DO NOW] Important and urgent (within 2 days)"
	       :and (:priority "A" :deadline today :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:priority "A" :scheduled today :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:priority "A" :deadline (before
					      ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 3 86400)))
					      ) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:priority "A" :scheduled (before
					       ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 3 86400)))
					       ) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :order 5)
	(:name "📅 [SCHEDULE] Important but not urgent in this week"
	       :and (:priority "A" :deadline (before ,org-end-of-week) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:priority "A" :scheduled (before ,org-end-of-week) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :order 10)
	(:name "📅 [SCHEDULE] Important but not urgent"
	       :and (:priority "A" :deadline future :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:priority "A" :scheduled future :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :order 11)
	(:name "📞 [DELEGATE] Urgent (within 2 days) but not important"
	       :and (:not (:priority "A") :deadline today :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:not (:priority "A") :scheduled today :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:not (:priority "A") :deadline (before
						     ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 3 86400)))
						     ) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       :and (:not (:priority "A") :scheduled (before
						      ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 3 86400)))
						      ) :not (:todo ("CLOSED" "CANCEL")) :not (:habit t))
	       ;; Show this section after "Today" and "Important", because
	       ;; their order is unspecified, defaulting to 0. Sections
	       ;; are displayed lowest-number-first.
	       :order 15)
	(:discard (:habit t))
	;; After the last group, the agenda will display items that didn't
	;; match any of these groups, with the default order position of 99
	))))
;; <<4osa-end>> | the link anchor to the start: [[./init-org.el::4osa-start]]
;; -- END -- }




(require 'org-task-scheduler)

(require 'init-org-deps-graph) ; task dependency graph (graphviz) + keyboard jump
(require 'init-org-edna-edit)  ; interactive edna dependency editor (M-x my/org-deps-edit)
(require 'init-org-deps-view)  ; read-only dependency view (M-x my/org-deps-view)



(provide 'init-org)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org.el ends here
