;;; init-org.el --- Org-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(when (not (file-directory-p org-directory))
  (if noninteractive
      (message "The org-directory is not defined, will set it to .emacs.d folder to avoid 'No such file org directory' warning.")
    (read-string "The org-directory is not defined, will set it to .emacs.d folder to avoid 'No such file org directory' warning. Press ENTER to continue."))
  ;; [[./init-org.el::od-1]]
  ;; [[./init-org.el::od-2]]
  (setq org-directory (symbol-value 'user-emacs-directory))
  )
(when (not (boundp 'org-mobile-directory))
  (if noninteractive
      (message "The org-mobile-directory is not defined, will set it to .emacs.d folder to avoid void-variable error.")
    (read-string "The org-mobile-directory is not defined, will set it to .emacs.d folder to avoid void-variable error. Press ENTER to continue."))
  ;; [[./init-org.el::omd]]
  (setq org-mobile-directory (symbol-value 'user-emacs-directory))
  )

(setq org-startup-indented t) ; enable org-indent mode

(setq org-log-done 'time) ; keep track of when a certain TODO item was marked as
			  ; done

(setq org-log-done 'note) ; record a note along with the timestamp

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
      '((sequence "TODO(t)" "IN-PROGRESS" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
	(sequence "REPORT(r)" "BUG(b)" "KNOWNCAUSE(k)" "IMPROVEMENT(m)" "ENHANCEMENT(e)" "FEATURE(a)" "|" "FIXED(f)")
	))

(setf org-todo-keyword-faces '(
			       ("CANCELED" . (:foreground "white" :background "#95A5A6"))
			       ("DONE" . (:foreground "white" :background "#2E8B57"))
			       ("WAIT" . (:foreground "white" :background "#F9BC41"))
			       ("IN-PROGRESS" . (:foreground "white" :background "#3498DB"))
			       ("TODO" . (:foreground "white" :background "#5F87FF"))
			       ("REPORT" (:foreground "#C0C0C0" :background "#308014" :box t))
			       ("BUG" (:foreground "#E6DB74" :background "black" :box t))
			       ("KNOWNCAUSE" (:foreground "#9C91E4" :background "black" :box t))
			       ("IMPROVEMENT" (:foreground "#FF9900" :background "black" :box t))
			       ("ENHANCEMENT" (:foreground "#9900ff" :background "black" :box t))
			       ("FEATURE" (:foreground "#38761d" :background "black" :box t))
			       ("FIXED" (:foreground "#4B5556" :strike-through t :box t))
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
  ;; non-nil (i.e., a prefix argument was given when the function was called), the
  ;; first branch of the then clause is taken, which disables the strike-through
  ;; style for both org-done and org-headline-done faces.

  ;; If disable is nil, the second branch of the then clause is taken, which
  ;; enables the strike-through style for both faces. The third form
  ;; set-face-attribute is not actually part of an else clause, but is instead
  ;; an additional statement that will always be executed, regardless of the
  ;; value of disable. It sets the foreground color of the org-headline-done
  ;; face to white.
  (interactive "P")
  ;; `org-headline-done' is used to indicate that a headline is DONE. This face
  ;; is only used if `org-fontify-done-headline' is set.
  ;; `org-done' is the face used for todo keywords that indicate DONE items.
  (setq org-fontify-done-headline t)
  (if disable
      (mapc (lambda (face) (set-face-attribute face nil :strike-through nil))
	    '(org-done org-headline-done))
    (mapc (lambda (face) (set-face-attribute face nil :strike-through t))
	  '(org-done org-headline-done))
    (set-face-attribute 'org-headline-done nil
			:foreground "white")))

(unless noninteractive
  (unless (display-graphic-p)
    (read-string "Please be informed that on Terminal Emacs, the strike-through might not work on the `org-fontify-done-headline' and `org-modern-horizontal-rule' in this configuration. Press ENTER to continue.")
    ;; see,
    ;; https://stackoverflow.com/questions/24185102/emacs-console-mode-org-mode-strike-through-is-not-displayed-as-expected
    ;; https://emacs.stackexchange.com/questions/43722/emacsclient-nw-and-strikethrough-text-in-org-mode
    ))

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
   " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
 org-agenda-current-time-string
 "⭠ now ─────────────────────────────────────────────────"
 )

(add-hook 'org-agenda-finalize-hook 'place-agenda-tags)
(defun place-agenda-tags ()
  "Put the agenda tags by the right border of the agenda window."
  ;; http://lists.gnu.org/archive/html/emacs-orgmode//2010-12/msg00410.html
  (setq org-agenda-tags-column (- 10 (window-width)))
  (org-agenda-align-tags)
  )

;; { -- BEGIN -- org-agenda custom commands
;; https://www.reddit.com/r/orgmode/comments/6ybjjw/aligned_agenda_view_anyway_to_make_this_more/
(setq org-agenda-prefix-format ; the display format
      ;; http://doc.endlessparentheses.com/Var/org-agenda-prefix-format.html
      (quote
       ((agenda . "%5e %12s %12t")
	(timeline . "  % s")
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
(setq org-agenda-files
	      (directory-files-recursively org-directory "\\.org$")) ; <<od-1>>
;; (length org-agenda-files)

;; how to truncate the long task name in the agenda custom view?
;; https://stackoverflow.com/a/16285673/4274775
(defun my-org-agenda-mode-hook ()
  (interactive)
  (setq truncate-lines t))
(add-hook 'org-agenda-mode-hook
          'my-org-agenda-mode-hook)

(add-hook 'org-agenda-mode-hook (lambda ()

				  ;; https://orgmode.org/list/loom.20111014T204701-149@post.gmane.org/
				  (setq org-agenda-files
					(delete-dups (append org-agenda-files
							     ;; <<od-2>>
							     (directory-files-recursively org-directory "\\.org$"))))
				  (setq org-agenda-files
					(delete-dups
					 (append org-agenda-files
						 (directory-files-recursively org-mobile-directory "\\.org$")))) ; <<omd>>

				  ;; { -- START --
				  ;; <<4osa-start>> | the link anchor to the end: [[./init-org.el::4osa-end]]

				  ;; move all these org-super-agenda
				  ;; configuration here to fix the issue that -
				  ;; "Symbol's value as variable is void"
				  ;; org-super-agenda -> [[./init-packages.el::org-super-agenda]]

				  (when (require 'org-super-agenda nil 'noerror) ; https://stackoverflow.com/a/7791820/4274775

				    ;; { -- START --
				    ;; <<calculate-date-start>> | the link anchor to the end: [[./init-org.el::calculate-date-end]]

				    ;; Return day of week: Sun=0, Mon=1, Tues=2, ..., Sat=6
				    ;; refer to 14.7 org-super-agenda from,
				    ;; https://web.archive.org/web/20220928105159/https://cmower.github.io/dotemacs/Emacs.html
				    ;; https://stackoverflow.com/a/67741229/4274775
				    (defun get-day-from-now (n)
				      (-let*
					  (((sec minute hour day month year dow dst utcoff)
					    (decode-time (+ (* n 86400) (float-time)))))
					dow)) ;; dow <=> day-of-week

				    ;; get day of week today
				    (setq day-of-week-today (get-day-from-now 0))

				    ;; get date of week end
				    (-let* (((sec minute hour day month year dow dst utcoff) (decode-time (+ (* (- 8 day-of-week-today) 86400) (float-time)))))
				      (setq org-end-of-week (format "%d-%02d-%02d" year month day)))

				    ;; USEFUL BUT NOT BEING USED CURRENTLY
				    ;; (-let* (((sec minute hour day month year dow dst utcoff)
				    ;; 	     (let ((d (decode-time)))
				    ;; 	       ;; https://www.reddit.com/r/emacs/comments/xdvbgd/easiest_way_for_the_dates_of_tomorrow_weekstart/
				    ;; 	       (decoded-time-add d (make-decoded-time :day (% (- 8 (decoded-time-weekday d)) 8))))
				    ;; 	     ))
				    ;;   (format "%d-%02d-%02d" year month day))

				    ;; USEFUL BUT NOT BEING USED CURRENTLY
				    ;; ;; get date of soon, i.e. 4 days later
				    ;; (-let* (((sec minute hour day month year dow dst utcoff) (decode-time (+ (* 4 86400) (float-time))))) ;; 4 days
				    ;;   (setq org-soon-date (format "%d-%02d-%02d" year month day)))

				    ;; USEFUL BUT NOT BEING USED CURRENTLY
				    ;; ;; get date of the day after tomorrow
				    ;; (-let* (((sec minute hour day month year dow dst utcoff) (decode-time (+ (* 2 86400) (float-time))))) ;; 2 days
				    ;;   (setq org-day-after-tomorrow-date (format "%d-%02d-%02d" year month day)))

				    ;; USEFUL BUT NOT BEING USED CURRENTLY
				    ;; ;; get date of tomorrow
				    ;; (-let* (((sec minute hour day month year dow dst utcoff) (decode-time (+ (* 1 86400) (float-time))))) ;; 1 days
				    ;;   (setq org-tomorrow-date (format "%d-%02d-%02d" year month day)))

				    ;; <<calculate-date-end>> | the link anchor to the start: [[./init-org.el::calculate-date-start]]
				    ;; -- END -- }

				    (setq org-super-agenda-groups
					  `(
					    ;; (:name "Scheduled today"
					    ;; 	 :and (:scheduled today :not (:habit t) :not (:todo ("WAIT" "CANCEL")))
					    ;; 	 :order 0)
					    ;; (format-time-string "%Y-%m-%d" (time-add (current-time) (* 1 86400)))
					    (:name "Important and urgent (within 2 days)"
						   :and (:priority "A" :deadline today :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:priority "A" :scheduled today :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:priority "A" :deadline (before
										  ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 2 86400)))
										  ) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:priority "A" :scheduled (before
										   ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 1 86400)))
										   ) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :order 5)
					    (:name "Important but not urgent in this week"
						   :and (:priority "A" :deadline (before ,org-end-of-week) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:priority "A" :scheduled (before ,org-end-of-week) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :order 10)
					    (:name "Urgent (within 2 days) but not important"
						   :and (:not (:priority "A") :deadline today :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:not (:priority "A") :scheduled today :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:not (:priority "A") :deadline (before
											 ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 1 86400)))
											 ) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   :and (:not (:priority "A") :scheduled (before
											  ,(format-time-string "%Y-%m-%d" (time-add (current-time) (* 1 86400)))
											  ) :not (:todo ("WAIT" "CANCEL")) :not (:habit t))
						   ;; Show this section after "Today" and "Important", because
						   ;; their order is unspecified, defaulting to 0. Sections
						   ;; are displayed lowest-number-first.
						   :order 15)
					    (:discard (:habit t))
					    ;; After the last group, the agenda will display items that didn't
					    ;; match any of these groups, with the default order position of 99
					    ))
				    )
				    ;; <<4osa-end>> | the link anchor to the start: [[./init-org.el::4osa-start]]
				    ;; -- END -- }

				  ))


(provide 'init-org)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org.el ends here
