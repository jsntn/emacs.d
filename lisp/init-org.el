;;; init-org.el --- Org-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq org-startup-indented t) ; enable org-indent mode

(setq org-log-done 'time) ; keep track of when a certain TODO item was marked as
			  ; done

(setq org-log-done 'note) ; record a note along with the timestamp

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
			       ("TODO" . (:foreground "white" :background "#CD5C5C"))
			       ("REPORT" (:foreground "#C0C0C0" :background "#308014" :box t))
			       ("BUG" (:foreground "#E6DB74" :background "black" :box t))
			       ("KNOWNCAUSE" (:foreground "#9C91E4" :background "black" :box t))
			       ("IMPROVEMENT" (:foreground "#FF9900" :background "black" :box t))
			       ("ENHANCEMENT" (:foreground "#9900ff" :background "black" :box t))
			       ("FEATURE" (:foreground "#38761d" :background "black" :box t))
			       ("FIXED" (:foreground "#4B5556" :strike-through t :box t))
			       ))

(defun my/modify-org-done-face ()
  (setq org-fontify-done-headline t)
  (set-face-attribute 'org-done nil :strike-through t)
  (set-face-attribute 'org-headline-done nil
		      :strike-through t
		      :foreground "white")
  )
;; https://emacs.stackexchange.com/questions/10595/how-to-strike-out-done-items-in-org-mode
(eval-after-load "org"
  (add-hook 'org-add-hook 'my/modify-org-done-face))


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
	      (directory-files-recursively org-directory "\\.org$"))
;; (length org-agenda-files)

;; how to truncate the long task name in the agenda custom view?
;; https://stackoverflow.com/a/16285673/4274775
(defun my/org-agenda-mode-hook ()
  (interactive)
  (setq truncate-lines t))
(add-hook 'org-agenda-mode-hook
          'my/org-agenda-mode-hook)


(provide 'init-org)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org.el ends here
