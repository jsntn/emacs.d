;;; init-org-edna-edit.el --- Interactive org-edna dependency editor -*- lexical-binding: t -*-
;;; Commentary:
;; Author org-edna task dependencies directly from a heading, without hand
;; copying UUIDs. `M-x my/org-deps-edit' on a task opens a dispatch menu:
;;
;;   [a]dd            pick another task + direction, write :BLOCKER:
;;   [m]ark           remember this task (its ID) for a later link
;;   [l]ink to marked link the marked task to this one, pick direction
;;   [d]elete         remove dependencies in either direction, one RET per pick
;;
;; Direction is user-chosen (blocks / blocked-by). edna's :BLOCKER: always
;; lives on the BLOCKED task, so direction decides which heading is edited:
;;   X blocks Y     -> write Y's  :BLOCKER: ids(<X-id>)
;;   X blocked-by Y -> write X's  :BLOCKER: ids(<Y-id>)
;;
;; Blockers merge into one space-separated ids(...) segment (de-duplicated),
;; matching the reader in init-org-deps-graph.el. Optionally a :TRIGGER: is
;; added on the blocker so finishing it advances the blocked task to NEXT.
;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)

(defgroup my-org-edna-edit nil
  "Interactive editing of org-edna task dependencies."
  :group 'org)

(defcustom my-org-edna-trigger-state "NEXT"
  "TODO state a generated :TRIGGER: advances the blocked task to."
  :type 'string
  :group 'my-org-edna-edit)

(defvar my--org-edna-marked nil
  "Marked task for `my/org-deps-link-marked'.
A plist (:id ID :title TITLE :file FILE).")

;;; ------------------------------------------------------------------
;;; Pure logic (unit-tested)
;;; ------------------------------------------------------------------

(defun my--org-edna-parse-ids (blocker)
  "Return the list of UUIDs inside every ids(...) in BLOCKER string.
BLOCKER may be nil or empty; returns nil then.  Order preserved."
  (let ((ids '()) (start 0))
    (when (and blocker (stringp blocker))
      (while (string-match "ids(\\([^)]*\\))" blocker start)
        (dolist (u (split-string (match-string 1 blocker) "[ \t]+" t))
          (push u ids))
        (setq start (match-end 0))))
    (nreverse ids)))

(defun my--org-edna-merge-ids (blocker new-id)
  "Merge NEW-ID into BLOCKER's ids(...) segment, de-duplicated.
BLOCKER is the current :BLOCKER: value (or nil).  Any non-ids() content
in BLOCKER is preserved and kept after a single merged ids(...) segment.
Returns the new :BLOCKER: string."
  (let* ((ids (my--org-edna-parse-ids blocker))
         (ids (if (member new-id ids) ids (append ids (list new-id))))
         ;; strip existing ids(...) segments, keep the remainder (e.g. a
         ;; trailing todo?(DONE) condition) so we don't drop it on rewrite
         (rest (string-trim
                (replace-regexp-in-string "ids([^)]*)" "" (or blocker "")))))
    (string-trim
     (concat "ids(" (mapconcat #'identity ids " ") ")"
             (unless (string-empty-p rest) (concat " " rest))))))

(defun my--org-edna-remove-ids (blocker drop-ids)
  "Remove DROP-IDS (list of UUIDs) from BLOCKER's ids(...) segment.
Returns the new :BLOCKER: string, or nil when no ids remain and there is
no other content (meaning the property should be deleted)."
  (let* ((ids (cl-remove-if (lambda (u) (member u drop-ids))
                            (my--org-edna-parse-ids blocker)))
         (rest (string-trim
                (replace-regexp-in-string "ids([^)]*)" "" (or blocker "")))))
    (cond
     (ids (string-trim
           (concat "ids(" (mapconcat #'identity ids " ") ")"
                   (unless (string-empty-p rest) (concat " " rest)))))
     ((not (string-empty-p rest)) rest)
     (t nil))))

(defun my--org-edna-resolve-roles (current other direction)
  "Return (BLOCKED . BLOCKER) markers for DIRECTION.
CURRENT and OTHER are markers; DIRECTION is `blocks' (CURRENT blocks
OTHER) or `blocked-by' (CURRENT is blocked by OTHER)."
  (pcase direction
    ('blocks       (cons other current))
    ('blocked-by   (cons current other))
    (_ (error "Unknown direction: %S" direction))))

;;; ------------------------------------------------------------------
;;; Property mutation (temp-buffer testable via markers)
;;; ------------------------------------------------------------------

(defun my--org-edna-add-blocker (blocked blocker &optional trigger)
  "Write a dependency: BLOCKED gains BLOCKER's ID in :BLOCKER: ids(...).
BLOCKED and BLOCKER are markers.  Creates BLOCKER's ID on demand.  When
TRIGGER is non-nil also writes BLOCKER's :TRIGGER: advancing BLOCKED to
`my-org-edna-trigger-state'."
  (let* ((blocker-id (save-excursion
                       (goto-char blocker)
                       (org-id-get-create)))
         (blocked-id (when trigger
                       (save-excursion
                         (goto-char blocked)
                         (org-id-get-create)))))
    (save-excursion
      (goto-char blocked)
      (let* ((cur (org-entry-get (point) "BLOCKER"))
             (new (my--org-edna-merge-ids cur blocker-id)))
        (org-entry-put (point) "BLOCKER" new)))
    (when trigger
      (save-excursion
        (goto-char blocker)
        (org-entry-put (point) "TRIGGER"
                       (format "ids(%s) todo!(%s)"
                               blocked-id my-org-edna-trigger-state))))
    blocker-id))

(defun my--org-edna-delete-blockers (task drop-ids)
  "Remove DROP-IDS from TASK's :BLOCKER:; delete the property if empty.
TASK is a marker."
  (save-excursion
    (goto-char task)
    (let* ((cur (org-entry-get (point) "BLOCKER"))
           (new (my--org-edna-remove-ids cur drop-ids)))
      (if new
          (org-entry-put (point) "BLOCKER" new)
        (org-entry-delete (point) "BLOCKER")))))

;;; ------------------------------------------------------------------
;;; Interactive layer
;;; ------------------------------------------------------------------

(defun my--org-edna-buffer-tasks ()
  "Return an alist (TITLE . MARKER) for every heading in this buffer."
  (let (tasks)
    (org-with-wide-buffer
     (goto-char (point-min))
     (org-map-entries
      (lambda ()
        (push (cons (org-get-heading t t t t) (point-marker)) tasks))))
    (nreverse tasks)))

(defun my--org-edna-read-task (prompt &optional exclude-marker)
  "completing-read a buffer task, PROMPT.  Return its marker.
EXCLUDE-MARKER, if set, is omitted from the candidates."
  (let* ((all (my--org-edna-buffer-tasks))
         (cands (cl-remove-if
                 (lambda (c) (and exclude-marker
                                  (= (cdr c) exclude-marker)))
                 all))
         (choice (completing-read prompt (mapcar #'car cands) nil t)))
    (cdr (assoc choice cands))))

(defun my--org-edna-task-summary (marker)
  "Return a summary plist for the task at MARKER.
\(:title TITLE :todo TODO :blockers (TITLE ...)), where :blockers lists
the headings this task is currently blocked by (its :BLOCKER: ids(...)
resolved to titles; unresolved ids show as \"<unknown ID>\")."
  (save-excursion
    (goto-char marker)
    (let* ((title (org-get-heading t t t t))
           (todo (org-get-todo-state))
           (blk (org-entry-get (point) "BLOCKER"))
           (ids (my--org-edna-parse-ids blk))
           (blockers
            (mapcar
             (lambda (id)
               (let ((pos (org-find-entry-with-id id)))
                 (if pos
                     (save-excursion (goto-char pos) (org-get-heading t t t t))
                   (format "<unknown %s>" id))))
             ids)))
      (list :title title :todo todo :blockers blockers))))

(defun my--org-edna-format-summary (summary)
  "Render a task SUMMARY plist (from `my--org-edna-task-summary') to text."
  (let ((title (plist-get summary :title))
        (todo (plist-get summary :todo))
        (blockers (plist-get summary :blockers)))
    (concat
     (format "  %-6s %s\n" (or todo "") title)
     (if blockers
         (mapconcat (lambda (b) (format "         ↑ blocked by: %s\n" b))
                    blockers "")
       "         (no blockers)\n"))))

(defun my--org-edna-preview (current other)
  "Show CURRENT and OTHER task summaries in a temp window.
Returns the preview buffer so the caller can bury/kill it."
  (let ((buf (get-buffer-create "*Org Deps Preview*"))
        (cur-sum (my--org-edna-task-summary current))
        (other-sum (my--org-edna-task-summary other)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Confirm the relation between these two tasks:\n\n")
        (insert "current:\n" (my--org-edna-format-summary cur-sum) "\n")
        (insert "other:\n" (my--org-edna-format-summary other-sum)))
      (goto-char (point-min))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom (window-height . 12)))
    buf))

(defun my--org-edna-read-direction (&optional current other)
  "Ask the user for the relation direction.  Return a symbol.
When CURRENT and OTHER markers are given, first show a preview of both
tasks' TODO state and existing blockers so the user can compare before
choosing."
  (let ((buf (when (and current other)
               (my--org-edna-preview current other))))
    (unwind-protect
        (pcase (completing-read "Direction: " '("blocks" "blocked-by") nil t)
          ("blocks" 'blocks)
          ("blocked-by" 'blocked-by))
      (when (buffer-live-p buf)
        (quit-windows-on buf)))))

(defun my--org-edna-maybe-trigger ()
  "Ask whether to add a TRIGGER.  Return non-nil on yes."
  (y-or-n-p
   (format "Add TRIGGER so the blocker advances the blocked task to %s? "
           my-org-edna-trigger-state)))

(defun my--org-edna-do-add (current)
  "Add a dependency from CURRENT marker via completion + direction."
  (let* ((other (my--org-edna-read-task "Other task: " current))
         (dir (my--org-edna-read-direction current other))
         (roles (my--org-edna-resolve-roles current other dir))
         (trigger (my--org-edna-maybe-trigger)))
    (my--org-edna-add-blocker (car roles) (cdr roles) trigger)
    (message "Dependency written (%s)." dir)))

(defun my--org-edna-do-mark (current)
  "Mark CURRENT task for a later link."
  (save-excursion
    (goto-char current)
    (setq my--org-edna-marked
          (list :id (org-id-get-create)
                :title (org-get-heading t t t t)
                :file (buffer-file-name)))
    (message "Marked: %s" (plist-get my--org-edna-marked :title))))

(defun my--org-edna-do-link (current)
  "Link the marked task to CURRENT, asking direction.
The marked task is resolved to a marker (same buffer by id)."
  (unless my--org-edna-marked
    (user-error "No marked task; use [m] first"))
  (let* ((mid (plist-get my--org-edna-marked :id))
         (mpos (org-find-entry-with-id mid)))
    (unless mpos
      (user-error "Marked task %s not found in this buffer"
                  (plist-get my--org-edna-marked :title)))
    (let* ((other (copy-marker mpos))
           (dir (my--org-edna-read-direction current other))
           (roles (my--org-edna-resolve-roles current other dir))
           (trigger (my--org-edna-maybe-trigger)))
      (my--org-edna-add-blocker (car roles) (cdr roles) trigger)
      (message "Linked %s (%s)."
               (plist-get my--org-edna-marked :title) dir))))

(defun my--org-edna-reverse-refs (id)
  "Return ((TITLE . MARKER) ...) for this-buffer headings blocked by ID.
A heading is blocked by ID when ID appears in its :BLOCKER: ids(...)."
  (let (hits)
    (when id
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (when (member id (my--org-edna-parse-ids
                            (org-entry-get nil "BLOCKER")))
            (push (cons (org-get-heading t t t t) (point-marker)) hits))))))
    (nreverse hits)))

(defun my--org-edna-delete-candidates (current)
  "Return ((LABEL . TARGET) ...) for every dependency of CURRENT.
Blocked-by candidates come first, then blocking candidates.  TARGET is
\(upstream . UUID) for a task blocking CURRENT, or
\(downstream . MARKER) for a task CURRENT blocks."
  (let* ((up (save-excursion
               (goto-char current)
               (my--org-edna-parse-ids (org-entry-get nil "BLOCKER"))))
         (self-id (save-excursion (goto-char current)
                                  (org-entry-get nil "ID")))
         (up-cands '())
         (down-cands '()))
    (dolist (uuid up)
      (let ((pos (org-find-entry-with-id uuid)))
        (push (cons (format "<- %s"
                            (if pos
                                (save-excursion (goto-char pos)
                                                (org-get-heading t t t t))
                              (format "<unknown %s>" uuid)))
                    (cons 'upstream uuid))
              up-cands)))
    (dolist (hit (my--org-edna-reverse-refs self-id))
      (unless (= (marker-position (cdr hit)) current)
        (push (cons (format "-> %s" (car hit))
                    (cons 'downstream (cdr hit)))
              down-cands)))
    (append (nreverse up-cands) (nreverse down-cands))))

(defun my--org-edna-read-picks (prompt candidates)
  "Read any number of picks from CANDIDATES, one RET per accepted pick.
Each accepted pick is removed from the remaining candidates.  RET on
empty input applies the picks; C-g cancels all of them.  Return
\(ABORTED . PICKS)."
  (let ((remaining candidates) (picks '()) (aborted nil) (done nil))
    (while (and (not done) remaining)
      (let ((input (condition-case nil
                       (completing-read
                        (format "%s [%d picked; empty RET = apply; C-g = cancel]\n  <- blocked-by (that task blocks this)   -> blocks (this task blocks that): "
                                prompt (length picks))
                        (mapcar #'car remaining) nil nil)
                     (quit (setq aborted t) nil))))
        (cond
         (aborted (setq done t))
         ((or (null input) (string-empty-p input)) (setq done t))
         ((assoc input remaining)
          (let ((hit (assoc input remaining)))
            (push hit picks)
            (setq remaining (delete hit remaining))))
         (t (message "Not a candidate: %s" input)))))
    (cons aborted (nreverse picks))))

(defun my--org-edna-do-delete (current)
  "Remove dependencies of CURRENT in both directions, interactively.
Upstream picks edit CURRENT's :BLOCKER:; downstream picks edit the
blocking task's :BLOCKER:.  One dependency per RET, C-g cancels."
  (let ((cands (my--org-edna-delete-candidates current)))
    (unless cands
      (user-error "This task has no dependencies"))
    (pcase-let* ((`(,aborted . ,picks)
                  (my--org-edna-read-picks "Remove dep:" cands)))
      (when aborted
        (user-error "Cancelled"))
      (unless picks
        (user-error "Nothing selected"))
      (let ((up '()) (down '())
            (self-id (save-excursion (goto-char current)
                                     (org-entry-get nil "ID"))))
        (dolist (p picks)
          (pcase (cdr p)
            (`(upstream . ,uuid) (push uuid up))
            (`(downstream . ,marker) (push marker down))))
        (when up
          (my--org-edna-delete-blockers current up))
        (dolist (m down)
          (my--org-edna-delete-blockers m (list self-id)))
        (message "Removed %d dependency(ies): %d blocked-by, %d blocks."
                 (length picks) (length up) (length down))))))

;;;###autoload
(defun my/org-deps-edit ()
  "Interactively edit org-edna dependencies for the task at point."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (save-excursion
    (org-back-to-heading t)
    (let ((current (point-marker)))
      (pcase (read-char-choice
              (format "Deps on \"%s\":  [a]dd  [m]ark  [l]ink to marked  [d]elete  [q]uit "
                      (org-get-heading t t t t))
              '(?a ?m ?l ?d ?q))
        (?a (my--org-edna-do-add current))
        (?m (my--org-edna-do-mark current))
        (?l (my--org-edna-do-link current))
        (?d (my--org-edna-do-delete current))
        (?q (message "Quit"))))))

(provide 'init-org-edna-edit)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org-edna-edit.el ends here
