;;; init-org-edna-edit.el --- Interactive org-edna dependency editor -*- lexical-binding: t -*-
;;; Commentary:
;; Author org-edna task dependencies directly from a heading, without hand
;; copying UUIDs. `M-x my/org-deps-edit' on a task opens a dispatch menu:
;;
;;   [a]dd            pick another task + direction, write :BLOCKER:
;;   [m]ark           remember this task (its ID) for a later link
;;   [l]ink to marked link the marked task to this one, pick direction
;;   [d]elete         remove selected blockers from this task
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

(defun my--org-edna-read-direction ()
  "Ask the user for the relation direction.  Return a symbol."
  (pcase (completing-read "Direction: " '("blocks" "blocked-by") nil t)
    ("blocks" 'blocks)
    ("blocked-by" 'blocked-by)))

(defun my--org-edna-maybe-trigger ()
  "Ask whether to add a TRIGGER.  Return non-nil on yes."
  (y-or-n-p
   (format "Add TRIGGER so the blocker advances the blocked task to %s? "
           my-org-edna-trigger-state)))

(defun my--org-edna-do-add (current)
  "Add a dependency from CURRENT marker via completion + direction."
  (let* ((other (my--org-edna-read-task "Other task: " current))
         (dir (my--org-edna-read-direction))
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
           (dir (my--org-edna-read-direction))
           (roles (my--org-edna-resolve-roles current other dir))
           (trigger (my--org-edna-maybe-trigger)))
      (my--org-edna-add-blocker (car roles) (cdr roles) trigger)
      (message "Linked %s (%s)."
               (plist-get my--org-edna-marked :title) dir))))

(defun my--org-edna-do-delete (current)
  "Delete selected blockers from CURRENT task."
  (let* ((cur (save-excursion (goto-char current)
                              (org-entry-get (point) "BLOCKER")))
         (ids (my--org-edna-parse-ids cur)))
    (unless ids
      (user-error "This task has no blockers"))
    (let* ((labeled (mapcar
                     (lambda (id)
                       (let ((pos (org-find-entry-with-id id)))
                         (cons (if pos
                                   (save-excursion
                                     (goto-char pos)
                                     (org-get-heading t t t t))
                                 (format "<unknown %s>" id))
                               id)))
                     ids))
           (picks (completing-read-multiple
                   "Remove blockers (comma-separated): "
                   (mapcar #'car labeled) nil t))
           (drop (delq nil (mapcar (lambda (p) (cdr (assoc p labeled)))
                                   picks))))
      (my--org-edna-delete-blockers current drop)
      (message "Removed %d blocker(s)." (length drop)))))

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
