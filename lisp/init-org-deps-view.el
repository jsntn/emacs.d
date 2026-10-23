;;; init-org-deps-view.el --- Read-only view of a task's dependencies -*- lexical-binding: t -*-
;;; Commentary:
;; `M-x my/org-deps-view' shows the task at point with both dependency
;; directions, blocked-by first:
;;
;;   <- BLOCKED BY   tasks that block this one
;;   -> BLOCKS       tasks that wait for this one
;;
;; The buffer is a real Org buffer, so entries are fontified exactly like
;; normal Org headings: TODO keyword faces, and `org-headline-done' on
;; completed tasks (which the user's config strikes through).
;;
;; Read-only.  RET/mouse-1 jumps to an entry, n/p move between entries,
;; D toggles hiding completed tasks, g refreshes, q closes the window.
;;
;; The view reads through the same parsers as the editor
;; (`my--org-edna-parse-ids', `my--org-edna-reverse-refs'), so it and
;; [a]/[d] can never disagree about what an edge is.  Scope is the
;; current buffer, like the editor: `org-find-entry-with-id' and the
;; reverse scan are buffer-local.
;;
;; DONE tasks are listed by default, even though `my/org-deps-graph'
;; hides them, so completed-but-still-wired edges stay visible.  D hides
;; them without touching the underlying data.
;;; Code:

(require 'org)
(require 'org-id)
(require 'cl-lib)
(require 'init-org-edna-edit)

(defgroup my-org-deps-view nil
  "Read-only view of org-edna task dependencies."
  :group 'org)

(defface my-org-deps-view-section
  '((t :inherit bold))
  "Face for dependency section headers.")

(defface my-org-deps-view-meta
  '((t :inherit shadow))
  "Face for secondary text: paths, dates, hints.")

(defface my-org-deps-view-unknown
  '((t :inherit error))
  "Face for UUIDs that do not resolve in the current buffer.")

(defcustom my-org-deps-view-width 62
  "Width in columns of the view window when it splits to the right."
  :type 'integer
  :group 'my-org-deps-view)

(defcustom my-org-deps-view-height 14
  "Height in lines of the view window when the frame is narrow."
  :type 'integer
  :group 'my-org-deps-view)

(defcustom my-org-deps-view-narrow-width 100
  "Frame width below which the view splits below instead of to the right."
  :type 'integer
  :group 'my-org-deps-view)

(defconst my--org-deps-view-name "*Org Deps*"
  "Buffer name of the dependency view.")

(defvar-local my--org-deps-view-source nil
  "Marker of the task whose dependencies this buffer shows.")

(defvar-local my--org-deps-view-hide-done nil
  "When non-nil, completed tasks are omitted from the view.")

;;; ------------------------------------------------------------------
;;; Data
;;; ------------------------------------------------------------------

(defun my--org-deps-view-at (marker)
  "Return a plist describing the heading at MARKER.
Keys: :title :todo :path :when :marker."
  (save-excursion
    (goto-char marker)
    (let ((deadline (org-get-deadline-time (point)))
          (scheduled (org-get-scheduled-time (point))))
      (list :title (org-get-heading t t t t)
            :todo (org-get-todo-state)
            :path (mapconcat #'identity (org-get-outline-path) " / ")
            :when (cond (deadline (format "deadline %s"
                                          (format-time-string "%m-%d" deadline)))
                        (scheduled (format "scheduled %s"
                                           (format-time-string "%m-%d" scheduled))))
            :marker (copy-marker (point))))))

(defun my--org-deps-view-data (source)
  "Collect both dependency directions of the heading at SOURCE.
Return (:self ENTRY :blocked-by (ENTRY...) :blocks (ENTRY...)).
An unresolvable UUID appears as an entry with only :unknown."
  (with-current-buffer (marker-buffer source)
    (save-excursion
      (goto-char source)
      (let ((self (my--org-deps-view-at source))
            (ids (my--org-edna-parse-ids (org-entry-get nil "BLOCKER")))
            (self-id (org-entry-get nil "ID"))
            (up '())
            (down '()))
        (dolist (id ids)
          (let ((pos (org-find-entry-with-id id)))
            (push (if pos
                      (my--org-deps-view-at (copy-marker pos))
                    (list :unknown id))
                  up)))
        (dolist (hit (my--org-edna-reverse-refs self-id))
          (unless (= (marker-position (cdr hit)) source)
            (push (my--org-deps-view-at (cdr hit)) down)))
        (list :self self
              :blocked-by (nreverse up)
              :blocks (nreverse down))))))

(defun my--org-deps-view-done-p (entry)
  "Return non-nil when ENTRY's TODO keyword counts as done."
  (let ((todo (plist-get entry :todo)))
    (and todo (member todo org-done-keywords))))

;;; ------------------------------------------------------------------
;;; Rendering
;;; ------------------------------------------------------------------

(defun my--org-deps-view-insert-entry (entry)
  "Insert ENTRY as an Org heading line plus an optional metadata line."
  (if (plist-get entry :unknown)
      (insert (propertize (format "  <unknown %s>" (plist-get entry :unknown))
                          'face 'my-org-deps-view-unknown)
              (propertize "  not in this buffer" 'face 'my-org-deps-view-meta)
              "\n")
    (let* ((title (let ((t0 (plist-get entry :title)))
                    (if (string-empty-p t0) "(untitled)" t0)))
           (todo (plist-get entry :todo))
           (path (plist-get entry :path))
           (when-text (plist-get entry :when))
           (meta (mapconcat #'identity
                            (delq nil
                                  (list when-text
                                        (and path (not (string-empty-p path)) path)))
                            " · ")))
      (insert "* " (if todo (concat todo " ") ""))
      (insert-text-button title
                          'action #'my--org-deps-view-jump
                          'marker (plist-get entry :marker)
                          'follow-link t
                          'help-echo (concat title
                                             (if (string-empty-p meta)
                                                 ""
                                               (concat "\n" meta))
                                             "\nRET jumps to the task"))
      (insert "\n")
      (unless (string-empty-p meta)
        (insert (propertize (concat "  " meta) 'face 'my-org-deps-view-meta)
                "\n")))))

(defun my--org-deps-view-insert-section (arrow header hint entries)
  "Insert a non-empty section with ARROW, HEADER, HINT and ENTRIES."
  (insert (propertize (format "%s %s (%d)" arrow header (length entries))
                      'face 'my-org-deps-view-section)
          (propertize (format "   %s" hint) 'face 'my-org-deps-view-meta)
          "\n")
  (dolist (entry entries)
    (my--org-deps-view-insert-entry entry)))

(defun my--org-deps-view-rule (buffer)
  "Return a horizontal rule matching BUFFER's window width."
  (let* ((win (get-buffer-window buffer))
         (width (if (window-live-p win)
                    (window-body-width win)
                  my-org-deps-view-width)))
    (make-string (max 20 (- width 2)) ?─)))

(defun my--org-deps-view-render (buffer source)
  "Render SOURCE's dependencies into BUFFER, keeping the current line."
  (with-current-buffer buffer
    (let ((line (line-number-at-pos))
          (inhibit-read-only t))
      (erase-buffer)
      (unless (derived-mode-p 'my-org-deps-view-mode)
        (let ((org-mode-hook nil)
              (org-inhibit-startup t))
          (my-org-deps-view-mode)))
      (setq my--org-deps-view-source source)
      (let* ((data (my--org-deps-view-data source))
             (self (plist-get data :self))
             (up-all (plist-get data :blocked-by))
             (down-all (plist-get data :blocks))
             (up (if my--org-deps-view-hide-done
                     (cl-remove-if #'my--org-deps-view-done-p up-all)
                   up-all))
             (down (if my--org-deps-view-hide-done
                       (cl-remove-if #'my--org-deps-view-done-p down-all)
                     down-all))
             (hidden (+ (- (length up-all) (length up))
                        (- (length down-all) (length down))))
             (counts (mapconcat
                      #'identity
                      (append (when up (list (format "%d blocked by" (length up))))
                              (when down (list (format "%d blocks" (length down)))))
                      " · ")))
        (insert (propertize "Org deps: " 'face 'my-org-deps-view-meta)
                (propertize (or (plist-get self :title) "") 'face 'bold)
                (or (when (plist-get self :todo)
                      (concat "  " (propertize (format "[%s]" (plist-get self :todo))
                                               'face (or (org-get-todo-face
                                                          (plist-get self :todo))
                                                         'org-todo))))
                    "")
                (or (when (plist-get self :when)
                      (concat "  " (propertize (plist-get self :when)
                                               'face 'my-org-deps-view-meta)))
                    "")
                (if (string-empty-p counts)
                    ""
                  (concat "   " (propertize counts 'face 'my-org-deps-view-meta)))
                (if (> hidden 0)
                    (concat "   " (propertize (format "%d done hidden" hidden)
                                              'face 'my-org-deps-view-meta))
                  "")
                "\n"
                (propertize (my--org-deps-view-rule buffer)
                            'face 'my-org-deps-view-meta)
                "\n\n")
        (cond
         ((and (null up) (null down))
          (insert (propertize (if (> hidden 0)
                                  (format "  All %d dependencies are completed and hidden.\n"
                                          hidden)
                                "  No dependencies in this buffer.\n")
                              'face 'my-org-deps-view-meta)))
         (t
          (when up
            (my--org-deps-view-insert-section
             "<-" "BLOCKED BY" "tasks that block this one" up))
          (when (and up down)
            (insert "\n"))
          (when down
            (my--org-deps-view-insert-section
             "->" "BLOCKS" "tasks that wait for this one" down))))
        (insert "\n"
                (propertize
                 (format "RET jump   n/p move   D %s done   g refresh   q close"
                         (if my--org-deps-view-hide-done "show" "hide"))
                 'face 'my-org-deps-view-meta))
        (when (fboundp 'font-lock-ensure)
          (font-lock-ensure))
        (goto-char (point-min))
        (forward-line (1- line))))))

(defun my--org-deps-view-display-window (buffer _alist)
  "Display BUFFER in a normal window sized by the view options.
A normal window, not a side window: side windows are outside the main
window tree, so `delete-other-windows' ignores them and `zoom-mode'
cannot resize them (`zoom--resize' works on `window-list' and
`balance-windows')."
  (let ((win (get-buffer-window buffer 'visible)))
    (unless (window-live-p win)
      (setq win
            (condition-case nil
                (if (< (frame-width) my-org-deps-view-narrow-width)
                    ;; negative SIZE gives it to the new (lower) window
                    (split-window-below
                     (- (max window-min-height
                             (min my-org-deps-view-height
                                  (- (window-height) window-min-height 1)))))
                  (split-window-right
                   (- (max window-min-width
                           (min my-org-deps-view-width
                                (- (frame-width) 20))))))
              (error (display-buffer-pop-up-window buffer nil)))))
    (set-window-buffer win buffer)
    win))

(defun my--org-deps-view-display (buffer)
  "Show BUFFER in a normal window: right, or below on a narrow frame."
  (display-buffer buffer '((my--org-deps-view-display-window))))

(defun my--org-deps-view-show (source)
  "Render and display the dependencies of SOURCE.  Return the buffer.
The buffer is displayed before rendering so the horizontal rule can
match the real window width."
  (let ((buffer (get-buffer-create my--org-deps-view-name)))
    (my--org-deps-view-display buffer)
    (my--org-deps-view-render buffer source)
    buffer))

;;; ------------------------------------------------------------------
;;; Interaction
;;; ------------------------------------------------------------------

(defun my--org-deps-view-jump (button)
  "Jump to the task behind BUTTON."
  (let ((marker (button-get button 'marker)))
    (unless (and (markerp marker) (marker-buffer marker)
                 (buffer-live-p (marker-buffer marker)))
      (user-error "Task buffer is gone"))
    (let ((buffer (marker-buffer marker))
          (win nil))
      (setq win (or (get-buffer-window buffer 'visible)
                    (display-buffer
                     buffer
                     '((display-buffer-reuse-window
                        display-buffer-pop-up-window)
                       (inhibit-same-window . t)))))
      (when (window-live-p win)
        (select-window win)
        (with-current-buffer buffer
          (goto-char marker)
          (org-reveal)
          (recenter))))))

(defun my--org-deps-view-ret ()
  "Jump to the dependency entry at point."
  (interactive)
  (let ((button (button-at (point))))
    (unless button
      (let ((pos (next-button (line-beginning-position))))
        (when (and pos (< pos (line-end-position)))
          (setq button (button-at pos)))))
    (if button
        (my--org-deps-view-jump button)
      (user-error "No task at point"))))

(defun my--org-deps-view-next ()
  "Move to the next dependency entry."
  (interactive)
  (let ((pos (next-button (point))))
    (if pos (goto-char pos) (message "No more entries"))))

(defun my--org-deps-view-prev ()
  "Move to the previous dependency entry."
  (interactive)
  (let ((pos (previous-button (point))))
    (if pos (goto-char pos) (message "No previous entry"))))

(defun my--org-deps-view-toggle-done ()
  "Toggle whether completed tasks are hidden in the view."
  (interactive)
  (setq my--org-deps-view-hide-done (not my--org-deps-view-hide-done))
  (my--org-deps-view-revert)
  (message "%s completed tasks."
           (if my--org-deps-view-hide-done "Hiding" "Showing")))

(defun my--org-deps-view-revert (&rest _)
  "Re-render the view from its stored source marker."
  (unless (and (markerp my--org-deps-view-source)
               (marker-buffer my--org-deps-view-source)
               (buffer-live-p (marker-buffer my--org-deps-view-source)))
    (user-error "Source task is no longer available"))
  (my--org-deps-view-render (current-buffer) my--org-deps-view-source))

(defvar my-org-deps-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    (define-key map (kbd "RET") #'my--org-deps-view-ret)
    (define-key map (kbd "n") #'my--org-deps-view-next)
    (define-key map (kbd "p") #'my--org-deps-view-prev)
    (define-key map (kbd "D") #'my--org-deps-view-toggle-done)
    (define-key map (kbd "g") #'revert-buffer)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `my-org-deps-view-mode'.")

(define-derived-mode my-org-deps-view-mode org-mode "OrgDeps"
  "Major mode for the read-only dependency view.
\\{my-org-deps-view-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'my--org-deps-view-revert)
  (setq buffer-read-only t))

;;;###autoload
(defun my/org-deps-view ()
  "Show the dependencies of the task at point.
Inside the view buffer, shows the dependencies of the entry at point
or refreshes the current one."
  (interactive)
  (cond
   ((derived-mode-p 'my-org-deps-view-mode)
    (let* ((button (button-at (point)))
           (marker (or (and button (button-get button 'marker))
                       my--org-deps-view-source)))
      (unless (and (markerp marker) (marker-buffer marker))
        (user-error "No source task"))
      (my--org-deps-view-show marker)))
   ((derived-mode-p 'org-mode)
    (save-excursion
      (condition-case nil
          (org-back-to-heading t)
        (error (user-error "No Org heading at point")))
      (my--org-deps-view-show (point-marker))))
   (t (user-error "Not in an Org buffer"))))

(provide 'init-org-deps-view)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org-deps-view.el ends here
