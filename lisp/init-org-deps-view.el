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

(defvar-local my--org-deps-view-return-config nil
  "Window configuration from before the view was displayed.")

(defvar-local my--org-deps-view-return-point nil
  "Marker of point in the parent window before the view was displayed.")

(defvar-local my--org-deps-view-rendered-width nil
  "Window body width used for the last render.")

(defvar my--org-deps-view--refreshing nil
  "Non-nil while `my--org-deps-view--window-size-change' re-renders.")

;; `my-org-deps-view-mode' runs after these are captured; keep them alive
;; across the major-mode change.
(put 'my--org-deps-view-return-config 'permanent-local t)
(put 'my--org-deps-view-return-point 'permanent-local t)

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

(defun my--org-deps-view-display-title (entry)
  "Return ENTRY's title as displayed, substituting an empty title."
  (let ((title (or (plist-get entry :title) "")))
    (if (string-empty-p title) "(untitled)" title)))

(defun my--org-deps-view-insert-entry (entry todo-width title-width)
  "Insert ENTRY as one Org heading line, columns per TODO-WIDTH/TITLE-WIDTH.
The metadata (date, outline path) is appended to the heading so each
task occupies a single line, and every metadata column starts at the
same offset across all sections."
  (if (plist-get entry :unknown)
      (insert (make-string (+ 2 todo-width 1) ?\s)
              (propertize (format "<unknown %s>" (plist-get entry :unknown))
                          'face 'my-org-deps-view-unknown)
              (propertize "  not in this buffer" 'face 'my-org-deps-view-meta)
              "\n")
    (let* ((title (my--org-deps-view-display-title entry))
           (todo (or (plist-get entry :todo) ""))
           (path (plist-get entry :path))
           (when-text (plist-get entry :when))
           (meta (mapconcat #'identity
                            (delq nil
                                  (list when-text
                                        (and path (not (string-empty-p path)) path)))
                            " · "))
           (start (point)))
      (insert "* "
              (if (string-empty-p todo)
                  (make-string todo-width ?\s)
                (concat todo
                        (make-string (max 0 (- todo-width (string-width todo))) ?\s)))
              " " title)
      (unless (string-empty-p meta)
        (insert (make-string (+ 3 (max 0 (- title-width (string-width title)))) ?\s)
                "· " meta))
      (insert "\n")
      ;; Plain text with a marker property: Org font-lock owns the faces,
      ;; so the heading looks exactly like a normal Org heading.
      (add-text-properties
       start (1- (point))
       (list 'my-org-deps-marker (plist-get entry :marker)
             'mouse-face 'highlight
             'help-echo (concat title
                                (if (string-empty-p meta) "" (concat "\n" meta))
                                "\nRET jumps to the task"))))))

(defun my--org-deps-view-insert-section (arrow header hint entries
                                               todo-width title-width)
  "Insert a non-empty section with ARROW, HEADER, HINT and ENTRIES.
TODO-WIDTH and TITLE-WIDTH are shared by every section so columns line
up across the whole buffer."
  (insert (propertize (format "%s %s (%d)" arrow header (length entries))
                      'face 'my-org-deps-view-section)
          (propertize (format "   %s" hint) 'face 'my-org-deps-view-meta)
          "\n")
  (dolist (entry entries)
    (my--org-deps-view-insert-entry entry todo-width title-width)))

(defun my--org-deps-view-rule (buffer)
  "Return a horizontal rule filling BUFFER's window width."
  (let* ((win (get-buffer-window buffer))
         (width (if (window-live-p win)
                    (window-body-width win)
                  my-org-deps-view-width)))
    (make-string (max 20 width) ?─)))

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
      ;; Re-apply both for buffers created before a reload: the keymap is
      ;; rebuilt by `setq', and Evil's state maps must stay out of the way.
      (use-local-map my-org-deps-view-mode-map)
      (when (fboundp 'evil-local-mode)
        (evil-local-mode -1))
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
             (real (cl-remove-if (lambda (e) (plist-get e :unknown))
                                 (append up down)))
             (todo-width (apply #'max 0
                                (mapcar (lambda (e)
                                          (string-width (or (plist-get e :todo) "")))
                                        real)))
             (title-width (apply #'max 0
                                 (mapcar (lambda (e)
                                           (string-width
                                            (my--org-deps-view-display-title e)))
                                         real)))
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
             "<-" "BLOCKED BY" "tasks that block this one" up
             todo-width title-width))
          (when (and up down)
            (insert "\n"))
          (when down
            (my--org-deps-view-insert-section
             "->" "BLOCKS" "tasks that wait for this one" down
             todo-width title-width))))
        (insert "\n"
                (propertize (my--org-deps-view-rule buffer)
                            'face 'my-org-deps-view-meta)
                "\n"
                (propertize
                 (format "RET jump   j/k move   D %s done   g refresh   q close"
                         (if my--org-deps-view-hide-done "show" "hide"))
                 'face 'my-org-deps-view-meta))
        (when (fboundp 'font-lock-ensure)
          (font-lock-ensure))
        (setq-local my--org-deps-view-rendered-width
                    (let ((win (get-buffer-window buffer)))
                      (and (window-live-p win) (window-body-width win))))
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
    (unless (get-buffer-window buffer)
      (with-current-buffer buffer
        (setq-local my--org-deps-view-return-config
                    (current-window-configuration))
        (setq-local my--org-deps-view-return-point
                    (copy-marker (window-point (selected-window))))))
    (my--org-deps-view-display buffer)
    (my--org-deps-view-render buffer source)
    buffer))

;;; ------------------------------------------------------------------
;;; Interaction
;;; ------------------------------------------------------------------

(defun my--org-deps-view-jump (marker)
  "Jump to the task at MARKER."
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
        (recenter)))))

(defun my--org-deps-view-marker-at-point ()
  "Return the task marker for the entry at point, or nil."
  (or (get-text-property (point) 'my-org-deps-marker)
      (get-text-property (line-beginning-position) 'my-org-deps-marker)))

(defun my--org-deps-view-ret ()
  "Jump to the dependency entry at point."
  (interactive)
  (let ((marker (my--org-deps-view-marker-at-point)))
    (unless marker
      (user-error "No task at point"))
    (my--org-deps-view-jump marker)))

(defun my--org-deps-view-mouse-jump (event)
  "Jump to the dependency clicked with EVENT."
  (interactive "e")
  (mouse-set-point event)
  (let ((marker (my--org-deps-view-marker-at-point)))
    (when marker
      (my--org-deps-view-jump marker))))

(defun my--org-deps-view--entry-line (dir)
  "Return the start of the next entry line in direction DIR, or nil."
  (save-excursion
    (forward-line dir)
    (while (and (not (eobp))
                (not (bobp))
                (null (get-text-property (line-beginning-position)
                                         'my-org-deps-marker)))
      (forward-line dir))
    (when (get-text-property (line-beginning-position) 'my-org-deps-marker)
      (line-beginning-position))))

(defun my--org-deps-view-next ()
  "Move to the next dependency entry."
  (interactive)
  (let ((pos (my--org-deps-view--entry-line 1)))
    (if pos (goto-char pos) (message "No more entries"))))

(defun my--org-deps-view-prev ()
  "Move to the previous dependency entry."
  (interactive)
  (let ((pos (my--org-deps-view--entry-line -1)))
    (if pos (goto-char pos) (message "No previous entry"))))

(defun my--org-deps-view-toggle-done ()
  "Toggle whether completed tasks are hidden in the view."
  (interactive)
  (setq my--org-deps-view-hide-done (not my--org-deps-view-hide-done))
  (my--org-deps-view-revert)
  (message "%s completed tasks."
           (if my--org-deps-view-hide-done "Hiding" "Showing")))

(defun my--org-deps-view--window-size-change (frame)
  "Re-render a visible view whose window width changed.
The first render can happen before `zoom-mode' resizes the split, so the
rules would otherwise keep the pre-zoom width until the next refresh."
  (unless my--org-deps-view--refreshing
    (let ((my--org-deps-view--refreshing t))
      (dolist (win (window-list frame 'no-minibuffer))
        (with-current-buffer (window-buffer win)
          (when (and (derived-mode-p 'my-org-deps-view-mode)
                     my--org-deps-view-source
                     (not (equal my--org-deps-view-rendered-width
                                 (window-body-width win))))
            (ignore-errors
              (my--org-deps-view-render (current-buffer)
                                        my--org-deps-view-source))))))))

(add-hook 'window-size-change-functions #'my--org-deps-view--window-size-change)

(defun my--org-deps-view-quit ()
  "Close the view, restoring the layout and point from before it opened."
  (interactive)
  (let ((buffer (current-buffer))
        (config my--org-deps-view-return-config)
        (origin my--org-deps-view-return-point))
    (kill-buffer buffer)
    (if (and config (window-configuration-p config))
        (progn
          (set-window-configuration config)
          (when (and (markerp origin) (marker-buffer origin)
                     (buffer-live-p (marker-buffer origin)))
            (let ((win (get-buffer-window (marker-buffer origin) t)))
              (when win
                (select-window win)
                (goto-char origin)))))
      (quit-window))))

(defun my--org-deps-view-revert (&rest _)
  "Re-render the view from its stored source marker."
  (unless (and (markerp my--org-deps-view-source)
               (marker-buffer my--org-deps-view-source)
               (buffer-live-p (marker-buffer my--org-deps-view-source)))
    (user-error "Source task is no longer available"))
  (my--org-deps-view-render (current-buffer) my--org-deps-view-source))

(defvar my-org-deps-view-mode-map nil
  "Keymap for `my-org-deps-view-mode'.")

;; `setq', not `defvar': reloading the file must rebuild the keymap so
;; newly added keys actually appear in a running Emacs.
(setq my-org-deps-view-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map org-mode-map)
        (define-key map (kbd "RET") #'my--org-deps-view-ret)
        (define-key map [mouse-1] #'my--org-deps-view-mouse-jump)
        (define-key map (kbd "j") #'my--org-deps-view-next)
        (define-key map (kbd "k") #'my--org-deps-view-prev)
        (define-key map (kbd "D") #'my--org-deps-view-toggle-done)
        (define-key map (kbd "g") #'revert-buffer)
        (define-key map (kbd "q") #'my--org-deps-view-quit)
        map))

(define-derived-mode my-org-deps-view-mode org-mode "OrgDeps"
  "Major mode for the read-only dependency view.
\\{my-org-deps-view-mode-map}"
  (setq-local truncate-lines t)
  (setq-local revert-buffer-function #'my--org-deps-view-revert)
  (use-local-map my-org-deps-view-mode-map)
  (setq buffer-read-only t)
  ;; Evil's normal-state map would shadow n/p/D/RET; this buffer is a
  ;; read-only list, so give it plain Emacs keys.
  (when (fboundp 'evil-local-mode)
    (evil-local-mode -1)))

;;;###autoload
(defun my/org-deps-view ()
  "Show the dependencies of the task at point.
Inside the view buffer, shows the dependencies of the entry at point
or refreshes the current one."
  (interactive)
  (cond
   ((derived-mode-p 'my-org-deps-view-mode)
    (let ((marker (or (my--org-deps-view-marker-at-point)
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
