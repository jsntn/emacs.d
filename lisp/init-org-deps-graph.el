;;; init-org-deps-graph.el --- Visualize org task dependencies as a graph -*- lexical-binding: t -*-
;;; Commentary:
;; Render the task-dependency graph of the current Org buffer with Graphviz
;; and jump to any task from the keyboard (avy/ace-jump style letter hints).
;;
;; Two dependency styles are understood:
;;   - org-edna   : upper-case  :BLOCKER: ids(UUID ...)   (jumps via org-id)
;;   - TaskJuggler: lower-case   :blocker: task_id, ...   (jumps via :task_id:)
;; An edge FROM -> TO means "FROM blocks TO".
;;
;; `M-x my/org-deps-graph' in an Org buffer opens *Org Deps Graph* showing the
;; SVG plus a hint list; press a node's letter to jump to that heading, `q' to
;; quit, `g' to regenerate.
;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

(defgroup my-org-deps-graph nil
  "Graphviz visualization of Org task dependencies."
  :group 'org)

(defcustom my-org-deps-graph-dot
  "V:/bin/GraphvizPortable/App/graphviz/bin/dot.exe"
  "Path to the Graphviz `dot' executable."
  :type 'string
  :group 'my-org-deps-graph)

(defcustom my-org-deps-graph-hint-chars
  ;; Home-row first, but WITHOUT j/k (vim navigation), q (quit), g (refresh),
  ;; n/p (cursor nav), so hints never collide with buffer movement keys.
  "asdfhlweruiotyvmcxzb"
  "Characters used for node jump hints, in assignment order.
Must not include the buffer's navigation/command keys (j k n p g q)."
  :type 'string
  :group 'my-org-deps-graph)

(defcustom my-org-deps-graph-done-keywords '("DONE" "CANCELED" "CLOSED" "FIXED")
  "TODO keywords treated as completed.
Tasks in these states are omitted from the dependency graph, and edges
touching them are dropped (a finished blocker no longer constrains anything)."
  :type '(repeat string)
  :group 'my-org-deps-graph)

(defvar my--org-deps-graph-source nil
  "Source Org file the current graph was built from.")

(defvar-local my--org-deps-graph-svg nil
  "Path to the SVG file of the graph currently shown in this buffer.")

(defvar-local my--org-deps-graph-dotfile nil
  "Path to the Graphviz .dot source of the graph in this buffer.
Reused to export a PNG for external viewing.")

(defvar my--org-deps-graph-jumps nil
  "Alist (CHAR . TARGET) for the current graph.
TARGET is (:id ID) or (:task-id TASK-ID) or (:pos POS).")

(defun my--org-deps-collect (file)
  "Parse FILE without triggering find-file hooks.
Return a plist (:nodes HASH :edges LIST).
HASH maps node-key -> (plist :title :todo :id :task-id :pos).
Each edge is (FROM-KEY . TO-KEY) meaning FROM blocks TO."
  (let ((nodes (make-hash-table :test 'equal))
        (ref->key (make-hash-table :test 'equal))
        (done-refs (make-hash-table :test 'equal))
        (raw '())
        (edges '()))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-inhibit-startup t)
            (org-mode-hook nil)
            (org-agenda-files nil))
        (delay-mode-hooks (org-mode)))
      (org-with-wide-buffer
       (goto-char (point-min))
       (org-map-entries
        (lambda ()
          (let* ((el (org-element-at-point))
                 (title (org-element-property :raw-value el))
                 (todo (org-element-property :todo-keyword el))
                 (prio-char (org-element-property :priority el))
                 (prio (and prio-char (char-to-string prio-char)))
                 (pos (point))
                 (oid (org-entry-get pos "ID"))
                 (tid (org-entry-get pos "task_id"))
                 ;; Org property names are case-insensitive, so
                 ;; `org-entry-get' for "BLOCKER" and "blocker" return the
                 ;; SAME value. Fetch once and dispatch on the value's shape:
                 ;; org-edna uses ids(UUID ...); TaskJuggler uses bare
                 ;; task_id names separated by commas.
                 (blk (org-entry-get pos "BLOCKER"))
                 (key (or tid oid title))
                 (done (member todo my-org-deps-graph-done-keywords)))
            (if done
                ;; Completed task: omit from the graph entirely. Record all of
                ;; its refs so edges touching it (as blocker or blocked) are
                ;; dropped below -- a finished blocker no longer constrains.
                (progn
                  (puthash key t done-refs)
                  (when oid (puthash oid t done-refs))
                  (when tid (puthash tid t done-refs))
                  (when title (puthash title t done-refs)))
              (puthash key (list :title title :todo todo :priority prio
                                 :id oid :task-id tid :pos pos)
                       nodes)
              (when oid (puthash oid key ref->key))
              (when tid (puthash tid key ref->key))
              ;; `raw' edges are (THIS-KEY . BLOCKER-REF): the blocker blocks
              ;; THIS task. Completed tasks collect no blockers (skipped above).
              (when (and blk (not (string-empty-p (string-trim blk))))
                (if (string-match-p "ids(" blk)
                    ;; org-edna: one or more ids(uuid uuid ...)
                    (let ((start 0))
                      (while (string-match "ids(\\([^)]+\\))" blk start)
                        (dolist (uuid (split-string (match-string 1 blk) "[ \t]+" t))
                          (push (cons key uuid) raw))
                        (setq start (match-end 0))))
                  ;; TaskJuggler: id1, id2
                  (dolist (b (split-string blk "[ ,]+" t))
                    (push (cons key b) raw)))))))
        t nil))
      ;; Resolve raw (THIS-KEY . BLOCKER-REF) into directed edges
      ;; (FROM-KEY . TO-KEY) meaning FROM blocks TO: FROM is the blocker,
      ;; TO is THIS task. Drop any edge whose blocker is a completed task
      ;; (its ref lives in DONE-REFS). Unknown refs stay as dangling nodes.
      (dolist (r (nreverse raw))
        (let* ((to-key (car r))
               (blocker-ref (cdr r)))
          (unless (gethash blocker-ref done-refs)
            (let ((from-key (or (gethash blocker-ref ref->key) blocker-ref)))
              (push (cons from-key to-key) edges)))))
      (list :nodes nodes :edges (nreverse edges)))))

(defun my--org-deps-involved (edges)
  "Return a hash-set of node keys that participate in EDGES."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e edges)
      (puthash (car e) t h)
      (puthash (cdr e) t h))
    h))

(defun my--org-deps-assign-hints (keys)
  "Assign single-char hints to KEYS in order.
Return an alist (KEY . CHAR); unassigned keys (beyond the hint
alphabet) get nil."
  (let ((chars (append my-org-deps-graph-hint-chars nil))
        (result '()))
    (dolist (k keys (nreverse result))
      (push (cons k (pop chars)) result))))

(defun my--org-deps-color-hex (color)
  "Normalize COLOR (name or hex) to \"#rrggbb\", or nil if unresolvable."
  (when (and color (stringp color))
    (let ((vals (ignore-errors (color-values color))))
      (when vals
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (c) (/ c 256)) vals))))))

(defun my--org-deps-todo-color (todo fallback)
  "Return a hex color for TODO keyword, derived from the live theme.
Prefers the keyword's own face in `org-todo-keyword-faces' (its
:background, else :foreground); falls back to the generic `org-todo' /
`org-done' faces; finally to FALLBACK.  Fully dynamic -- no hardcoded
palette."
  (or
   ;; 1) user keyword face spec, e.g. (:foreground "white" :background "#2E8B57")
   (let* ((spec (cdr (assoc todo org-todo-keyword-faces)))
          ;; spec may be a plist, a string color, or a nested face list
          (plist (cond ((stringp spec) nil)
                       ((and (listp spec) (keywordp (car spec))) spec)
                       ((and (listp spec) (listp (car spec))
                             (keywordp (caar spec))) (car spec))
                       (t nil))))
     (cond
      ((stringp spec) (my--org-deps-color-hex spec))
      (plist (my--org-deps-color-hex
              (or (plist-get plist :background)
                  (plist-get plist :foreground))))))
   ;; 2) generic org faces by done/not-done (guard var that may be unbound
   ;;    outside an Org buffer)
   (my--org-deps-color-hex
    (face-attribute (if (and (boundp 'org-done-keywords-for-agenda)
                             (member todo org-done-keywords-for-agenda))
                        'org-done 'org-todo)
                    :foreground nil t))
   ;; 3) caller fallback (e.g. theme fg)
   fallback))

(defun my--org-deps-todo-colors (todo)
  "Return (BG . FG) hex colors for TODO keyword from the live theme.
Uses the keyword's `org-todo-keyword-faces' spec (:background/:foreground);
falls back to generic org-todo/org-done face fg on a neutral chip."
  (let* ((spec (cdr (assoc todo org-todo-keyword-faces)))
         (plist (cond ((and (listp spec) (keywordp (car spec))) spec)
                      ((and (listp spec) (listp (car spec))
                            (keywordp (caar spec))) (car spec))
                      (t nil)))
         (bg (and plist (my--org-deps-color-hex (plist-get plist :background))))
         (fg (and plist (my--org-deps-color-hex (plist-get plist :foreground)))))
    (if bg
        (cons bg (or fg "#ffffff"))
      ;; no explicit keyword bg: use the keyword's generic face fg as chip bg
      (let ((c (my--org-deps-todo-color todo "#888888")))
        (cons c "#ffffff")))))

(defun my--org-deps-html-escape (s)
  "Escape S for a graphviz HTML-like label."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "&" "&amp;" s))
    (setq s (replace-regexp-in-string "<" "&lt;" s))
    (setq s (replace-regexp-in-string ">" "&gt;" s))
    s))

(defun my--org-deps-node-label (hint title todo prio fg pt)
  "Build a graphviz HTML-like label (an org-style TODO row).
HINT char, TODO keyword as a colored chip (theme colors), optional PRIO
like #A, then the TITLE in theme FG.  PT is the title point size; the
hint/chip/priority use a slightly smaller size, matching org's look.
Returns a string in graphviz HTML-label syntax (begins `<', ends `>')."
  (let* ((small (max 6 (round (* pt 0.85))))
         (big (max 6 (round pt)))
         (htxt (if hint (format "<B>%c</B>" hint) " "))
         (cells
          (concat
           ;; hint cell
           (format "<TD><FONT COLOR=\"%s\" POINT-SIZE=\"%d\">%s</FONT></TD>" fg small htxt)
           ;; todo chip
           (when (and todo (not (string-empty-p todo)))
             (let* ((c (my--org-deps-todo-colors todo))
                    (bg (car c)) (tf (cdr c)))
               (format "<TD BGCOLOR=\"%s\"><FONT COLOR=\"%s\" POINT-SIZE=\"%d\"><B> %s </B></FONT></TD>"
                       bg tf small (my--org-deps-html-escape todo))))
           ;; priority
           (when (and prio (not (string-empty-p prio)))
             (format "<TD><FONT COLOR=\"%s\" POINT-SIZE=\"%d\"><B>[#%s]</B></FONT></TD>" fg small prio))
           ;; title
           (format "<TD><FONT COLOR=\"%s\" POINT-SIZE=\"%d\"> %s </FONT></TD>"
                   fg big (my--org-deps-html-escape title)))))
    (format "<<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLSPACING=\"2\" CELLPADDING=\"1\"><TR>%s</TR></TABLE>>"
            cells)))

(defun my--org-deps-dot (data hints fg pt)
  "Build dot source from DATA plist and HINTS alist (key . char).
Nodes are rendered as org-style TODO rows (HTML-like labels) with colored
status chips pulled live from the theme.  FG is the theme foreground color
and PT the default font point size; the canvas is transparent and the
graph flows top-to-bottom."
  (let* ((nodes (plist-get data :nodes))
         (edges (plist-get data :edges))
         (involved (my--org-deps-involved edges))
         (seen (make-hash-table :test 'equal))
         (epts (max 6 (round (* pt 0.8))))
         (out (concat "digraph deps {\n"
                      "  bgcolor=\"transparent\";\n"
                      "  rankdir=TB;\n"
                      "  nodesep=0.3;\n"
                      "  ranksep=0.5;\n"
                      (format "  node [shape=box style=\"rounded\" fontname=\"Helvetica\" color=\"%s\" penwidth=1];\n" fg)
                      (format "  edge [color=\"%s\" fontcolor=\"%s\" fontsize=%d arrowsize=0.7];\n" fg fg epts))))
    (cl-labels ((nid (k) (format "n%d" (abs (sxhash k)))))
      ;; involved nodes as org-style rows
      (maphash
       (lambda (k v)
         (when (gethash k involved)
           (let* ((title (plist-get v :title))
                  (todo (plist-get v :todo))
                  (prio (plist-get v :priority))
                  (label (my--org-deps-node-label
                          (cdr (assoc k hints)) title todo prio fg pt)))
             (setq out (concat out (format "  %s [label=%s];\n" (nid k) label)))
             (puthash k t seen))))
       nodes)
      ;; edges; declare any dangling endpoint as a bare row
      (dolist (e edges)
        (let ((f (car e)) (tk (cdr e)))
          (unless (gethash f seen)
            (setq out (concat out (format "  %s [label=%s];\n"
                                          (nid f)
                                          (my--org-deps-node-label
                                           (cdr (assoc f hints)) f nil nil fg pt))))
            (puthash f t seen))
          (unless (gethash tk seen)
            (setq out (concat out (format "  %s [label=%s];\n"
                                          (nid tk)
                                          (my--org-deps-node-label
                                           (cdr (assoc tk hints)) tk nil nil fg pt))))
            (puthash tk t seen))
          (setq out (concat out (format "  %s -> %s [label=\"blocks\"];\n"
                                        (nid f) (nid tk))))))
      (setq out (concat out "}\n")))
    out))

(defun my--org-deps-jump-target (key nodes)
  "Return a jump TARGET for KEY using NODES metadata, or nil.
TARGET is (:id ID) / (:task-id TASK-ID) / (:pos POS)."
  (let ((v (gethash key nodes)))
    (cond
     ((and v (plist-get v :id)) (list :id (plist-get v :id)))
     ((and v (plist-get v :task-id)) (list :task-id (plist-get v :task-id)))
     ((and v (plist-get v :pos)) (list :pos (plist-get v :pos)))
     ;; dangling ref: key is itself an id or task_id string
     ((and (stringp key) (org-id-find key)) (list :id key))
     ((stringp key) (list :task-id key)))))

(defun my--org-deps-do-jump (target)
  "Jump to TARGET (:id/:task-id/:pos) in `my--org-deps-graph-source'."
  (pcase target
    (`(:id ,id)
     (org-id-goto id))
    (`(:pos ,pos)
     (find-file my--org-deps-graph-source)
     (goto-char pos)
     (org-reveal))
    (`(:task-id ,tid)
     (find-file my--org-deps-graph-source)
     (goto-char (point-min))
     (if (re-search-forward
          (format "^[ \t]*:task_id:[ \t]+%s[ \t]*$" (regexp-quote tid)) nil t)
         (progn (org-back-to-heading t) (org-reveal))
       (user-error "No heading with task_id=%s" tid)))
    (_ (user-error "No jump target"))))

(defun my-org-deps-graph-jump-line ()
  "Jump to the task on the current list line (via its text property)."
  (interactive)
  (let ((target (get-text-property (line-beginning-position) 'org-deps-target)))
    (if target
        (my--org-deps-do-jump target)
      (message "No task on this line"))))

(defun my--org-deps-move-line (dir)
  "Move cursor to the next (DIR=1) or previous (DIR=-1) task list line."
  (let ((start (point)) found)
    (forward-line dir)
    (while (and (not (bobp)) (not (eobp))
                (not (get-text-property (line-beginning-position) 'org-deps-target)))
      (forward-line dir))
    (if (get-text-property (line-beginning-position) 'org-deps-target)
        (progn (beginning-of-line) (setq found t))
      (goto-char start))
    found))

(defun my-org-deps-graph-next ()
  "Move to the next task line."
  (interactive) (my--org-deps-move-line 1))

(defun my-org-deps-graph-prev ()
  "Move to the previous task line."
  (interactive) (my--org-deps-move-line -1))

(defvar my-org-deps-graph-mode-map (make-sparse-keymap)
  "Keymap for `my-org-deps-graph-mode'.
Bindings are (re)applied below at load time so reloading the file always
refreshes them (a `defvar' alone would not overwrite an existing keymap).
Node hint characters are bound dynamically per graph in `my--org-deps-show'.")

;; Apply bindings at top level (runs on every load, unlike the defvar value).
(let ((map my-org-deps-graph-mode-map))
  (define-key map (kbd "q")   #'quit-window)
  (define-key map (kbd "g")   #'my/org-deps-graph-refresh)
  (define-key map (kbd "RET") #'my-org-deps-graph-jump-line)
  (define-key map (kbd "O")   #'my/org-deps-graph-open-external)
  ;; cursor navigation between task lines (vim + emacs)
  (define-key map (kbd "j")   #'my-org-deps-graph-next)
  (define-key map (kbd "k")   #'my-org-deps-graph-prev)
  (define-key map (kbd "n")   #'my-org-deps-graph-next)
  (define-key map (kbd "p")   #'my-org-deps-graph-prev)
  (define-key map (kbd "C-n") #'my-org-deps-graph-next)
  (define-key map (kbd "C-p") #'my-org-deps-graph-prev))

(define-derived-mode my-org-deps-graph-mode special-mode "OrgDepsGraph"
  "Major mode for the Org task-dependency graph buffer."
  (setq buffer-read-only t
        truncate-lines nil)
  ;; In evil, a special-mode buffer defaults to motion/normal state, whose
  ;; keymap swallows the letter hint keys and j/k. Switch THIS buffer to evil
  ;; emacs-state so our keymap wins. Done in the mode body (reliable, buffer
  ;; local) rather than only via the initial-state registry.
  (when (fboundp 'evil-emacs-state)
    (evil-emacs-state)))

;; Belt-and-suspenders: also register the initial state so a buffer that
;; somehow (re)enters a vim state still comes up in emacs-state. evil is
;; already loaded here, so call it directly.
(when (fboundp 'evil-set-initial-state)
  (evil-set-initial-state 'my-org-deps-graph-mode 'emacs))

(defun my--org-deps-theme-fg ()
  "Return the current theme's default foreground as a hex string.
Falls back to a light gray if the color cannot be resolved."
  (let ((fg (face-attribute 'default :foreground nil t)))
    (if (and fg (not (eq fg 'unspecified)))
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (c) (/ c 256)) (color-values fg)))
      "#cccccc")))

(defun my--org-deps-theme-bg ()
  "Return the current theme's default background as a hex string.
Falls back to white if the color cannot be resolved."
  (let ((bg (face-attribute 'default :background nil t)))
    (if (and bg (not (eq bg 'unspecified)))
        (apply #'format "#%02x%02x%02x"
               (mapcar (lambda (c) (/ c 256)) (color-values bg)))
      "#ffffff")))

(defun my--org-deps-font-pt ()
  "Return the current default font size in points, following the live theme.
`face-attribute :height' is in 1/10 pt, so divide by 10."
  (let ((h (face-attribute 'default :height nil t)))
    (if (integerp h) (/ h 10.0) 10.0)))

(defun my--org-deps-render (file)
  "Parse FILE, render SVG via dot, return a plist with :svg path.
The SVG uses the theme foreground color, a transparent canvas, and the
current default font size (so it matches and tracks the buffer text);
it is vector so it stays crisp at any scale."
  (unless (file-exists-p my-org-deps-graph-dot)
    (user-error "Graphviz dot not found: %s" my-org-deps-graph-dot))
  (let* ((data (my--org-deps-collect file))
         (edges (plist-get data :edges)))
    (when (null edges)
      (user-error "No task dependencies (BLOCKER/blocker) found in %s"
                  (file-name-nondirectory file)))
    (let* ((involved (my--org-deps-involved edges))
           (keys (let (ks) (maphash (lambda (k _) (push k ks)) involved)
                      (nreverse ks)))
           (hints (my--org-deps-assign-hints keys))
           (fg (my--org-deps-theme-fg))
           (pt (my--org-deps-font-pt))
           (dot (my--org-deps-dot data hints fg pt))
           (base (make-temp-name
                  (expand-file-name "org-deps-" temporary-file-directory)))
           (dotfile (concat base ".dot"))
           (svg (concat base ".svg")))
      (with-temp-file dotfile (insert dot))
      (let ((rc (call-process my-org-deps-graph-dot nil "*org-deps-dot*" nil
                              "-Tsvg" dotfile "-o" svg)))
        (unless (and (eq rc 0) (file-exists-p svg))
          (user-error "dot failed (rc=%s); see *org-deps-dot*" rc)))
      (list :data data :svg svg :dot dotfile :hints hints :keys keys))))

(defun my--org-deps-todo-face (todo)
  "Return a face spec for TODO keyword from the live theme, or nil."
  (let ((spec (cdr (assoc todo org-todo-keyword-faces))))
    (cond ((null spec) nil)
          ((stringp spec) (list :foreground spec))
          ((and (listp spec) (keywordp (car spec))) spec)
          ((and (listp spec) (listp (car spec)) (keywordp (caar spec))) (car spec))
          (t nil))))

(defun my--org-deps-show (result)
  "Populate *Org Deps Graph* from RESULT and set up jump keys."
  (let* ((data (plist-get result :data))
         (nodes (plist-get data :nodes))
         (svg (plist-get result :svg))
         (dotfile (plist-get result :dot))
         (hints (plist-get result :hints))
         (keys (plist-get result :keys))
         (edges (plist-get data :edges))
         ;; key -> list of keys it blocks / is blocked by, for the per-row
         ;; relation suffix (shown using the same hint letters as the graph)
         (blocks (make-hash-table :test 'equal))
         (blocked-by (make-hash-table :test 'equal))
         (buf (get-buffer-create "*Org Deps Graph*")))
    ;; build both relation directions from the edges (FROM blocks TO)
    (dolist (e edges)
      (let ((f (car e)) (tk (cdr e)))
        (push tk (gethash f blocks))
        (push f (gethash tk blocked-by))))
    ;; Show the buffer alone in the frame so the graph gets the full frame
    ;; width (not a split half), then measure that window: the SVG width below
    ;; needs the real text width of the window the graph ends up in.
    (pop-to-buffer buf)
    (delete-other-windows (get-buffer-window buf t))
    (with-current-buffer buf
      (my-org-deps-graph-mode)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (format "Task dependencies — %s\n"
                                    (file-name-nondirectory
                                     my--org-deps-graph-source))
                            'face 'bold))
        (insert (propertize
                 "Hint: jump · j/k/n/p: move · RET: jump · O: open externally · g: refresh · q: quit\n\n"
                 'face 'shadow))
        ;; Vector graph (transparent canvas blends with buffer bg).
        ;; Scale the graph to exactly fill the window's text width: narrow
        ;; graphs grow to be legible, wide ones shrink to stay on-screen, so
        ;; browsing is always vertical.  `:scale 1.0' is pinned so `:width'
        ;; is the literal target pixel width — otherwise Emacs' HiDPI
        ;; auto-scale multiplies it and the graph overshoots.  SVG is vector,
        ;; so both growing and shrinking stay crisp.
        (insert-image
         (create-image svg 'svg nil
                       :width (window-body-width (get-buffer-window buf t) t)
                       :scale 1.0))
        (insert "\n\n")
        ;; org-style hint list; each line carries its jump target as a text
        ;; property so cursor navigation + RET work, and letters jump directly.
        ;; remember the generated files so `O' can export/open them externally
        (setq-local my--org-deps-graph-svg svg)
        (setq-local my--org-deps-graph-dotfile dotfile)
        (setq-local my--org-deps-graph-jumps nil)
        (dolist (k keys)
          (let* ((ch (cdr (assoc k hints)))
                 (v (gethash k nodes))
                 (title (or (and v (plist-get v :title)) k))
                 (todo (and v (plist-get v :todo)))
                 (prio (and v (plist-get v :priority)))
                 (target (my--org-deps-jump-target k nodes)))
            (when (and ch target)
              (push (cons ch target) my--org-deps-graph-jumps)
              (local-set-key (vector ch) #'my-org-deps-graph-jump-self)
              (let* ((hintstr (propertize (format "[%c]" ch) 'face 'bold))
                     (todostr (when (and todo (not (string-empty-p todo)))
                                (propertize todo 'face
                                            (or (my--org-deps-todo-face todo) 'org-todo))))
                     (priostr (when (and prio (not (string-empty-p prio)))
                                (propertize (format "[#%s]" prio) 'face 'org-priority)))
                     ;; relation suffix: which tasks this one blocks / is
                     ;; blocked by, by hint letter (matches the graph)
                     (labelfn (lambda (ks)
                                (mapconcat
                                 (lambda (x)
                                   (let ((c (cdr (assoc x hints))))
                                     (if c (char-to-string c) (format "%s" x))))
                                 (sort (copy-sequence ks) #'string<) ", ")))
                     (bl (gethash k blocks))
                     (bb (gethash k blocked-by))
                     (relstr (concat
                              (when bl (propertize
                                        (format "  → blocks: %s" (funcall labelfn bl))
                                        'face 'shadow))
                              (when bb (propertize
                                        (format "  ← blocked-by: %s" (funcall labelfn bb))
                                        'face 'shadow))))
                     (line (concat "  " hintstr " "
                                   (when todostr (concat todostr " "))
                                   (when priostr (concat priostr " "))
                                   title relstr "\n")))
                (insert (propertize line 'org-deps-target target)))))))
      (goto-char (point-min))
      (my--org-deps-move-line 1)))) ; land on first task line

(defun my-org-deps-graph-jump-self ()
  "Jump using the hint char that invoked this command."
  (interactive)
  (let* ((ch last-command-event)
         (target (cdr (assq ch my--org-deps-graph-jumps))))
    (if target
        (my--org-deps-do-jump target)
      (message "No node for hint `%c'" ch))))

(defun my/org-deps-graph-open-external ()
  "Export the current graph as a PNG and open it externally.
The in-buffer SVG has a transparent canvas, which many OS image viewers
render as black or blank; so a PNG whose background matches the current
Emacs theme background is produced via `dot -Gbgcolor=...' from the saved
.dot source and opened with the system default image viewer (Windows shell,
else `browse-url'), so the exported image looks like the in-buffer graph."
  (interactive)
  (unless (and my--org-deps-graph-dotfile
               (file-exists-p my--org-deps-graph-dotfile))
    (user-error "No graph .dot source for this buffer; regenerate with `g'"))
  (let* ((png (concat (file-name-sans-extension my--org-deps-graph-dotfile)
                      ".png"))
         (rc (call-process my-org-deps-graph-dot nil "*org-deps-dot*" nil
                           "-Tpng" (format "-Gbgcolor=%s" (my--org-deps-theme-bg))
                           my--org-deps-graph-dotfile "-o" png)))
    (unless (and (eq rc 0) (file-exists-p png))
      (user-error "dot PNG export failed (rc=%s); see *org-deps-dot*" rc))
    (cond
     ((fboundp 'w32-shell-execute)
      (w32-shell-execute "open" (expand-file-name png)))
     ((eq system-type 'darwin)
      (call-process "open" nil 0 nil png))
     (t (browse-url (concat "file://" (expand-file-name png)))))
    (message "Opened %s" (file-name-nondirectory png))))

;;;###autoload
(defun my/org-deps-graph ()
  "Render the task-dependency graph of the current Org buffer.
Opens *Org Deps Graph*; press a node's hint letter to jump to it."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (setq my--org-deps-graph-source file)
    (my--org-deps-show (my--org-deps-render file))))

(defun my/org-deps-graph-refresh ()
  "Regenerate the graph from `my--org-deps-graph-source'."
  (interactive)
  (unless my--org-deps-graph-source
    (user-error "No graph source"))
  (my--org-deps-show (my--org-deps-render my--org-deps-graph-source)))

(provide 'init-org-deps-graph)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org-deps-graph.el ends here
