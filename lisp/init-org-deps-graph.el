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
;;
;; `C-u M-x my/org-deps-graph' restricts the graph to the subtree at point:
;; a leaf task shows the tasks it blocks and is blocked by, a parent headline
;; shows the graph of the tasks inside it.  Dependencies that cross the
;; subtree boundary are kept, with the outside endpoint drawn dashed.
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

(defvar my--org-deps-graph-scope nil
  "Scope plist of the graph shown in this buffer, or nil for the whole file.
See `my--org-deps-scope-at-point' for the shape; `g' reuses it so a
regenerated graph keeps the same subtree view.")

(defvar-local my--org-deps-graph-svg nil
  "Path to the SVG file of the graph currently shown in this buffer.")

(defvar-local my--org-deps-graph-dotfile nil
  "Path to the Graphviz .dot source of the graph in this buffer.
Reused to export a PNG for external viewing.")

(defvar my--org-deps-graph-jumps nil
  "Alist (CHAR . TARGET) for the current graph.
TARGET is (:id ID) or (:task-id TASK-ID) or (:pos POS).")

(defvar-local my--org-deps-graph-image-marker nil
  "Marker on the display character that shows the graph image.")

(defvar-local my--org-deps-graph-natural nil
  "Cons (WIDTH . HEIGHT), in pixels, of the chosen SVG at render time.")

(defvar-local my--org-deps-graph-font-height nil
  "Window font height in pixels when the graph was rendered.
The image is scaled by the current height over this one, so it tracks
font-size changes (`cnfonts-increase-fontsize') and text scaling.")

(defun my--org-deps-key (oid tid title)
  "Return the graph key for an entry with OID, TID and TITLE.
TID (:task_id:) wins over OID (:ID:) which wins over TITLE, so an entry
gets the same key whether it is read from a file or from the live buffer."
  (or tid oid title))

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
                 (key (my--org-deps-key oid tid title))
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

(defun my--org-deps-scope-at-point ()
  "Return the subtree scope for the headline at point, or nil.
The plist is (:title TITLE :level LEVEL :leaf LEAF :keys KEYS), where
KEYS are the graph keys (see `my--org-deps-key') of the headline and all
its descendants, and LEAF is non-nil when it has no child headings.
Returns nil when point is not inside a headline."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (org-back-to-heading-or-point-min)
      (when (org-at-heading-p)
        (let* ((title (org-get-heading t t t t))
               (level (org-current-level))
               (leaf (save-excursion (not (org-goto-first-child))))
               (keys '()))
          (org-map-entries
           (lambda ()
             (let ((oid (org-entry-get (point) "ID"))
                   (tid (org-entry-get (point) "task_id"))
                   (ttl (org-element-property :raw-value (org-element-at-point))))
               (push (my--org-deps-key oid tid ttl) keys)))
           nil 'tree)
          (list :title title :level level :leaf leaf
                :keys (delete-dups (delq nil keys))))))))

(defun my--org-deps-filter (data scope)
  "Restrict DATA to edges that touch SCOPE.
An edge is kept when at least one endpoint is inside the scope; the
endpoints outside it are collected in the returned :outside hash-set so
they can be drawn as dashed \"outside\" nodes.  The :nodes table is kept
intact because labels and jump targets are looked up there."
  (let ((keyset (make-hash-table :test 'equal))
        (outside (make-hash-table :test 'equal))
        (edges '()))
    (dolist (k (plist-get scope :keys))
      (puthash k t keyset))
    (dolist (e (plist-get data :edges))
      (let ((from (car e)) (to (cdr e)))
        (when (or (gethash from keyset) (gethash to keyset))
          (push e edges)
          (unless (gethash from keyset) (puthash from t outside))
          (unless (gethash to keyset) (puthash to t outside)))))
    (list :nodes (plist-get data :nodes)
          :edges (nreverse edges)
          :outside outside)))

(defun my--org-deps-involved (edges)
  "Return a hash-set of node keys that participate in EDGES."
  (let ((h (make-hash-table :test 'equal)))
    (dolist (e edges)
      (puthash (car e) t h)
      (puthash (cdr e) t h))
    h))

(defun my--org-deps-order-keys (nodes edges)
  "Return involved keys grouped by component, then by dependency rank.
Components are ordered by the earliest source position of their members.
Inside a component keys are ordered by rank (longest blocker chain), then by
source position, so `j'/`k' walk chains in dependency order instead of
hash-table order."
  (let ((keys '())
        (blockers (make-hash-table :test 'equal))
        (rank (make-hash-table :test 'equal))
        (parent (make-hash-table :test 'equal))
        (pos (lambda (k)
               (or (plist-get (gethash k nodes) :pos) most-positive-fixnum))))
    (dolist (e edges)
      (let ((f (car e)) (tk (cdr e)))
        (cl-pushnew f keys :test #'equal)
        (cl-pushnew tk keys :test #'equal)
        (push f (gethash tk blockers))
        (puthash f f parent)
        (puthash tk tk parent)))
    (cl-labels ((uf-find (x)
                  (let ((p (gethash x parent)))
                    (if (or (null p) (equal p x)) x
                      (puthash x (uf-find p) parent))))
                (uf-union (a b)
                  (let ((ra (uf-find a)) (rb (uf-find b)))
                    (unless (equal ra rb) (puthash ra rb parent)))))
      (dolist (e edges) (uf-union (car e) (cdr e)))
      ;; longest path from a root: rank(k) = 1 + max rank(blockers of k).
      ;; Bounded by the node count so a cyclic BLOCKER chain cannot spin.
      (let ((changed t) (rounds 0))
        (while (and changed (<= rounds (length keys)))
          (setq changed nil rounds (1+ rounds))
          (dolist (k keys)
            (let ((bs (gethash k blockers)))
              (when bs
                (let ((r (1+ (apply #'max
                                    (mapcar (lambda (b) (or (gethash b rank) 0))
                                            bs)))))
                  (unless (eq r (gethash k rank))
                    (puthash k r rank)
                    (setq changed t))))))))
      ;; per-component sort anchor: (min position . min key)
      (let ((comps (make-hash-table :test 'equal)))
        (dolist (k keys)
          (let* ((c (uf-find k))
                 (p (funcall pos k))
                 (cur (gethash c comps)))
            (cond ((null cur) (puthash c (cons p (format "%s" k)) comps))
                  ((or (< p (car cur))
                       (and (= p (car cur))
                            (string-lessp (format "%s" k) (cdr cur))))
                   (puthash c (cons p (format "%s" k)) comps)))))
        (sort keys
              (lambda (a b)
                (let* ((ca (uf-find a)) (cb (uf-find b))
                       (pa (car (gethash ca comps)))
                       (pb (car (gethash cb comps)))
                       (ra (or (gethash a rank) 0))
                       (rb (or (gethash b rank) 0))
                       (xa (funcall pos a))
                       (xb (funcall pos b)))
                  (cond ((/= pa pb) (< pa pb))
                        ((not (equal ca cb))
                         (string-lessp (cdr (gethash ca comps))
                                       (cdr (gethash cb comps))))
                        ((/= ra rb) (< ra rb))
                        ((/= xa xb) (< xa xb))
                        (t (string-lessp (format "%s" a) (format "%s" b)))))))))))

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

(defun my--org-deps-dot (data hints fg pt rankdir nodesep ranksep)
  "Build dot source from DATA plist and HINTS alist (key . char).
Nodes are rendered as org-style TODO rows (HTML-like labels) with colored
status chips pulled live from the theme.  FG is the theme foreground color,
PT the default font point size and RANKDIR either \"TB\" or \"LR\".
NODESEP and RANKSEP are Graphviz spacing in inches; the canvas is transparent
and disconnected components are packed independently."
  (let* ((nodes (plist-get data :nodes))
         (edges (plist-get data :edges))
         (outside (plist-get data :outside))
         (involved (my--org-deps-involved edges))
         (seen (make-hash-table :test 'equal))
         (epts (max 6 (round (* pt 0.8))))
         (out (concat "digraph deps {\n"
                      "  bgcolor=\"transparent\";\n"
                      "  pack=true;\n"
                      (format "  rankdir=%s;\n" rankdir)
                      (format "  nodesep=%s;\n" nodesep)
                      (format "  ranksep=%s;\n" ranksep)
                      (format "  node [shape=box style=\"rounded\" fontname=\"Helvetica\" color=\"%s\" penwidth=1];\n" fg)
                      (format "  edge [color=\"%s\" fontcolor=\"%s\" fontsize=%d arrowsize=0.7];\n" fg fg epts))))
    (cl-labels ((nid (k) (format "n%d" (abs (sxhash k))))
                (node-style (k)
                            ;; nodes outside the C-u subtree are dashed
                            (if (and outside (gethash k outside))
                                "rounded,dashed"
                              "rounded")))
      ;; involved nodes as org-style rows
      (maphash
       (lambda (k v)
         (when (gethash k involved)
           (let* ((title (plist-get v :title))
                  (todo (plist-get v :todo))
                  (prio (plist-get v :priority))
                  (label (my--org-deps-node-label
                          (cdr (assoc k hints)) title todo prio fg pt)))
             (setq out (concat out (format "  %s [label=%s style=\"%s\"];\n"
                                           (nid k) label (node-style k))))
             (puthash k t seen))))
       nodes)
      ;; edges; declare any dangling endpoint as a bare row
      (dolist (e edges)
        (let ((f (car e)) (tk (cdr e)))
          (unless (gethash f seen)
            (setq out (concat out (format "  %s [label=%s style=\"%s\"];\n"
                                          (nid f)
                                          (my--org-deps-node-label
                                           (cdr (assoc f hints)) f nil nil fg pt)
                                          (node-style f))))
            (puthash f t seen))
          (unless (gethash tk seen)
            (setq out (concat out (format "  %s [label=%s style=\"%s\"];\n"
                                          (nid tk)
                                          (my--org-deps-node-label
                                           (cdr (assoc tk hints)) tk nil nil fg pt)
                                          (node-style tk))))
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

(defun my--org-deps-render (file &optional scope)
  "Parse FILE, render SVG via dot, return a plist with :candidates.
With SCOPE (a plist from `my--org-deps-scope-at-point') the graph is
restricted to edges touching that subtree; outside endpoints are drawn
dashed.  The candidates are the same graph laid out top-to-bottom (\"TB\")
and left-to-right (\"LR\"); `my--org-deps-show' picks whichever needs the
least downscaling in the actual window.  The SVG uses the theme foreground
color, a transparent canvas, and the current default font size (so it
matches and tracks the buffer text); it is vector so it stays crisp at any
scale."
  (unless (file-exists-p my-org-deps-graph-dot)
    (user-error "Graphviz dot not found: %s" my-org-deps-graph-dot))
  (let* ((data (my--org-deps-collect file))
         (data (if scope (my--org-deps-filter data scope) data))
         (edges (plist-get data :edges)))
    (when (null edges)
      (if scope
          (user-error "No task dependencies within %s"
                      (or (plist-get scope :title)
                          (file-name-nondirectory file)))
        (user-error "No task dependencies (BLOCKER/blocker) found in %s"
                    (file-name-nondirectory file))))
    (let* ((keys (my--org-deps-order-keys (plist-get data :nodes) edges))
           (hints (my--org-deps-assign-hints keys))
           (fg (my--org-deps-theme-fg))
           (pt (my--org-deps-font-pt))
           (cands '()))
      ;; Two orientations x two spacings.  Graphviz layout heuristics are not
      ;; monotonic in the spacing, so the sizes are measured rather than
      ;; guessed; the roomier preset comes first so it wins a tie.
      (dolist (spec '(("TB" 0.3 0.5) ("LR" 0.3 0.5)
                      ("TB" 0.1 0.35) ("LR" 0.1 0.35)))
        (let* ((rankdir (nth 0 spec))
               (nodesep (nth 1 spec))
               (ranksep (nth 2 spec))
               (base (make-temp-name
                      (expand-file-name "org-deps-" temporary-file-directory)))
               (dotfile (concat base ".dot"))
               (svg (concat base ".svg")))
          (with-temp-file dotfile
            (insert (my--org-deps-dot data hints fg pt rankdir nodesep ranksep)))
          (let ((rc (call-process my-org-deps-graph-dot nil "*org-deps-dot*" nil
                                  "-Tsvg" dotfile "-o" svg)))
            (unless (and (eq rc 0) (file-exists-p svg))
              (user-error "dot failed (rc=%s); see *org-deps-dot*" rc)))
          ;; `:scale 1.0' pins HiDPI auto-scaling so the measured size is the
          ;; SVG's own pixel size, comparable with `window-body-*' pixels.
          (let ((size (image-size (create-image svg 'svg nil :scale 1.0) t)))
            (push (list :rankdir rankdir :nodesep nodesep :ranksep ranksep
                        :svg svg :dot dotfile
                        :width (car size) :height (cdr size))
                  cands))))
      (list :data data :candidates (nreverse cands)
            :hints hints :keys keys :scope scope))))

(defun my--org-deps-choose-candidate (cands maxw maxh)
  "Return the candidate that best fits MAXW x MAXH pixels.
Each candidate is scaled down only if it exceeds either dimension; the one
needing the least downscaling wins, so a wide fan-out flips to the LR
layout while a deep chain stays TB.  Ties keep the earlier TB candidate."
  (let ((best nil) (best-scale -1.0))
    (dolist (c cands)
      (let* ((w (max 1.0 (float (plist-get c :width))))
             (h (max 1.0 (float (plist-get c :height))))
             (s (min 1.0 (/ (float maxw) w) (/ (float maxh) h))))
        (when (> s best-scale)
          (setq best c best-scale s))))
    (or best (car cands))))

(defun my--org-deps-todo-face (todo)
  "Return a face spec for TODO keyword from the live theme, or nil."
  (let ((spec (cdr (assoc todo org-todo-keyword-faces))))
    (cond ((null spec) nil)
          ((stringp spec) (list :foreground spec))
          ((and (listp spec) (keywordp (car spec))) spec)
          ((and (listp spec) (listp (car spec)) (keywordp (caar spec))) (car spec))
          (t nil))))

(defun my--org-deps-graph-fit-image ()
  "(Re)display the current SVG, scaled with the frame's font size.
The image keeps the proportions it had when rendered and is capped by the
window, so increasing the font grows the graph in step with the text."
  (when (and my--org-deps-graph-svg
             my--org-deps-graph-natural
             my--org-deps-graph-font-height
             my--org-deps-graph-image-marker)
    (let ((win (get-buffer-window (current-buffer) t)))
      (when (and win (marker-position my--org-deps-graph-image-marker))
        (let* ((charh (window-font-height win))
               (factor (max 0.1 (/ (float charh)
                                   (max 1 my--org-deps-graph-font-height))))
               (w (round (* (car my--org-deps-graph-natural) factor)))
               (h (round (* (cdr my--org-deps-graph-natural) factor)))
               (maxw (min w (window-body-width win t)))
               ;; leave room for the title, the hint line and the first row
               (maxh (min h (max 120 (- (window-body-height win t)
                                        (* 4 charh)))))
               (pos (marker-position my--org-deps-graph-image-marker)))
          (with-silent-modifications
            (put-text-property
             pos (1+ pos) 'display
             (create-image my--org-deps-graph-svg 'svg nil
                           :max-width maxw :max-height maxh :scale 1.0))))))))

(defun my--org-deps-graph-refit-windows (&rest _)
  "Re-fit graph images in every window after a font or frame change.
Only buffers that are actually displayed are touched, so a hidden graph
is not regenerated behind the user's back."
  (dolist (frame (frame-list))
    (dolist (win (window-list frame t))
      (with-current-buffer (window-buffer win)
        (when (derived-mode-p 'my-org-deps-graph-mode)
          (my--org-deps-graph-fit-image))))))

(defun my--org-deps-graph-refit-soon (&rest _)
  "Re-fit graph images after the pending font/frame resize is applied.
Deferred so the implied frame resize that follows a font change is already
in effect when the window's pixel size is measured."
  (run-at-time 0 nil #'my--org-deps-graph-refit-windows))

(defun my--org-deps-graph-refit-on-frame-change (&rest _)
  "Re-fit graph images after a frame or window-size change."
  (my--org-deps-graph-refit-soon))

;; `cnfonts-increase/decrease-fontsize' resize the default face; the finish
;; hook runs after the new font is installed.  Frame-size changes are handled
;; too, because a font step usually resizes the frame around the same text.
(add-hook 'cnfonts-set-font-finish-hook #'my--org-deps-graph-refit-soon)
(add-hook 'window-size-change-functions
          #'my--org-deps-graph-refit-on-frame-change)
(add-hook 'text-scale-mode-hook
          #'my--org-deps-graph-refit-on-frame-change)

(defun my--org-deps-show (result)
  "Populate *Org Deps Graph* from RESULT and set up jump keys."
  (let* ((data (plist-get result :data))
         (nodes (plist-get data :nodes))
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
    ;; width (not a split half), then measure that window: the SVG fit below
    ;; needs the real pixel size of the window the graph ends up in.
    (pop-to-buffer buf)
    (delete-other-windows (get-buffer-window buf t))
    (with-current-buffer buf
      (my-org-deps-graph-mode)
      (let* ((win (get-buffer-window buf t))
             (maxw (window-body-width win t))
             ;; leave room for the title, the hint line and the first list row
             (maxh (max 120 (- (window-body-height win t)
                               (* 4 (frame-char-height)))))
             (choice (my--org-deps-choose-candidate
                      (plist-get result :candidates) maxw maxh))
             (svg (plist-get choice :svg))
             (dotfile (plist-get choice :dot))
             (inhibit-read-only t))
        (erase-buffer)
        (let* ((scope (plist-get result :scope))
               (in-scope (and scope
                              (cl-count-if (lambda (k) (gethash k nodes))
                                           (plist-get scope :keys))))
               (n-outside (and scope
                               (let ((n 0))
                                 (maphash (lambda (_k _v) (setq n (1+ n)))
                                          (plist-get data :outside))
                                 n)))
               (source (file-name-nondirectory my--org-deps-graph-source))
               (where (if scope
                          (format "%s › %s" source (plist-get scope :title))
                        source))
               (detail (if scope
                           (format "  (%s: %d task%s, %d outside)"
                                   (if (plist-get scope :leaf) "task" "subtree")
                                   in-scope (if (= in-scope 1) "" "s")
                                   n-outside)
                         "")))
          (insert (propertize (format "Task dependencies — %s%s\n" where detail)
                              'face 'bold))
          (insert (propertize
                   (concat "Hint: jump · j/k/n/p: move · RET: jump · O: open externally · g: refresh · q: quit"
                           (when scope " · dashed = outside the subtree")
                           "\n\n")
                   'face 'shadow)))
        ;; Vector graph (transparent canvas blends with buffer bg).  The
        ;; orientation was chosen against this window: TB (vertical chains)
        ;; unless the graph is a wide fan-out, where LR keeps the text at full
        ;; size.  The image itself is placed by `my--org-deps-graph-fit-image',
        ;; which also re-scales it when the font size changes; `:max-width' /
        ;; `:max-height' cap it to the window and never enlarge past it.
        (setq-local my--org-deps-graph-svg svg)
        (setq-local my--org-deps-graph-scope (plist-get result :scope))
        (setq-local my--org-deps-graph-natural
                    (cons (plist-get choice :width) (plist-get choice :height)))
        (setq-local my--org-deps-graph-font-height (window-font-height win))
        (let ((start (point)))
          (insert " ")
          (setq-local my--org-deps-graph-image-marker (copy-marker start))
          (my--org-deps-graph-fit-image))
        (insert "\n\n")
        ;; the losing candidates are no longer needed; only the winner is kept
        ;; so `O' and later refreshes do not accumulate temp SVGs/.dot files
        (dolist (c (plist-get result :candidates))
          (unless (eq c choice)
            (ignore-errors (delete-file (plist-get c :svg)))
            (ignore-errors (delete-file (plist-get c :dot)))))
        ;; org-style hint list; each line carries its jump target as a text
        ;; property so cursor navigation + RET work, and letters jump directly.
        ;; remember the generated files so `O' can export/open them externally
        (setq-local my--org-deps-graph-dotfile dotfile)
        (setq-local my--org-deps-graph-jumps nil)
        ;; Bind the per-graph hint letters on a buffer-private COPY of the mode
        ;; map.  `special-mode' hands every graph buffer the *same* shared
        ;; `my-org-deps-graph-mode-map' as its local map, so `local-set-key'
        ;; here would permanently overwrite keys (e.g. a hint `j' clobbering
        ;; j→next) in that shared map for all buffers.  A fresh copy isolates
        ;; the hints and keeps j/k/n/p/g/q/RET/O intact.
        (use-local-map (copy-keymap my-org-deps-graph-mode-map))
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
(defun my/org-deps-graph (&optional arg)
  "Render the task-dependency graph of the current Org buffer.
Opens *Org Deps Graph*; press a node's hint letter to jump to it.

With universal argument ARG (`C-u'), restrict the graph to the subtree at
point: a leaf task shows the tasks it blocks and is blocked by, while a
parent headline shows the graph of the tasks inside it.  Dependencies that
cross the subtree boundary are kept, with the outside endpoint dashed."
  (interactive "P")
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an Org buffer"))
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer is not visiting a file"))
    (let ((scope (when arg (my--org-deps-scope-at-point))))
      (when (and arg (null scope))
        (message "No headline at point; showing the whole file"))
      (when (and scope (buffer-modified-p))
        (message "Buffer has unsaved changes; the graph reflects the file on disk"))
      (setq my--org-deps-graph-source file)
      (my--org-deps-show (my--org-deps-render file scope)))))

(defun my/org-deps-graph-refresh ()
  "Regenerate the graph from `my--org-deps-graph-source'.
Keeps the subtree scope of the current graph, if it has one."
  (interactive)
  (unless my--org-deps-graph-source
    (user-error "No graph source"))
  (my--org-deps-show (my--org-deps-render my--org-deps-graph-source
                                          my--org-deps-graph-scope)))

(provide 'init-org-deps-graph)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-org-deps-graph.el ends here
