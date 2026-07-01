;;; init-image-viewer.el --- image viewer and image-mode settings -*- lexical-binding: t -*-
;;; Commentary:
;; Custom image viewer with navigation, annotations, slideshow, and fit-to-frame.
;; Works in GUI Emacs (requires display-graphic-p for actual image rendering).
;;; Code:


;; {{ START: image-mode general settings

(defun my/kill-image-buffers ()
  "Kill all buffers visiting image files."
  (interactive)
  (dolist (buf (buffer-list))
    (when (with-current-buffer buf (derived-mode-p 'image-mode))
      (let ((undo-tree-auto-save-history nil))
        (kill-buffer buf)))))

(defun my/image-fit-to-frame ()
  "Resize image in current buffer to fit the window without upscaling."
  (interactive)
  (when (eq major-mode 'image-mode)
    ;; Reset any previous scaling before recalculating
    (image-transform-set-scale 1.0)
    (let* ((img (image--get-image))
	   ;; Image size in pixels
	   (img-size (image-size img t))
	   (img-width (car img-size))
	   (img-height (cdr img-size))
	   ;; Window size in pixels
	   (win-width (window-pixel-width))
	   (win-height (window-pixel-height))
	   ;; Padding for mode-line, fringes, etc.
	   (usable-width (- win-width (or (car (window-fringes)) 0)))
	   (usable-height (- win-height 20)) ; fudge factor for modeline
	   ;; Scaling factors
	   (scale-x (/ (float usable-width) img-width))
	   (scale-y (/ (float usable-height) img-height))
	   (scale (min scale-x scale-y 1.0))) ;; never upscale
      (image-transform-set-scale scale))))

(defun my--image-mode-setup ()
  "Disable line numbers and fit image in image-mode buffers."
  (when (eq major-mode 'image-mode)
    ;; Defer to after display updates
    (run-at-time
     "0.01 sec" nil
     (lambda ()
       (with-current-buffer (current-buffer)
	 (setq-local display-line-numbers nil)
	 (my/image-fit-to-frame))))))

(add-hook 'image-mode-hook #'my--image-mode-setup)

;; Exclude image-mode from undo-tree
(with-eval-after-load 'undo-tree
  (add-to-list 'undo-tree-incompatible-major-modes 'image-mode))

;; }} END: image-mode general settings


;; {{ START: image viewer annotations

(defvar my--image-viewer-annotation-buffer "*Image Annotation*"
  "Buffer name used for displaying image annotations below the image window.")

(defun my--image-viewer-annotation-file (image-file)
  "Return the path to the annotation file for IMAGE-FILE."
  (concat image-file ".anno"))

(defun my--image-viewer-load-annotation (image-file)
  "Return the annotation text for IMAGE-FILE, or nil if none."
  (let ((anno-file (my--image-viewer-annotation-file image-file)))
    (when (file-exists-p anno-file)
      (with-temp-buffer
        (insert-file-contents anno-file)
        (string-trim (buffer-string))))))

(defvar my--image-viewer-annotation-timestamp t
  "If non-nil, append a timestamp line to the annotation when saving.")

(defun my--image-viewer-save-annotation (image-file text &optional no-timestamp)
  "Save TEXT as annotation for IMAGE-FILE.
If `my--image-viewer-annotation-timestamp` is non-nil, append a timestamp line.
With prefix argument NO-TIMESTAMP (or when called with C-u), skip timestamp."
  (let ((anno-file (my--image-viewer-annotation-file image-file))
	(my--image-viewer-annotation-timestamp
	 (if no-timestamp nil my--image-viewer-annotation-timestamp))
	(final-text text))
    (when (and my--image-viewer-annotation-timestamp
	       (not (string-empty-p text)))
      (setq final-text (concat text "\n- " (format-time-string "%Y-%m-%d %H:%M:%S"))))
    (with-temp-file anno-file
      (insert final-text))))

(defun my--image-viewer-display-annotation-buffer (text)
  "Display annotation TEXT in a bottom window, auto-fitting its height to content.
If TEXT is nil or empty, close the annotation window if present."
  (let ((buf (get-buffer-create my--image-viewer-annotation-buffer)))
    (if (and text (not (string-empty-p text)))
        (progn
          (with-current-buffer buf
            (setq buffer-read-only nil)
            (erase-buffer)
            (insert text)
            (setq-local buffer-read-only t)
            (setq-local truncate-lines nil)
            (setq-local word-wrap t)
            (setq-local mode-line-format '(" Annotation ")))
          ;; Display the buffer below current window
          (let ((win (display-buffer-in-side-window
                      buf '((side . bottom)
                            (slot . 1)))))
            ;; Auto-fit height based on number of lines (up to 1/3 of frame)
            (with-current-buffer buf
              (let* ((lines (count-lines (point-min) (point-max)))
                     (max-lines (max 1 (min lines (/ (frame-height) 3)))))
                (fit-window-to-buffer win max-lines)))))
      ;; If no annotation, close any existing annotation window
      (when-let ((win (get-buffer-window buf)))
        (delete-window win)))))

;; }} END: image viewer annotations


;; {{ START: image viewer zoom-mode integration

(defvar my--image-viewer-had-zoom nil
  "Non-nil if `zoom-mode` was enabled before viewing the image.")

(defvar my--image-viewer-last-buffer nil
  "Track the last active buffer for zoom restoration.")

(defun my--image-viewer--buffer-list-update ()
  "Restore `zoom-mode` if leaving an image-mode buffer."
  (let ((current (window-buffer (selected-window))))
    ;; If last buffer was image-mode and zoom was disabled, restore it
    (when (and my--image-viewer-had-zoom
               my--image-viewer-last-buffer
               (not (eq (buffer-local-value 'major-mode my--image-viewer-last-buffer) 'image-mode)))
      (setq my--image-viewer-had-zoom nil)
      (zoom-mode 1))
    ;; Update last buffer
    (setq my--image-viewer-last-buffer current)))

(defun my--image-viewer-show-annotation-for-current ()
  "Update bottom annotation display for the current image.
Temporarily disable `zoom-mode` while in image-mode, restore when leaving."
  (when (and (eq major-mode 'image-mode)
             (buffer-file-name))
    ;; Disable zoom-mode if active
    (when (bound-and-true-p zoom-mode)
      (setq my--image-viewer-had-zoom t)
      (zoom-mode -1))
    ;; Track last buffer and add buffer-list-update-hook once
    (setq my--image-viewer-last-buffer (current-buffer))
    (unless (member #'my--image-viewer--buffer-list-update buffer-list-update-hook)
      (add-hook 'buffer-list-update-hook #'my--image-viewer--buffer-list-update))
    ;; Show annotation
    (let ((anno (my--image-viewer-load-annotation (buffer-file-name))))
      (my--image-viewer-display-annotation-buffer anno))))

;; }} END: image viewer zoom-mode integration


;; {{ START: image viewer interactive commands

(defun my/image-viewer-add-edit-annotation ()
  "Add or edit annotation for current image, then refresh display.
With prefix argument (C-u), skip timestamp."
  (interactive)
  (let* ((file (buffer-file-name))
	 (current (my--image-viewer-load-annotation file))
	 (new (read-string "Annotation: " current)))
    (if (string-empty-p new)
	(message "No annotation saved.")
      (my--image-viewer-save-annotation file new current-prefix-arg)
      (message "Annotation saved.")
      (my--image-viewer-show-annotation-for-current))))

(defun my/image-viewer-delete-annotation ()
  "Delete annotation for current image, then refresh display."
  (interactive)
  (let ((anno-file (my--image-viewer-annotation-file (buffer-file-name))))
    (if (and (file-exists-p anno-file)
             (y-or-n-p "Delete annotation? "))
        (progn
          (delete-file anno-file)
          (message "Annotation deleted.")
          (my--image-viewer-show-annotation-for-current))
      (message "No annotation found."))))

;; }} END: image viewer interactive commands


;; {{ START: image viewer minor mode and navigation

(defvar my--image-viewer-files nil
  "List of image files to view.")

(defvar my--image-viewer-index 0
  "Current index in `my--image-viewer-files'.")

(defvar my-image-viewer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n")  #'my--image-viewer-next)
    (define-key map (kbd "p")  #'my--image-viewer-prev)
    (define-key map (kbd "d")  #'my--image-viewer-delete)
    (define-key map (kbd "u")  #'my--image-viewer-restore-last)
    (define-key map (kbd "=")  #'image-increase-size)
    (define-key map (kbd "-")  #'image-decrease-size)
    (define-key map (kbd "s")  #'my/image-viewer-toggle-slideshow)
    (define-key map (kbd "S")  #'my/image-viewer-toggle-slideshow_reverse)
    (define-key map (kbd "a")  #'my/image-viewer-add-edit-annotation)
    (define-key map (kbd "A")  #'my/image-viewer-delete-annotation)
    (define-key map (kbd "br") #'ibuffer)
    map)
  "Keymap for `my-image-viewer-mode'.")

(define-minor-mode my-image-viewer-mode
  "Minor mode for image viewer navigation."
  :lighter " ImgView"
  :keymap my-image-viewer-mode-map
  (if my-image-viewer-mode
      (when (bound-and-true-p evil-local-mode)
        (evil-local-mode -1))
    (when (fboundp 'evil-local-mode)
      (evil-local-mode 1))))

(defun my--image-viewer--after-display ()
  "Fit image to frame after display and enable my-image-viewer-mode."
  (when (eq major-mode 'image-mode)
    (setq-local display-line-numbers nil)
    (my/image-fit-to-frame)
    (my-image-viewer-mode 1)
    (my--image-viewer-show-annotation-for-current)))

(defun my--image-viewer-show-image ()
  "Show the image at the current index."
  (let ((file (nth my--image-viewer-index my--image-viewer-files)))
    (when file
      (find-file file)
      (image-mode)
      (my--image-viewer--after-display)
      (message "[%d/%d] %s"
               (1+ my--image-viewer-index)
               (length my--image-viewer-files)
               file))))

(defun my--image-viewer-next ()
  "Show the next image."
  (interactive)
  (when my--image-viewer-files
    (setq my--image-viewer-index
          (mod (1+ my--image-viewer-index) (length my--image-viewer-files)))
    (my--image-viewer-show-image)))

(defun my--image-viewer-prev ()
  "Show the previous image."
  (interactive)
  (when my--image-viewer-files
    (setq my--image-viewer-index
          (mod (1- my--image-viewer-index) (length my--image-viewer-files)))
    (my--image-viewer-show-image)))

;; }} END: image viewer minor mode and navigation


;; {{ START: image viewer trash and restore

(defvar my--image-viewer-trash-dir
  (expand-file-name "image-viewer-trash" user-emacs-directory)
  "Directory to store 'deleted' images temporarily.")

(defvar my--image-viewer-deleted-history nil
  "List of (original-path . trashed-path) for recently deleted images.")

(defun my--image-viewer-delete ()
  "Move the current image file to a custom trash folder and show the next image."
  (interactive)
  (let ((file (nth my--image-viewer-index my--image-viewer-files)))
    (when (and file (file-exists-p file)
	       (y-or-n-p (format "Move %s to viewer trash? " file)))
      (unless (file-directory-p my--image-viewer-trash-dir)
	(make-directory my--image-viewer-trash-dir t))
      (let* ((basename (file-name-nondirectory file))
	     (trashed-path (expand-file-name basename my--image-viewer-trash-dir)))
	;; Ensure unique name in trash
	(while (file-exists-p trashed-path)
	  (setq trashed-path (expand-file-name
			      (concat (file-name-sans-extension basename)
				      "-"
				      (format-time-string "%Y%m%d%H%M%S")
				      (file-name-extension basename t))
			      my--image-viewer-trash-dir)))
	;; Move the file
	(rename-file file trashed-path)
	;; Remember for restore
	(push (cons file trashed-path) my--image-viewer-deleted-history)
	;; Update viewer state
	(setq my--image-viewer-files (delete file my--image-viewer-files))
	(if my--image-viewer-files
	    (progn
	      (setq my--image-viewer-index
		    (mod my--image-viewer-index (length my--image-viewer-files)))
	      (my--image-viewer-show-image))
	  (kill-buffer)
	  (message "No more images."))))))

(defun my--image-viewer-restore-last ()
  "Restore the most recently deleted image from viewer trash."
  (interactive)
  (if-let ((entry (pop my--image-viewer-deleted-history)))
      (let ((orig (car entry))
	    (trashed (cdr entry)))
	(rename-file trashed orig t)
	(message "Restored %s" orig))
    (message "No deleted images to restore.")))

;; }} END: image viewer trash and restore


;; {{ START: image viewer main entry and slideshow

(defun my/image-viewer (directory)
  "View all images in DIRECTORY with navigation and deletion."
  (interactive "DDirectory: ")
  (let* ((files (sort (directory-files directory t "\\.\\(jpg\\|jpeg\\|png\\|gif\\|bmp\\|webp\\)$" t)
		      (lambda (a b)
			(string< (file-name-nondirectory a)
				 (file-name-nondirectory b))))))
    (if (null files)
	(message "No images found in %s" directory)
      (setq my--image-viewer-files files
	    my--image-viewer-index 0)
      (my--image-viewer-show-image))))

(defvar my--image-viewer-slideshow-timer nil
  "Timer object for image viewer slideshow.")

(defvar my--image-viewer-slideshow-interval 3
  "Number of seconds between images in slideshow mode.")

(defun my/image-viewer-start-slideshow (&optional interval reverse)
  "Start a slideshow of images in the current `my/image-viewer` session.
If INTERVAL is provided, use it (in seconds), otherwise use
`my--image-viewer-slideshow-interval`.
If REVERSE is non-nil, go backwards."
  (interactive
   (list
    (when current-prefix-arg
      (prefix-numeric-value current-prefix-arg))
    (y-or-n-p "Play slideshow in reverse? ")))
  (unless my--image-viewer-files
    (user-error "No images loaded. Run `my/image-viewer` first"))
  (when my--image-viewer-slideshow-timer
    (cancel-timer my--image-viewer-slideshow-timer))
  (setq my--image-viewer-slideshow-interval
        (if interval
            interval
          my--image-viewer-slideshow-interval))
  (setq my--image-viewer-slideshow-timer
        (run-at-time my--image-viewer-slideshow-interval
                     my--image-viewer-slideshow-interval
                     (if reverse
                         #'my--image-viewer-prev
                       #'my--image-viewer-next)))
  (message "Slideshow started (interval: %s sec, direction: %s)"
           my--image-viewer-slideshow-interval
           (if reverse "reverse" "forward")))

(defun my/image-viewer-stop-slideshow ()
  "Stop the running image viewer slideshow."
  (interactive)
  (when my--image-viewer-slideshow-timer
    (cancel-timer my--image-viewer-slideshow-timer)
    (setq my--image-viewer-slideshow-timer nil)
    (message "Slideshow stopped")))

(defun my/image-viewer-toggle-slideshow (&optional interval)
  "Toggle slideshow mode."
  (interactive "P")
  (if my--image-viewer-slideshow-timer
      (my/image-viewer-stop-slideshow)
    (my/image-viewer-start-slideshow interval)))

(defun my/image-viewer-toggle-slideshow_reverse (&optional interval)
  "Toggle reverse slideshow mode."
  (interactive "P")
  (if my--image-viewer-slideshow-timer
      (my/image-viewer-stop-slideshow)
    (my/image-viewer-start-slideshow interval t)))

;; }} END: image viewer main entry and slideshow


(provide 'init-image-viewer)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-image-viewer.el ends here
