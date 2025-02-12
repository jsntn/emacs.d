;;; init-display.el --- display settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:



(use-package delight)

(use-package diminish)

(use-package doom-themes
  :config
  ;; global settings (defaults)
  (setq doom-themes-enable-bold t) ; if nil, bold is universally disabled
  ;; corrects (and improves) org-mode's native fontification
  ;; update: disable this as it is not compatible with org-modern horizontal
  ;; line, see https://github.com/jsntn/emacs.d/issues/13
  ;; (doom-themes-org-config)

  ;; theme does not load correctly in daemon mode, see,
  ;; - https://stackoverflow.com/a/23668935/4274775
  ;; - https://github.com/cpaulik/emacs-material-theme/issues/45#issuecomment-385247309
  ;; - https://github.com/nordtheme/emacs/issues/59
  ;; customization on doom-monokai-classic
  (defun my-load-theme ()
    (load-theme 'doom-monokai-classic t)
    (custom-set-faces
     `(mode-line ((t (:background ,(doom-color 'dark-violet)))))
     `(font-lock-comment-face ((t (:foreground ,(doom-color 'base6)))))
     `(default ((t (:background "black"))))))
  (if (daemonp)
      (add-hook 'after-make-frame-functions
		(lambda (frame)
		  (with-selected-frame frame
		    (my-load-theme))))
    (my-load-theme))
  )


;; { START: hide list of minor modes in mode-line
;; from https://emacs.stackexchange.com/a/3928/29715
(defvar my-hidden-minor-modes
  '(abbrev-mode
    auto-capitalize-mode
    auto-fill-function
    auto-revert-mode
    dired-async-mode
    flycheck-mode
    flyspell-mode
    ;; haskell-indent-mode
    ;; haskell-doc-mode
    ;; inf-haskell-mode
    org-roam-mode
    pangu-spacing-mode
    projectile-mode
    pyim-isearch-mode
    ;; smooth-scroll-mode
    undo-tree-mode
    which-key-mode
    evil-collection-unimpaired-mode
    hs-minor-mode
    org-remark-global-tracking-mode
    yas-minor-mode
    eldoc-mode
    org-indent-mode
    ))

(defun my/purge-minor-modes ()
  (interactive)
  (dolist (x my-hidden-minor-modes nil)
    (let ((trg (cdr (assoc x minor-mode-alist))))
      (when trg
        (setcar trg "")))))

(add-hook 'after-change-major-mode-hook 'my/purge-minor-modes)
;; END: hide list of minor modes in mode-line }


;; { -- START: display time in mode line --
;; reference:
;; ... https://www.reddit.com/r/emacs/comments/6ftm3x/share_your_modeline_customization/dil4x5z/?utm_source=reddit&utm_medium=web2x&context=3
;; ... http://emacs.1067599.n8.nabble.com/Week-number-td89988.html
(setq display-time-string-forms
      '((propertize
	 ;; %W and %V
	 ;; http://emacs.1067599.n8.nabble.com/Week-number-tp89988p89991.html
	 (format-time-string "[%V] %m-%d %a %H:%M:%S" now) ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Time-Parsing.html
	 ;; 'face 'modeline-display-time
	 'help-echo (format-time-string "[%V] %m-%d %a %H:%M:%S" now))))
(display-time-mode 1)
(defun my-update-time ()
  "Update the time string in the mode line every second."
  (display-time-mode 1))
(run-with-timer 0 1 'my-update-time)
;; -- END: display time in mode line -- }


(add-hook 'emacs-lisp-mode-hook 'show-paren-mode) ; highlight matching
						  ; parenthesis

(setq display-line-numbers-width-start t)

;; use below to fix slow scrolling issue
(global-display-line-numbers-mode 1)
;; via https://www.reddit.com/r/orgmode/comments/e7pq7k/linummode_very_slow_for_large_org_files/
;; there is display issue on citre-peek, see,
;; https://github.com/universal-ctags/citre/issues/161

(setq column-number-mode t) ; turn on column numbers

;; wrap lines at 80 characters
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq-default fill-column 80)

;; disable splash screen and startup message
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; disable the bars display
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)

;; recent files
;; https://stackoverflow.com/questions/50417/how-do-i-get-list-of-recent-files-in-gnu-emacs/50422#50422
(recentf-mode 1)





;; the default split-screen direction
;; https://stackoverflow.com/a/7998271
;; (setq split-width-threshold nil) ; for vertical split
(setq split-width-threshold 1 ) ; for horizontal split

(defun my/windows-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")

    (let* ((current-buffer (window-buffer))
	   (other-window-buffer (window-buffer (next-window)))
	   (func (if (window-full-height-p)
		     #'split-window-vertically
		   #'split-window-horizontally)))
      (delete-other-windows)

      ;; Split the window as needed (either horizontally or vertically)
      (funcall func)

      ;; Ensure that the original buffer from the other window is switched to
      (let ((other-win (selected-window)))
	(select-window (next-window other-win))
	(switch-to-buffer other-window-buffer)

	;; Return to the original window to maintain focus if needed
	(select-window other-win)))))

(defun xah-syntax-color-hex ()
  "Syntax color text of the form 「#ff1100」 and 「#abc」 in current buffer.
URL `http://ergoemacs.org/emacs/emacs_CSS_colors.html'
Version 2017-03-12"
  (interactive)
  (font-lock-add-keywords
   nil
   '(("#[[:xdigit:]]\\{3\\}"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background
		      (let* (
			     (ms (match-string-no-properties 0))
			     (r (substring ms 1 2))
			     (g (substring ms 2 3))
			     (b (substring ms 3 4)))
			(concat "#" r r g g b b))))))
     ("#[[:xdigit:]]\\{6\\}"
      (0 (put-text-property
	  (match-beginning 0)
	  (match-end 0)
	  'face (list :background (match-string-no-properties 0)))))))
  (font-lock-flush)
  )

;; FIXME: to be tested...

;; explaination from ChatGPT,

;; When show-trailing-whitespace is set to t, it makes whitespace characters
;; at the end of lines in the buffer visible by highlighting them. This is
;; useful for detecting trailing whitespace, which can cause problems in
;; certain programming languages or when copying and pasting code.
;;
;; So in summary, the line (setq show-trailing-whitespace t) enables
;; highlighting of trailing whitespace characters in the buffer.

;; visualize trailing whitespace
(defun my/show-trailing-whitespace ()
  (setq show-trailing-whitespace t)
  )

;; codes folding
(load-library "hideshow")
;; refer to https://sachachua.com/blog/2006/10/emacs-hideshow/
(defun my/toggle-hideshow-all ()
  "Toggle hideshow all."
  (interactive)
  (set (make-variable-buffer-local 'my-hs-hide-all) (not my-hs-hide-all))
  (if my-hs-hide-all
      (hs-hide-all)
    (hs-show-all)))
(defun my/toggle-hideshow-block ()
  "Toggle hideshow block."
  (interactive)
  (set (make-variable-buffer-local 'my-hs-hide-block) (not my-hs-hide-block))
  (if my-hs-hide-block
      (progn
	(beginning-of-defun)
	(hs-hide-block))
    (hs-show-block)))

;; { -- START: default inline image background in Org-mode
;; https://emacs.stackexchange.com/a/37927/29715
;; note: restart Emacs to make this change effective
(defcustom org-inline-image-background nil
  "The color used as the default background for inline images.
When nil, use the default face background."
  :group 'org
  :type '(choice color (const nil)))

(defvar my-bg-color-to-create-image 'transparent
  "Variable to track the current advice for create-image.
   Possible values: 'transparent or 'white.")

(defun my-create-image-with-white-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
	 (type (cadr args))
	 (data-p (caddr args))
	 (props (cdddr args)))
    ;; Get this return result style from `create-image'.
    (append (list file type data-p)
	    (list :background "white")
	    props)))

(defun my-create-image-with-transparent-background-color (args)
  "Specify background color of Org-mode inline image through modify `ARGS'."
  (let* ((file (car args))
	 (type (cadr args))
	 (data-p (caddr args))
	 (props (cdddr args)))
    ;; Get this return result style from `create-image'.
    (append (list file type data-p)
	    (list :background "transparent")
	    props)))

(advice-add 'create-image :filter-args
	    #'my-create-image-with-transparent-background-color)

(defun my/toggle-bg-color-to-create-image ()
  "Toggle between transparent and white background color advice for create-image."
  (interactive)
  (advice-remove 'create-image 'my-create-image-with-transparent-background-color)
  (advice-remove 'create-image 'my-create-image-with-white-background-color)
  (if (eq my-bg-color-to-create-image 'transparent)
      (progn
	(advice-add 'create-image :filter-args
		    #'my-create-image-with-white-background-color)
	(setq my-bg-color-to-create-image 'white)
	(message "Switched background color for create-image to white.")
	)
    (progn
      (advice-add 'create-image :filter-args
		  #'my-create-image-with-transparent-background-color)
      (setq my-bg-color-to-create-image 'transparent)
      (message "Switched background color for create-image to transparent.")
      )))
;; -- END: default inline image background in Org-mode }


(provide 'init-display)

;; Local Variables:
;; coding: utf-8
;; End:
;;; init-display.el ends here
