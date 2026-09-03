;;; prune-mode.el --- interactively prune lines from a buffer  -*- lexical-binding: t; -*-

(require 'cl-lib)

(defgroup prune-mode nil
  "Interactively mark, narrow, and delete lines from a buffer."
  :group 'convenience)

(defface prune-mode-mark-face
  '((t (:inherit error)))
  "Face used to highlight marked lines."
  :group 'prune-mode)

;;; Internal helpers

(defun prune-mode--bol () (line-beginning-position))
(defun prune-mode--eol () (line-end-position))

(defmacro prune-mode--with-widen (&rest body)
  (declare (indent 0))
  `(save-restriction
     (widen)
     ,@body))

(defun prune-mode--line-region-at (bol)
  "Return (BOL . END) for line starting at BOL, including trailing newline."
  (save-excursion
    (goto-char bol)
    (cons bol (min (1+ (prune-mode--eol)) (point-max)))))

(defun prune-mode--mark-overlay-at (bol)
  "Return prune-mode mark overlay at BOL, or nil."
  (cl-find-if (lambda (ov) (overlay-get ov 'prune-mode))
              (overlays-at bol)))

(defun prune-mode--marked-p (bol)
  (and (prune-mode--mark-overlay-at bol) t))

(defun prune-mode--set-mark (bol val)
  (let ((ov (prune-mode--mark-overlay-at bol)))
    (cond
     (val
      (pcase-let ((`(,beg . ,end) (prune-mode--line-region-at bol)))
        (if (overlayp ov)
            (move-overlay ov beg end)
          (let ((new (make-overlay beg end nil t nil)))
            (overlay-put new 'face 'prune-mode-mark-face)
            (overlay-put new 'prune-mode t)
            (overlay-put new 'evaporate t)))))
     ((overlayp ov)
      (delete-overlay ov)))))

(defun prune-mode--remove-all-marks ()
  (prune-mode--with-widen
    (remove-overlays (point-min) (point-max) 'prune-mode t)))

(defun prune-mode--region-active-p ()
  "Return non-nil if mark is active (robust for special-mode buffers)."
  (and mark-active (mark)))

(defun prune-mode--region-line-bounds ()
  "Return (FIRST-BOL . LAST-BOL) for lines covered by region.
If region ends exactly at beginning of a line, that last line is excluded."
  (unless (prune-mode--region-active-p)
    (user-error "No active region"))
  (let* ((a (point))
         (b (mark))
         (beg (min a b))
         (end (max a b))
         first last)
    (save-excursion
      (goto-char beg)
      (setq first (line-beginning-position))
      (goto-char end)
      ;; If region ends at BOL (e.g. C-SPC then move down with line motion),
      ;; exclude that line so "down 3 lines" selects 3 lines.
      (when (and (= end (line-beginning-position))
                 (> end first))
        (forward-line -1))
      (setq last (line-beginning-position)))
    (cons first last)))

;;; Navigation

(defun prune-mode-next-line () (interactive) (forward-line 1))
(defun prune-mode-previous-line () (interactive) (forward-line -1))

;;; Marking

(defun prune-mode-unmark ()
  "Unmark current line and move down one line."
  (interactive)
  (let ((bol (prune-mode--bol)))
    (prune-mode--set-mark bol nil)
    (forward-line 1)))

(defun prune-mode-unmark-backward ()
  "Unmark previous line and move to it."
  (interactive)
  (let ((here (point)))
    (forward-line -1)
    (if (< (point) (point-min))
        (goto-char here)
      (prune-mode--set-mark (prune-mode--bol) nil))))

(defun prune-mode-unmark-all ()
  (interactive)
  (prune-mode--remove-all-marks)
  (message "Prune: unmarked all"))

(defun prune-mode-toggle-mark ()
  "Toggle mark on current line."
  (interactive)
  (let ((bol (prune-mode--bol)))
    (prune-mode--set-mark bol (not (prune-mode--marked-p bol)))))

(defun prune-mode-toggle-mark-and-next ()
  "Toggle mark on current line and move down one line."
  (interactive)
  (prune-mode-toggle-mark)
  (forward-line 1))

(defun prune-mode-mark-all-visible ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (let ((bol (prune-mode--bol)))
        (prune-mode--set-mark bol t))
      (forward-line 1)))
  (message "Prune: marked all visible lines"))

(defun prune-mode-mark-region-or-mark-and-next ()
  "If region active, mark all lines it spans and move past the region.
Otherwise mark current line and move down one line."
  (interactive)
  (if (prune-mode--region-active-p)
      (pcase-let* ((`(,first . ,last) (prune-mode--region-line-bounds)))
        (save-excursion
          (goto-char first)
          (while (<= (point) last)
            (let ((bol (line-beginning-position)))
              (prune-mode--set-mark bol t))
            (forward-line 1)))
        (goto-char last)
        (forward-line 1)
        (deactivate-mark))
    (let ((bol (prune-mode--bol)))
      (prune-mode--set-mark bol t)
      (forward-line 1))))

(defun prune-mode-mark-region-or-toggle-and-next ()
  "If region active, mark all lines it spans and move past the region.
Otherwise toggle mark on current line and move down one line."
  (interactive)
  (if (prune-mode--region-active-p)
      (prune-mode-mark-region-or-mark-and-next)
    (prune-mode-toggle-mark-and-next)))

(defun prune-mode-flag-line ()
  "Flag current line for deletion and move down one line."
  (interactive)
  (let ((bol (prune-mode--bol)))
    (prune-mode--set-mark bol t)
    (forward-line 1)))

(defun prune-mode-toggle-marks ()
  "Toggle marks on all visible lines."
  (interactive)
  (let ((marked 0)
        (unmarked 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (prune-mode--bol)))
          (if (prune-mode--marked-p bol)
              (progn
                (setq marked (1+ marked))
                (prune-mode--set-mark bol nil))
            (setq unmarked (1+ unmarked))
            (prune-mode--set-mark bol t)))
        (forward-line 1)))
    (message "Prune: toggled (%d unmarked, %d marked)" marked unmarked)))

(defun prune-mode--mark-by-regexp (re val)
  (let ((changed 0))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (let ((bol (prune-mode--bol))
              (eol (prune-mode--eol)))
          (when (save-excursion
                  (goto-char bol)
                  (re-search-forward re eol t))
            (setq changed (1+ changed))
            (prune-mode--set-mark bol val)))
        (forward-line 1)))
    changed))

(defun prune-mode-mark-regexp (re)
  "Mark all visible lines matching regexp RE."
  (interactive (list (read-regexp "Mark lines matching regexp: ")))
  (message "Prune: marked %d line(s)" (prune-mode--mark-by-regexp re t)))

(defun prune-mode-unmark-regexp (re)
  "Unmark all visible lines matching regexp RE."
  (interactive (list (read-regexp "Unmark lines matching regexp: ")))
  (message "Prune: unmarked %d line(s)" (prune-mode--mark-by-regexp re nil)))

(defun prune-mode-count-marked ()
  "Show count of marked lines (across the whole buffer, even if narrowed)."
  (interactive)
  (let ((count (prune-mode--with-widen
                 (let ((n 0))
                   (dolist (ov (overlays-in (point-min) (point-max)))
                     (when (overlay-get ov 'prune-mode)
                       (setq n (1+ n))))
                   n))))
    (message "Prune: %d marked line(s)" count)))

;;; Deleting

(defun prune-mode-delete-line ()
  "Delete current line immediately."
  (interactive)
  (let ((inhibit-read-only t)
        (bol (prune-mode--bol)))
    (prune-mode--set-mark bol nil)
    (pcase-let ((`(,beg . ,end) (prune-mode--line-region-at bol)))
      (delete-region beg end))))

(defun prune-mode-delete-marked ()
  "Delete all marked lines."
  (interactive)
  (let ((inhibit-read-only t))
    (prune-mode--with-widen
      (let ((bols nil))
        (dolist (ov (overlays-in (point-min) (point-max)))
          (when (overlay-get ov 'prune-mode)
            (push (overlay-start ov) bols)))
        (setq bols (sort (delete-dups bols) #'>))
        (dolist (bol bols)
          (pcase-let ((`(,beg . ,end) (prune-mode--line-region-at bol)))
            (delete-region beg end)))))
    (prune-mode-unmark-all)
    (message "Prune: deleted marked lines")))

(defun prune-mode-delete-region-lines ()
  "Delete all lines spanned by the active region."
  (interactive)
  (unless (prune-mode--region-active-p)
    (user-error "No active region"))
  (let ((inhibit-read-only t))
    (pcase-let* ((`(,first . ,last) (prune-mode--region-line-bounds)))
      ;; Delete bottom-up so positions stay valid.
      (let (bols)
        (save-excursion
          (goto-char first)
          (while (<= (point) last)
            (push (line-beginning-position) bols)
            (forward-line 1)))
        (dolist (bol (sort bols #'>))
          (pcase-let ((`(,beg . ,end) (prune-mode--line-region-at bol)))
            (delete-region beg end)))))
    (deactivate-mark)
    (message "Prune: deleted region lines")))

(defun prune-mode-execute ()
  "If region active, delete region lines. Otherwise delete marked lines."
  (interactive)
  (if (prune-mode--region-active-p)
      (prune-mode-delete-region-lines)
    (prune-mode-delete-marked)))

;;; Narrowing

(defun prune-mode-narrow (re)
  "Narrow buffer to first and last line matching RE."
  (interactive "sNarrow (regex): ")
  (let ((start (save-excursion
                 (goto-char (point-min))
                 (if (re-search-forward re nil t)
                     (line-beginning-position)
                   (point-min))))
        (end (save-excursion
               (goto-char (point-max))
	               (if (re-search-backward re nil t)
	                   (line-end-position)
	                 (point-max)))))
    (narrow-to-region start end)
    ;; Marks are overlays, so they stay consistent across narrowing.
    ))

;;; Navigation (marked)

(defun prune-mode-next-marked-line ()
  "Move point to the next marked line."
  (interactive)
  (let ((start (point)))
    (forward-line 1)
    (while (and (not (eobp))
                (not (prune-mode--marked-p (prune-mode--bol))))
      (forward-line 1))
    (when (eobp)
      (goto-char start)
      (user-error "No next marked line"))))

(defun prune-mode-previous-marked-line ()
  "Move point to the previous marked line."
  (interactive)
  (let ((start (point)))
    (forward-line -1)
    (while (and (not (bobp))
                (not (prune-mode--marked-p (prune-mode--bol))))
      (forward-line -1))
    (when (and (bobp)
               (not (prune-mode--marked-p (prune-mode--bol))))
      (goto-char start)
      (user-error "No previous marked line"))))

;;; Keymap + mode

(defvar prune-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'prune-mode-next-line)
    (define-key map (kbd "p") #'prune-mode-previous-line)
    (define-key map (kbd "SPC") #'prune-mode-next-line)

    (define-key map (kbd "m") #'prune-mode-mark-region-or-mark-and-next)
    (define-key map (kbd "d") #'prune-mode-flag-line)
    (define-key map (kbd "RET") #'prune-mode-toggle-mark-and-next)
    (define-key map (kbd "M-m") #'prune-mode-toggle-mark-and-next)
    (define-key map (kbd "u") #'prune-mode-unmark)
    (define-key map (kbd "DEL") #'prune-mode-unmark-backward)
    (define-key map (kbd "U") #'prune-mode-unmark-all)
    (define-key map (kbd "* m") #'prune-mode-mark-all-visible)
    (define-key map (kbd "* u") #'prune-mode-unmark-all)
    (define-key map (kbd "t") #'prune-mode-toggle-marks)
    (define-key map (kbd "* t") #'prune-mode-toggle-marks)

    (define-key map (kbd "% m") #'prune-mode-mark-regexp)
    (define-key map (kbd "% d") #'prune-mode-mark-regexp)
    (define-key map (kbd "% u") #'prune-mode-unmark-regexp)

    (define-key map (kbd "D") #'prune-mode-delete-line)
    (define-key map (kbd "x") #'prune-mode-execute)

    (define-key map (kbd "M-n") #'prune-mode-next-marked-line)
    (define-key map (kbd "M-p") #'prune-mode-previous-marked-line)
    (define-key map (kbd "c") #'prune-mode-count-marked)

    (define-key map (kbd "/") #'prune-mode-narrow)
    (define-key map (kbd "w") #'widen)

    (define-key map (kbd "q") #'quit-window)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `prune-mode'.")

;;;###autoload
(define-derived-mode prune-mode special-mode "Prune"
  "Interactively mark, narrow, and delete lines from a buffer."
  (use-local-map prune-mode-map)
  (setq truncate-lines t))

(provide 'prune-mode)
;;; prune-mode.el ends here
