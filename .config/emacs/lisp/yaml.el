(defun tj-yaml-indentation-level (s)
  (if (string-match "^ " s)
      (+ 1 (tj-yaml-indentation-level (substring s 1)))
    0))

(defun tj-yaml-current-line ()
  (buffer-substring-no-properties (point-at-bol) (point-at-eol)))

(defun tj-yaml-clean-string (s)
  (let* ((s (replace-regexp-in-string "^[ -:]*" "" s))
         (s (replace-regexp-in-string ":.*$" "" s)))
    s))

(defun tj-yaml-not-blank-p (s)
  (string-match "[^[:blank:]]" s))

(defun tj-yaml-path-to-point ()
  (save-excursion
    (let* ((line (tj-yaml-current-line))
           (level (tj-yaml-indentation-level line))
           (result (list (tj-yaml-clean-string line))))      
      (while (> (point) (point-min))
        (beginning-of-line 0)
        (setq line (tj-yaml-current-line))

        (let ((new-level (tj-yaml-indentation-level line)))
          (when (and (tj-yaml-not-blank-p line)
                     (< new-level level))

            (setq level new-level)
            (setq result (push (tj-yaml-clean-string line) result)))))

      (mapconcat 'identity result "."))))

(defun tj-yaml-show-path-to-point (&optional arg)
  "Show path to point. If ARG set, then kill path too."
  (interactive "p")
  (let ((path (tj-yaml-path-to-point)))
    (message path)
    (if arg
      (kill-new path))))

