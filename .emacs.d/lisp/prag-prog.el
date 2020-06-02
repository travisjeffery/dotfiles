;;; prag-prog.el --- Tools to help write and edit Prag Prog books. -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'cl-lib)

;;; Code:

(defvar-local prag-prog-tags-hidden nil "Whether tags are hidden or not.")

(cl-defstruct (prag-prog-tag
               (:constructor prag-prog-tag-create)
               (:copier nil))
  name highlight edit)

(defvar prag-prog-tags
  (list
   (prag-prog-tag-create :name "filename")
   (prag-prog-tag-create :name "dir")
   (prag-prog-tag-create :name "ed" :highlight 'hi-yellow :edit t)
   (prag-prog-tag-create :name "author" :highlight 'hi-blue :edit t)))

(defun prag-prog--edit-tags ()
  (cl-remove-if (lambda (tag) (not (prag-prog-tag-edit tag))) prag-prog-tags))

(defun prag-prog--tag-re (tag)
  "Regexp for TAG."
  (let ((name (prag-prog-tag-name tag)))
    (format "<%s>\\(.\\|\n\\)*?</%s>" name name)))

(defun prag-prog--format-defun-name (string &rest objects)
  "Format OBJECTS with STRING and return as a symbol."
  (intern (apply #'format string objects)))

(cl-dolist (tag prag-prog-tags)
  (eval
   (let* (
          (tag-re (prag-prog--tag-re tag))
          (tag-name (prag-prog-tag-name tag))
          (tag-open (format "<%s>" tag-name))
          (tag-close (format "</%s>" tag-name))
          (tag-close-length (length tag-close))
          (highlight-face (prag-prog-tag-highlight tag))

          (defun-insert (prag-prog--format-defun-name "prag-prog-insert-%s-tag" tag-name))
          (defun-next (prag-prog--format-defun-name "prag-prog-next-%s-tag" tag-name))
          (defun-prev (prag-prog--format-defun-name "prag-prog-prev-%s-tag" tag-name))
          (defun-highlight (prag-prog--format-defun-name "prag-prog-highlight-%s-tag" tag-name)))
     `(progn
        (cl-defun ,defun-next ()
          "Move to the next tag."
          (interactive)
          (search-forward ,tag-close))

        (cl-defun ,defun-prev ()
          "Move to the previous tag."
          (interactive)
          (search-backward ,tag-open))

        (cl-defun ,defun-insert ()
          "Inert the tag."
          (interactive)
          (insert ,tag-open ,tag-close)
          (backward-char ,tag-close-length))

        (cl-defun ,defun-highlight ()
          "Highlight the tag."
          (interactive)
          (highlight-regexp ,tag-re ',highlight-face nil))))))


(defun prag-prog-stat ()
  "Count number of tags."
  (interactive)
  (let ((counts
         (s-join "\n"
                 (let* ((str (buffer-substring-no-properties (point-min) (point-max))))
                   (cl-mapcar (lambda (tag)
                                (format "%s %d"
                                        (prag-prog-tag-name tag)
                                        (s-count-matches (prag-prog--tag-re tag) str)))
                              (prag-prog--edit-tags))))))
    (with-temp-buffer
      (insert counts)
      (align-regexp (point-min) (point-max) ".* \\([0-9,]+\\).*" -1)
      (message (buffer-substring-no-properties (point-min) (point-max))))))

(defun prag-prog-show-edit-tags ()
  "Make edit tags visible."
  (interactive)
  (save-excursion
    (cl-dolist (tag (prag-prog--edit-tags))
      (condition-case nil
          (while (not (eobp))
            (re-search-forward (prag-prog--tag-re tag))
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (remove-from-invisibility-spec '(prag-prog . t))))
        (error nil)))))

(defun prag-prog-hide-edit-tags ()
  "Make edit tags invisible."
  (interactive)
  (add-to-invisibility-spec '(prag-prog . t))
  (save-excursion
    (cl-dolist (tag (prag-prog--edit-tags))
      (goto-char (point-min))
      (condition-case nil
          (while (not (eobp))
            (message (prag-prog--tag-re tag))
            (re-search-forward (prag-prog--tag-re tag))
            (let ((beg (match-beginning 0))
                  (end (match-end 0)))
              (overlay-put (make-overlay beg end) 'invisible 'prag-prog)))
        (error nil)))))

(defun prag-prog-highlight-tags ()
  "Highlight of tags."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (cl-dolist (tag prag-prog-tags)
      (when (prag-prog-tag-highlight tag)
        (goto-char (point-min))
        (highlight-regexp (prag-prog--tag-re tag) (prag-prog-tag-highlight tag) nil)))))

(defun prag-prog-unhighlight-tags ()
  "Unhighlight of tags."
  (interactive)
  (save-excursion
    (cl-dolist (tag prag-prog-tags)
      (goto-char (point-min))
      (unhighlight-regexp (prag-prog--tag-re tag)))))

(define-minor-mode prag-prog-mode
  "Mode for writing Prag Prog markdown."
  :lighter " prag-prog-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p") 'prag-prog-prev-ed-tag)
            (define-key map (kbd "M-n") 'prag-prog-next-ed-tag)
            (define-key map (kbd "C-c C-c a") 'prag-prog-insert-author-tag)
            (define-key map (kbd "C-c C-c f") 'prag-prog-insert-filename-tag)
            (define-key map (kbd "C-c C-c d") 'prag-prog-insert-dir-tag)
            map))

(defun prag-prog-mode-setup ()
  "Setup 'prag-prog-mode'."
  (interactive)
  (prag-prog-highlight-tags))

(add-hook 'prag-prog-mode-hook 'prag-prog-mode-setup)

(provide 'prag-prog)

;;; prag-prog.el ends here
