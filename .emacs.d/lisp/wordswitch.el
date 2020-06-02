;;; wordswitch.el ---  Word Switch is a tool to try a different word or sentence so you don't have to delete one idea to create another.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Travis Jeffery

;; Author: Travis Jeffery <tj@travisjeffery.com>
;; URL: http://github.com/travisjeffery/wordswitch.el
;; Version: 1.0
;; Package-Requires: ((emacs "25.2"))
;; Keywords: writing, text

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my package.  It is nice.  You should try it.

;;;; Installation

;;;;; MELPA

;; If you installed from MELPA, you're done.

;;;;; Manual

;; Put this file in your load-path, and put this in your init
;; file:

;; (require 'wordswitch)

;;;; Usage

;; Highlight a region and run this command:

;; `wordswitch'

;; A wordswitch buffer will pop up, add and remove different words or sentences by pressing + and
;; -.  And switch between sentences by navigating up and down to the next sentence by pressing n and
;; to the previous sentence by pressing p.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar-local wordswitch-beg nil "The beginning point of the word or sentence in the original buffer.")
(defvar-local wordswitch-end nil "The ending point of the word or sentence in the original buffer.")
(defvar-local wordswitch-str nil "The original word or sentence.")
(defvar-local wordswitch-buf nil "The original buffer.")

(defvar wordswitch-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'wordswitch-add)
    (define-key map "-" 'wordswitch-remove)
    (define-key map "p" 'wordswitch-previous-line)
    (define-key map "n" 'wordswitch-next-line)
    (define-key map (kbd "C-c C-c") 'wordswitch-commit)
    (define-key map (kbd "C-c C-k") 'wordswitch-abort)
    (define-key map [remap previous-line] 'wordswitch-previous-line)
    (define-key map [remap next-line] 'wordswitch-next-line)
    map))

(define-derived-mode wordswitch-mode special-mode "wordswitch"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (hl-line-mode +1)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1)))

(defun wordswitch-add ()
  "Add a word or sentence option."
  (interactive)
  (let ((str (read-from-minibuffer "Add: "))
        (inhibit-read-only t))
    (goto-char (point-max))
    (unless (eq (point-max) (point-min))
      (insert "\n"))
    (insert str)
    (wordswitch-update-buffer-for-current-line)))

(defun wordswitch-remove ()
  "Remove the current word or sentence option."
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (kill-whole-line -1)))

(defun wordswitch-commit ()
  "Keep the current word or sentence and kill the wordswitch buffer."
  (interactive)
  (with-current-buffer "*wordswitch*"
    (kill-buffer-and-window)))

(defun wordswitch-abort ()
  "Keep the original word or sentence and kill the wordswitch buffer."
  (interactive)
  (goto-char (point-min))
  (wordswitch-update-buffer-for-current-line)
  (wordswitch-commit))

(defun wordswitch-previous-line ()
  "Move to the previous line."
  (interactive)
  (forward-line -1)
  (wordswitch-update-buffer-for-current-line))

(defun wordswitch-next-line ()
  "Move to the next line."
  (interactive)
  (forward-line)
  (wordswitch-update-buffer-for-current-line))

(defun wordswitch-update-buffer-for-current-line ()
  "Update the original buffer with the current line."
  (wordswitch-update-buffer (wordswitch-current-line-string)))

(defun wordswitch-current-line-string ()
  "Return the current line as a string."
  (buffer-substring (point-at-bol) (point-at-eol)))

(defun wordswitch-update-buffer (str)
  "Update the original buffer with the given string STR."
  (if wordswitch-buf
      (let ((beg wordswitch-beg)
            (cur-end wordswitch-end)
            (new-end wordswitch-end))
        (with-current-buffer wordswitch-buf
          (progn
            (goto-char beg)
            (if cur-end
                (kill-region beg cur-end))
            (insert str)
            (setq new-end (point))))
        (setq-local wordswitch-end new-end))))

(defun wordswitch ()
  "Show wordswitch for the highlighted region."
  (interactive)
  (split-window)
  (let* ((cur (current-buffer))
        (reg (region-active-p))
        (beg (region-beginning))
        (end (region-end))
        (str (buffer-substring (region-beginning) (region-end)))
        (_ (switch-to-buffer "*wordswitch*"))
        (_ (wordswitch-mode))
        (inhibit-read-only t))
    (setq-local wordswitch-buf cur)
    (setq-local wordswitch-str str)
    (setq-local wordswitch-beg beg)
    (if reg
        (progn
          (setq-local wordswitch-end end)
          (insert wordswitch-str)))))

(provide 'wordswitch)
;;; wordswitch.el ends here
