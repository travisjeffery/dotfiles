;-*- coding: utf-8 -*-
;; Shortcuts for ERGOEMACS_KEYBOARD_LAYOUT=it
;; Keyboard Layout: Italian
;; Contributor: David Capello, Francesco Biccari
;; Creation date: 2009-09

;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(defconst ergoemacs-backward-char-key			(kbd "M-j"))
(defconst ergoemacs-forward-char-key			(kbd "M-l"))
(defconst ergoemacs-previous-line-key			(kbd "M-i"))
(defconst ergoemacs-next-line-key			(kbd "M-k"))

;; Move by word
(defconst ergoemacs-backward-word-key			(kbd "M-u"))
(defconst ergoemacs-forward-word-key			(kbd "M-o"))

;; Move by paragraph
(defconst ergoemacs-backward-paragraph-key		(kbd "M-U"))
(defconst ergoemacs-forward-paragraph-key		(kbd "M-O"))

;; Move to beginning/ending of line
(defconst ergoemacs-move-beginning-of-line-key		(kbd "M-h"))
(defconst ergoemacs-move-end-of-line-key		(kbd "M-H"))

;; Move by screen (page up/down)
(defconst ergoemacs-scroll-down-key			(kbd "M-I"))
(defconst ergoemacs-scroll-up-key			(kbd "M-K"))

;; Move to beginning/ending of file
(defconst ergoemacs-beginning-of-buffer-key		(kbd "M-J"))
(defconst ergoemacs-end-of-buffer-key			(kbd "M-L"))

;; isearch
(defconst ergoemacs-isearch-forward-key			(kbd "M-ò"))
(defconst ergoemacs-isearch-backward-key		(kbd "M-ç"))

(defconst ergoemacs-recenter-key			(kbd "M-p"))

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(defconst ergoemacs-delete-backward-char-key		(kbd "M-d"))
(defconst ergoemacs-delete-char-key			(kbd "M-f"))

; Delete previous/next word.
(defconst ergoemacs-backward-kill-word-key		(kbd "M-e"))
(defconst ergoemacs-kill-word-key			(kbd "M-r"))

; Copy Cut Paste, Paste previous
(defconst ergoemacs-kill-region-key			(kbd "M-x"))
(defconst ergoemacs-kill-ring-save-key			(kbd "M-c"))
(defconst ergoemacs-yank-key				(kbd "M-v"))
(defconst ergoemacs-yank-pop-key			(kbd "M-V"))
(defconst ergoemacs-copy-all-key			(kbd "M-C"))
(defconst ergoemacs-cut-all-key				(kbd "M-X"))

;; undo and redo
(defconst ergoemacs-redo-key				(kbd "M-Z"))
(defconst ergoemacs-undo-key				(kbd "M-z"))

; Kill line
(defconst ergoemacs-kill-line-key			(kbd "M-g"))
(defconst ergoemacs-kill-line-backward-key		(kbd "M-G"))

;;; Textual Transformation

(defconst ergoemacs-mark-paragraph-key			(kbd "M-S-SPC"))
(defconst ergoemacs-shrink-whitespaces-key		(kbd "M-w"))
(defconst ergoemacs-comment-dwim-key			(kbd "M-à"))
(defconst ergoemacs-toggle-letter-case-key		(kbd "M--"))

; keyword completion, because Alt+Tab is used by OS
(defconst ergoemacs-call-keyword-completion-key		(kbd "M-t"))

; Hard-wrap/un-hard-wrap paragraph
(defconst ergoemacs-compact-uncompact-block-key		(kbd "M-q"))

;;; EMACS'S SPECIAL COMMANDS

; Cancel
(defconst ergoemacs-keyboard-quit-key			(kbd "M-n"))

; Mark point.
(defconst ergoemacs-set-mark-command-key		(kbd "M-SPC"))

(defconst ergoemacs-execute-extended-command-key	(kbd "M-a"))
(defconst ergoemacs-shell-command-key			(kbd "M-A"))

;;; WINDOW SPLITING
(defconst ergoemacs-move-cursor-next-pane-key		(kbd "M-s"))
(defconst ergoemacs-move-cursor-previous-pane-key	(kbd "M-S"))

;;; --------------------------------------------------
;;; OTHER SHORTCUTS

(defconst ergoemacs-switch-to-previous-frame-key        (kbd "M-|"))
(defconst ergoemacs-switch-to-next-frame-key            (kbd "M-\\"))

(defconst ergoemacs-query-replace-key                   (kbd "M-5"))
(defconst ergoemacs-query-replace-regexp-key            (kbd "M-%"))

(defconst ergoemacs-delete-other-windows-key            (kbd "M-3"))
(defconst ergoemacs-delete-window-key                   (kbd "M-0"))

(defconst ergoemacs-split-window-vertically-key         (kbd "M-4"))
(defconst ergoemacs-split-window-horizontally-key       (kbd "M-$"))

(defconst ergoemacs-extend-selection-key                (kbd "M-8"))
(defconst ergoemacs-select-text-in-quote-key            (kbd "M-("))
