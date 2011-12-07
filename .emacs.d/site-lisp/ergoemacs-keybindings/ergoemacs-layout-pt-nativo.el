;-*- coding: utf-8 -*-
;; Shortcuts for ERGOEMACS_KEYBOARD_LAYOUT=pt-nativo
;; Keyboard Layout: PT-Nativo from http://tecladobrasileiro.com.br
;; Contributor: Xavier Pinho
;; Creation date: 2010

;;; --------------------------------------------------
;;; CURSOR MOVEMENTS

;; Single char cursor movement
(defconst ergoemacs-backward-char-key			(kbd "M-d"))
(defconst ergoemacs-forward-char-key			(kbd "M-r"))
(defconst ergoemacs-previous-line-key			(kbd "M-t"))
(defconst ergoemacs-next-line-key			(kbd "M-s"))

;; Move by word
(defconst ergoemacs-backward-word-key			(kbd "M-l"))
(defconst ergoemacs-forward-word-key			(kbd "M-c"))

;; Move by paragraph
(defconst ergoemacs-backward-paragraph-key		(kbd "M-L"))
(defconst ergoemacs-forward-paragraph-key		(kbd "M-C"))

;; Move to beginning/ending of line
(defconst ergoemacs-move-beginning-of-line-key		(kbd "M-m"))
(defconst ergoemacs-move-end-of-line-key		(kbd "M-M"))

;; Move by screen (page up/down)
(defconst ergoemacs-scroll-down-key			(kbd "M-T"))
(defconst ergoemacs-scroll-up-key			(kbd "M-S"))

;; Move to beginning/ending of file
(defconst ergoemacs-beginning-of-buffer-key		(kbd "M-D"))
(defconst ergoemacs-end-of-buffer-key			(kbd "M-R"))

;; isearch
(defconst ergoemacs-isearch-forward-key			(kbd "M-;"))
(defconst ergoemacs-isearch-backward-key		(kbd "M-:"))

(defconst ergoemacs-recenter-key			(kbd "M-p"))

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(defconst ergoemacs-delete-backward-char-key		(kbd "M-a"))
(defconst ergoemacs-delete-char-key			(kbd "M-o"))

; Delete previous/next word.
(defconst ergoemacs-backward-kill-word-key		(kbd "M-."))
(defconst ergoemacs-kill-word-key			(kbd "M-h"))

; Copy Cut Paste, Paste previous
(defconst ergoemacs-kill-region-key			(kbd "M-ç"))
(defconst ergoemacs-kill-ring-save-key			(kbd "M-j"))
(defconst ergoemacs-yank-key				(kbd "M-b"))
(defconst ergoemacs-yank-pop-key			(kbd "M-B"))
(defconst ergoemacs-copy-all-key			(kbd "M-J"))
(defconst ergoemacs-cut-all-key				(kbd "M-Ç"))

;; undo and redo
(defconst ergoemacs-redo-key				(kbd "M-Y"))
(defconst ergoemacs-undo-key				(kbd "M-y"))

; Kill line
(defconst ergoemacs-kill-line-key			(kbd "M-u"))
(defconst ergoemacs-kill-line-backward-key		(kbd "M-U"))

;;; Textual Transformation

(defconst ergoemacs-mark-paragraph-key			(kbd "M-S-SPC"))
(defconst ergoemacs-shrink-whitespaces-key		(kbd "M-;"))
(defconst ergoemacs-comment-dwim-key			(kbd "M-º"))
(defconst ergoemacs-toggle-letter-case-key		(kbd "M-/"))

; keyword completion, because Alt+Tab is used by OS
(defconst ergoemacs-call-keyword-completion-key		(kbd "M-x"))

; Hard-wrap/un-hard-wrap paragraph
(defconst ergoemacs-compact-uncompact-block-key		(kbd "M-'"))

;;; EMACS'S SPECIAL COMMANDS

; Cancel
(defconst ergoemacs-keyboard-quit-key			(kbd "M-q"))

; Mark point.
(defconst ergoemacs-set-mark-command-key		(kbd "M-SPC"))

(defconst ergoemacs-execute-extended-command-key	(kbd "M-i"))
(defconst ergoemacs-shell-command-key			(kbd "M-I"))

;;; WINDOW SPLITING
(defconst ergoemacs-move-cursor-next-pane-key		(kbd "M-e"))
(defconst ergoemacs-move-cursor-previous-pane-key	(kbd "M-E"))

;;; --------------------------------------------------
;;; OTHER SHORTCUTS

(defconst ergoemacs-switch-to-previous-frame-key        (kbd "M-+"))
(defconst ergoemacs-switch-to-next-frame-key            (kbd "M-*"))

(defconst ergoemacs-query-replace-key                   (kbd "M-5"))
(defconst ergoemacs-query-replace-regexp-key            (kbd "M-%"))

(defconst ergoemacs-delete-other-windows-key            (kbd "M-3"))
(defconst ergoemacs-delete-window-key                   (kbd "M-0"))

(defconst ergoemacs-split-window-vertically-key         (kbd "M-4"))
(defconst ergoemacs-split-window-horizontally-key       (kbd "M-$"))

(defconst ergoemacs-extend-selection-key                (kbd "M-8"))
(defconst ergoemacs-select-text-in-quote-key            (kbd "M-("))
