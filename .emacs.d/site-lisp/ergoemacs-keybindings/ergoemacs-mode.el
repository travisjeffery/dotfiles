;-*- coding: utf-8 -*-
;; ergoemacs-mode.el -- A emacs keybinding set based on ergonomics.

;; Copyright © 2007, 2008, 2009 by Xah Lee
;; Copyright © 2009, 2010 by David Capello

;; Author: Xah Lee ( http://xahlee.org/ ), David Capello ( http://www.davidcapello.com.ar/ )
;; Version: 5.3.9
;; Keywords: qwerty, dvorak, keybinding, ergonomic, colemak

;; You can redistribute this program and/or modify it under the terms
;; of the GNU General Public License as published by the Free Software
;; Foundation; either version 2, or (at your option) any later
;; version.

;;; DESCRIPTION

;; This keybinding set puts the most frequently used emacs keyboard
;; shortcuts into the most easy-to-type spots.
;;
;; For complete detail, see:
;; http://xahlee.org/emacs/ergonomic_emacs_keybinding.html

;;; INSTALL

;; See the file “_INSTALL.txt”.

;;; HISTORY

;; See the file “_HISTORY.txt”.

;;; ACKNOWLEDGMENT
;; Thanks to Nikolaj Schumacher for his implementation of extend-selection.
;; Thanks to Andreas Politz and Nikolaj Schumacher for correcting/improving implementation of toggle-letter-case.
;; Thanks to Lennart Borgman for several suggestions on code to prevent shortcuts involving shift key to start select text when CUA-mode is on.
;; Thanks to marciomazza for spotting several default bindings that should have been unbound.
;; Thanks to lwarxx for bug report on diff-mode
;; Thanks to maddin for ergoemacs-global/local-set-key functions and ergoemacs-hook-modes improvements.
;; Thanks to many users who send in comments and appreciations on this.
;; Layout contributors:
;; ergoemacs-layout-da.el Contributor: Michael Budde
;; ergoemacs-layout-dv.el Contributor: Xah Lee, David Capello
;; ergoemacs-layout-gb-dv.el Contributor: Phillip Wood
;; ergoemacs-layout-gb.el Contributor: Jorge Dias (aka theturingmachine)
;; ergoemacs-layout-it.el Contributor: David Capello, Francesco Biccari
;; ergoemacs-layout-sp.el Contributor: David Capello
;; ergoemacs-layout-sv.el Contributor: Kristian Hellquist
;; ergoemacs-layout-us.el Contributor: David Capello, Xah Lee
;; ergoemacs-layout-colemak.el Contributor: Ivan Haralamov ( postivan gmail.com ), “vockets”, Graham Poulter.
;; ergoemacs-layout-pt-nativo.el Contributor: Xavier Pinho

;;; --------------------------------------------------

;; Add this same directory to load elisp files
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))

;; Ergoemacs-keybindings version
(defconst ergoemacs-mode-version "5.3.9"
  "Ergoemacs-keybindings minor mode version number.")

;; Include extra files
(load "functions")
(load "ergoemacs-unbind")

;; Load the keyboard layout looking the ERGOEMACS_KEYBOARD_LAYOUT
;; enviroment variable (this variable is set by ErgoEmacs runner)
(defvar ergoemacs-keyboard-layout (getenv "ERGOEMACS_KEYBOARD_LAYOUT")
  "Specifies which keyboard layout to use.
This is a mirror of the environment variable ERGOEMACS_KEYBOARD_LAYOUT
Valid values are:

 “us” (US English QWERTY)
 “dv” (US-Dvorak)
 “gb” (UK)
 “gb-dv” (UK Dvorak)
 “sp” (Spanish)
 “it” (Italian)
 “sv” (Swedish)
 “da” (Danish)
 “colemak” (Ergonomic Colemak URL `http://colemak.com/')
 “pt-nativo” (Ergonomic PT-Nativo URL `http://tecladobrasileiro.com.br')"
)

(cond
 ((string= ergoemacs-keyboard-layout "us")
  (load "ergoemacs-layout-us"))
 ((or (string= ergoemacs-keyboard-layout "us_dvorak")
      (string= ergoemacs-keyboard-layout "dv"))
  (load "ergoemacs-layout-dv"))
 ((string= ergoemacs-keyboard-layout "sp")
  (load "ergoemacs-layout-sp"))
 ((or (string= ergoemacs-keyboard-layout "it")
      (string= ergoemacs-keyboard-layout "it142"))
  (load "ergoemacs-layout-it"))
 ((string= ergoemacs-keyboard-layout "gb")
  (load "ergoemacs-layout-gb"))
 ((string= ergoemacs-keyboard-layout "gb-dv")
  (load "ergoemacs-layout-gb-dv"))
 ((string= ergoemacs-keyboard-layout "sv")
  (load "ergoemacs-layout-sv"))
 ((string= ergoemacs-keyboard-layout "da")
  (load "ergoemacs-layout-da"))
 ((string= ergoemacs-keyboard-layout "colemak")
  (load "ergoemacs-layout-colemak"))
((string= ergoemacs-keyboard-layout "pt-nativo")
  (load "ergoemacs-layout-pt-nativo.el"))
 (t ; US qwerty by default
  (load "ergoemacs-layout-us"))
 )

;;; --------------------------------------------------
;;; ergoemacs-keymap

(defvar ergoemacs-keymap (make-sparse-keymap)
  "ErgoEmacs minor mode keymap.")

;; Single char cursor movement
(define-key ergoemacs-keymap ergoemacs-backward-char-key 'backward-char)
(define-key ergoemacs-keymap ergoemacs-forward-char-key 'forward-char)
(define-key ergoemacs-keymap ergoemacs-previous-line-key 'previous-line)
(define-key ergoemacs-keymap ergoemacs-next-line-key 'next-line)

;; Move by word
(define-key ergoemacs-keymap ergoemacs-backward-word-key 'backward-word)
(define-key ergoemacs-keymap ergoemacs-forward-word-key 'forward-word)

;; Move by paragraph
(define-key ergoemacs-keymap ergoemacs-backward-paragraph-key 'backward-paragraph)
(define-key ergoemacs-keymap ergoemacs-forward-paragraph-key 'forward-paragraph)

;; Move to beginning/ending of line
(define-key ergoemacs-keymap ergoemacs-move-beginning-of-line-key 'move-beginning-of-line)
(define-key ergoemacs-keymap ergoemacs-move-end-of-line-key 'move-end-of-line)

;; Move by screen (page up/down)
(define-key ergoemacs-keymap ergoemacs-scroll-down-key 'scroll-down)
(define-key ergoemacs-keymap ergoemacs-scroll-up-key 'scroll-up)

;; Move to beginning/ending of file
(define-key ergoemacs-keymap ergoemacs-beginning-of-buffer-key 'beginning-of-buffer)
(define-key ergoemacs-keymap ergoemacs-end-of-buffer-key 'end-of-buffer)

;; isearch
(define-key ergoemacs-keymap ergoemacs-isearch-forward-key 'isearch-forward)
(define-key ergoemacs-keymap ergoemacs-isearch-backward-key 'isearch-backward)

(define-key ergoemacs-keymap ergoemacs-recenter-key 'recenter-top-bottom)

;;; MAJOR EDITING COMMANDS

;; Delete previous/next char.
(define-key ergoemacs-keymap ergoemacs-delete-backward-char-key 'delete-backward-char)
(define-key ergoemacs-keymap ergoemacs-delete-char-key 'delete-char)

; Delete previous/next word.
(define-key ergoemacs-keymap ergoemacs-backward-kill-word-key 'backward-kill-word)
(define-key ergoemacs-keymap ergoemacs-kill-word-key 'kill-word)

; Copy Cut Paste, Paste previous
(define-key ergoemacs-keymap ergoemacs-kill-region-key 'kill-region)
(define-key ergoemacs-keymap ergoemacs-kill-ring-save-key 'kill-ring-save)
(define-key ergoemacs-keymap ergoemacs-yank-key 'yank)
(define-key ergoemacs-keymap ergoemacs-yank-pop-key 'yank-pop)
(define-key ergoemacs-keymap ergoemacs-copy-all-key 'copy-all)
(define-key ergoemacs-keymap ergoemacs-cut-all-key 'cut-all)

;; undo and redo
(define-key ergoemacs-keymap ergoemacs-redo-key 'redo)
(define-key ergoemacs-keymap ergoemacs-undo-key 'undo)

; Kill line
(define-key ergoemacs-keymap ergoemacs-kill-line-key 'kill-line)
(define-key ergoemacs-keymap ergoemacs-kill-line-backward-key 'kill-line-backward)

;;; Textual Transformation

(define-key ergoemacs-keymap ergoemacs-mark-paragraph-key 'mark-paragraph)
(define-key ergoemacs-keymap ergoemacs-shrink-whitespaces-key 'shrink-whitespaces)
(define-key ergoemacs-keymap ergoemacs-comment-dwim-key 'comment-dwim)
(define-key ergoemacs-keymap ergoemacs-toggle-letter-case-key 'toggle-letter-case)

; keyword completion, because Alt+Tab is used by OS
(define-key ergoemacs-keymap ergoemacs-call-keyword-completion-key 'call-keyword-completion)

; Hard-wrap/un-hard-wrap paragraph
(define-key ergoemacs-keymap ergoemacs-compact-uncompact-block-key 'compact-uncompact-block)

;;; EMACS'S SPECIAL COMMANDS

; Cancel
(define-key ergoemacs-keymap ergoemacs-keyboard-quit-key 'keyboard-quit)

; Mark point.
(define-key ergoemacs-keymap ergoemacs-set-mark-command-key 'set-mark-command)

(define-key ergoemacs-keymap ergoemacs-execute-extended-command-key 'execute-extended-command)
(define-key ergoemacs-keymap ergoemacs-shell-command-key 'shell-command)

;;; WINDOW SPLITING
(define-key ergoemacs-keymap ergoemacs-move-cursor-next-pane-key 'move-cursor-next-pane)
(define-key ergoemacs-keymap ergoemacs-move-cursor-previous-pane-key 'move-cursor-previous-pane)

;;; --------------------------------------------------
;;; STANDARD SHORTCUTS

(define-key ergoemacs-keymap (kbd "C-n") 'new-empty-buffer)
(define-key ergoemacs-keymap (kbd "C-S-n") 'make-frame-command)
(define-key ergoemacs-keymap (kbd "C-o") 'find-file)
(define-key ergoemacs-keymap (kbd "C-S-o") 'open-in-desktop)
(define-key ergoemacs-keymap (kbd "C-S-t") 'open-last-closed)
(define-key ergoemacs-keymap (kbd "C-w") 'close-current-buffer)
(define-key ergoemacs-keymap (kbd "C-s") 'save-buffer)
(define-key ergoemacs-keymap (kbd "C-S-s") 'write-file)
(define-key ergoemacs-keymap (kbd "C-p") 'print-buffer-confirm)
(define-key ergoemacs-keymap (kbd "C-a") 'mark-whole-buffer)
(define-key ergoemacs-keymap (kbd "C-S-w") 'delete-frame)

(define-key ergoemacs-keymap (kbd "C-f") 'search-forward)

(define-key ergoemacs-keymap (kbd "<delete>") 'delete-char) ; the Del key for forward delete. Needed if C-d is set to nil.

(define-key ergoemacs-keymap (kbd "C-<prior>") 'previous-user-buffer)
(define-key ergoemacs-keymap (kbd "C-<next>") 'next-user-buffer)

(define-key ergoemacs-keymap (kbd "C-S-<prior>") 'previous-emacs-buffer)
(define-key ergoemacs-keymap (kbd "C-S-<next>") 'next-emacs-buffer)

(define-key ergoemacs-keymap (kbd "M-S-<prior>") 'backward-page)
(define-key ergoemacs-keymap (kbd "M-S-<next>") 'forward-page)

(define-key ergoemacs-keymap (kbd "C-x C-b") 'ibuffer)
(define-key ergoemacs-keymap (kbd "C-h m") 'describe-major-mode)
(define-key ergoemacs-keymap (kbd "C-h o") 'where-is-old-binding)

;; Ctrl+Break is a common IDE shortcut to stop compilation/find/grep
(define-key ergoemacs-keymap (kbd "C-<pause>") 'kill-compilation)

;;; --------------------------------------------------
;;; OTHER SHORTCUTS

(define-key ergoemacs-keymap ergoemacs-switch-to-previous-frame-key 'switch-to-previous-frame)
(define-key ergoemacs-keymap ergoemacs-switch-to-next-frame-key 'switch-to-next-frame)

(define-key ergoemacs-keymap ergoemacs-query-replace-key 'query-replace)
(define-key ergoemacs-keymap ergoemacs-query-replace-regexp-key 'query-replace-regexp)

(define-key ergoemacs-keymap ergoemacs-delete-other-windows-key 'delete-other-windows)
(define-key ergoemacs-keymap ergoemacs-delete-window-key 'delete-window)

(define-key ergoemacs-keymap ergoemacs-split-window-vertically-key 'split-window-vertically)
(define-key ergoemacs-keymap ergoemacs-split-window-horizontally-key 'split-window-horizontally)

(define-key ergoemacs-keymap ergoemacs-extend-selection-key 'extend-selection)
(define-key ergoemacs-keymap ergoemacs-select-text-in-quote-key 'select-text-in-quote)

;;----------------------------------------------------------------------
;; CUA fix

(let (cuaModeState cua-mode)
(cua-mode 1) ; turn on cua-mode first so the command ergoemacs-fix-cua--pre-command-handler-1 will be able to set some symbols from cua-mode

(defun ergoemacs-fix-cua--pre-command-handler-1 ()
  "Fixes CUA minor mode so selection is highlighted only when
Shift+<special key> is used (arrows keys, home, end, pgdn, pgup, etc.)."
 (defun cua--pre-command-handler-1 ()
  ;; Cancel prefix key timeout if user enters another key.
  (when cua--prefix-override-timer
    (if (timerp cua--prefix-override-timer)
        (cancel-timer cua--prefix-override-timer))
    (setq cua--prefix-override-timer nil))

  (cond
   ;; Only symbol commands can have necessary properties
   ((not (symbolp this-command))
    nil)

   ;; Handle delete-selection property on non-movement commands
   ((not (eq (get this-command 'CUA) 'move))
    (when (and mark-active (not deactivate-mark))
      (let* ((ds (or (get this-command 'delete-selection)
                     (get this-command 'pending-delete)))
             (nc (cond
                  ((not ds) nil)
                  ((eq ds 'yank)
                   'cua-paste)
                  ((eq ds 'kill)
                   (if cua--rectangle
                       'cua-copy-rectangle
                     'cua-copy-region))
                  ((eq ds 'supersede)
                   (if cua--rectangle
                       'cua-delete-rectangle
                     'cua-delete-region))
                  (t
                   (if cua--rectangle
                       'cua-delete-rectangle ;; replace?
                     'cua-replace-region)))))
        (if nc
            (setq this-original-command this-command
                  this-command nc)))))

   ;; Handle shifted cursor keys and other movement commands.
   ;; If region is not active, region is activated if key is shifted.
   ;; If region is active, region is cancelled if key is unshifted
   ;;   (and region not started with C-SPC).
   ;; If rectangle is active, expand rectangle in specified direction and
   ;;   ignore the movement.
   ((if window-system
        ;; Shortcut for window-system, assuming that input-decode-map is empty.

        ;; ErgoEmacs patch begin ------------------
        ;;;; (memq 'shift (event-modifiers
        ;;;;               (aref (this-single-command-raw-keys) 0)))
        (and (memq 'shift (event-modifiers
                           (aref (this-single-command-raw-keys) 0)))
             ;; In this way, we expect to use CUA only with keys that
             ;; are symbols (like <left>, <next>, etc.)
             (symbolp (event-basic-type (aref (this-single-command-raw-keys) 0))))
        ;; ErgoEmacs patch end --------------------

      (or
       ;; Check if the final key-sequence was shifted.
       (memq 'shift (event-modifiers
                     (aref (this-single-command-keys) 0)))
       ;; If not, maybe the raw key-sequence was mapped by input-decode-map
       ;; to a shifted key (and then mapped down to its unshifted form).
       (let* ((keys (this-single-command-raw-keys))
              (ev (lookup-key input-decode-map keys)))
         (or (and (vector ev) (memq 'shift (event-modifiers (aref ev 0))))
             ;; Or maybe, the raw key-sequence was not an escape sequence
             ;; and was shifted (and then mapped down to its unshifted form).
             (memq 'shift (event-modifiers (aref keys 0)))))))
    (unless mark-active
      (push-mark-command nil t))
    (setq cua--last-region-shifted t)
    (setq cua--explicit-region-start nil))

   ;; Set mark if user explicitly said to do so
   ((or cua--explicit-region-start cua--rectangle)
    (unless mark-active
      (push-mark-command nil nil)))

   ;; Else clear mark after this command.
   (t
    ;; If we set mark-active to nil here, the region highlight will not be
    ;; removed by the direct_output_ commands.
    (setq deactivate-mark t)))

  ;; Detect extension of rectangles by mouse or other movement
  (setq cua--buffer-and-point-before-command
        (if cua--rectangle (cons (current-buffer) (point)))))
 )
(if cuaModeState (cua-mode 1) (cua-mode 0))
  )

;;----------------------------------------------------------------------
;; ErgoEmacs hooks

(defun ergoemacs-minibuffer-setup-hook ()
  "Hook for minibuffer to move through history with previous-line and next-line keys."

  (defvar ergoemacs-minibuffer-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-minibuffer-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-minibuffer-keymap ergoemacs-previous-line-key 'previous-history-element)
  (define-key ergoemacs-minibuffer-keymap ergoemacs-next-line-key 'next-history-element)

  (define-key ergoemacs-minibuffer-keymap (kbd "<f11>") 'previous-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "<f12>") 'next-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key ergoemacs-minibuffer-keymap (kbd "S-<f12>") 'next-matching-history-element)

  ;; The ergoemacs-mode keymap could already be in the minor-mode-overriding map
  ;; (e.g. iswitchb or ido hooks were executed)
  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-minibuffer-keymap)
	       nil (lambda (x y)
		     (equal (car y) (car x))))
  )

(defun ergoemacs-isearch-hook ()
  "Hook for `isearch-mode-hook' so ergoemacs keybindings are not lost."

  ;; TODO restore these keys! (it is not necessary, when the
  ;; ergoemacs-isearch-hook is removed from isearch-mode-hook)

  (define-key isearch-mode-map (kbd "M-p") 'nil) ; was isearch-ring-retreat
  (define-key isearch-mode-map (kbd "M-n") 'nil) ; was isearch-ring-advance
  (define-key isearch-mode-map (kbd "M-y") 'nil) ; was isearch-yank-kill
  (define-key isearch-mode-map (kbd "M-c") 'nil) ; was isearch-toggle-case-fold
  (define-key isearch-mode-map (kbd "M-r") 'nil) ; was isearch-toggle-regexp
  (define-key isearch-mode-map (kbd "M-e") 'nil) ; was isearch-edit-string

  (define-key isearch-mode-map ergoemacs-keyboard-quit-key 'isearch-abort)
  (define-key isearch-mode-map ergoemacs-isearch-forward-key 'isearch-repeat-forward)
  (define-key isearch-mode-map ergoemacs-isearch-backward-key 'isearch-repeat-backward)
  (define-key isearch-mode-map ergoemacs-recenter-key 'recenter)
  (define-key isearch-mode-map ergoemacs-yank-key 'isearch-yank-kill)

  ;; CUA paste key is isearch-yank-kill in isearch mode
  (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)

  ;; isearch-other-control-char sends the key to the original buffer and cancels isearch
  (define-key isearch-mode-map ergoemacs-kill-ring-save-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-kill-word-key 'isearch-other-control-char)
  (define-key isearch-mode-map ergoemacs-backward-kill-word-key 'isearch-other-control-char)

  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  )

;; Hook for interpreters
(defun ergoemacs-comint-hook ()
  "Hook for `comint-mode-hook'."

  (define-key comint-mode-map (kbd "<f11>") 'comint-previous-input)
  (define-key comint-mode-map (kbd "<f12>") 'comint-next-input)
  (define-key comint-mode-map (kbd "S-<f11>") 'comint-previous-matching-input)
  (define-key comint-mode-map (kbd "S-<f12>") 'comint-next-matching-input)
  )

;; Log edit mode
(defun ergoemacs-log-edit-hook ()
  "Hook for `log-edit-mode-hook'."

  (define-key log-edit-mode-map (kbd "<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "<f12>") 'log-edit-next-comment)
  (define-key log-edit-mode-map (kbd "S-<f11>") 'log-edit-previous-comment)
  (define-key log-edit-mode-map (kbd "S-<f12>") 'log-edit-next-comment)
  )

(defun ergoemacs-eshell-hook ()
  "Hook for `eshell-mode-hook', to redefine some ErgoEmacs keys so they are more useful."

  ;; Redefining ergoemacs-move-beginning-of-line-key to eshell-bol in eshell-mode-map
  ;; does not work, we have to use minor-mode-overriding-map-alist in this case
  (defvar ergoemacs-eshell-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-eshell-keymap ergoemacs-move-beginning-of-line-key 'eshell-bol)
  (define-key ergoemacs-eshell-keymap (kbd "<home>") 'eshell-bol)
  (define-key ergoemacs-eshell-keymap (kbd "<f11>") 'eshell-previous-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "<f12>") 'eshell-next-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "S-<f11>") 'eshell-previous-matching-input-from-input)
  (define-key ergoemacs-eshell-keymap (kbd "S-<f12>") 'eshell-next-matching-input-from-input)

  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-eshell-keymap))
  )

(defun ergoemacs-iswitchb-hook ()
  "Hooks for `iswitchb-minibuffer-setup-hook'."

  (defvar ergoemacs-iswitchb-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-iswitchb-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-backward-key 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap ergoemacs-isearch-forward-key 'iswitchb-next-match)

  (define-key ergoemacs-iswitchb-keymap (kbd "<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "<f12>") 'iswitchb-next-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f11>") 'iswitchb-prev-match)
  (define-key ergoemacs-iswitchb-keymap (kbd "S-<f12>") 'iswitchb-next-match)

  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-iswitchb-keymap))
  )

(defun ergoemacs-ido-minibuffer-setup-hook ()
  "Hook for `ido-minibuffer-setup-hook'."

  (defvar ergoemacs-ido-keymap (copy-keymap ergoemacs-keymap))

  (define-key ergoemacs-ido-keymap ergoemacs-keyboard-quit-key 'minibuffer-keyboard-quit)
  (define-key ergoemacs-ido-keymap ergoemacs-forward-char-key 'ido-next-match)
  (define-key ergoemacs-ido-keymap ergoemacs-backward-char-key 'ido-prev-match)
  (define-key ergoemacs-ido-keymap ergoemacs-previous-line-key 'ido-next-match-dir)
  (define-key ergoemacs-ido-keymap ergoemacs-next-line-key 'ido-prev-match-dir)

  (define-key ergoemacs-ido-keymap (kbd "<f11>") 'previous-history-element)
  (define-key ergoemacs-ido-keymap (kbd "<f12>") 'next-history-element)
  (define-key ergoemacs-ido-keymap (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key ergoemacs-ido-keymap (kbd "S-<f12>") 'next-matching-history-element)

  (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-ido-keymap))
  )

(defun ergoemacs-auto-complete-mode-hook ()
  "Hook for `auto-complete-mode-hook'.

When the `auto-complete-mode' is on, and when a word completion
is in process, Ctrl+s does `ac-isearch'.
This fixes it."

(define-key ac-completing-map ergoemacs-isearch-forward-key 'ac-isearch)
(define-key ac-completing-map (kbd "C-s") nil)
  )

(defvar ergoemacs-hook-list (list)
  "List of hook and hook-function pairs.")

(defun ergoemacs-add-hook (hook hook-function)
  "Adds a pair of hook and hook-function to the list
ergoemacs hooks."
  (add-to-list 'ergoemacs-hook-list (cons hook hook-function)))

(ergoemacs-add-hook 'isearch-mode-hook 'ergoemacs-isearch-hook)
(ergoemacs-add-hook 'comint-mode-hook 'ergoemacs-comint-hook)
(ergoemacs-add-hook 'log-edit-mode-hook 'ergoemacs-log-edit-hook)
(ergoemacs-add-hook 'eshell-mode-hook 'ergoemacs-eshell-hook)
(ergoemacs-add-hook 'minibuffer-setup-hook 'ergoemacs-minibuffer-setup-hook)
(ergoemacs-add-hook 'iswitchb-minibuffer-setup-hook 'ergoemacs-iswitchb-hook)
(ergoemacs-add-hook 'ido-minibuffer-setup-hook 'ergoemacs-ido-minibuffer-setup-hook)
(ergoemacs-add-hook 'auto-complete-mode-hook 'ergoemacs-auto-complete-mode-hook)

(defun ergoemacs-hook-modes ()
  "Installs/Removes ErgoEmacs minor mode hooks from major modes
depending the state of `ergoemacs-mode' variable.  If the mode
is being initialized, some global keybindings in current-global-map
will change."

  (let ((modify-hook (if ergoemacs-mode 'add-hook 'remove-hook))
	(modify-advice (if ergoemacs-mode 'ad-enable-advice 'ad-disable-advice)))

    ;; Fix CUA
    (if ergoemacs-mode
        (ergoemacs-fix-cua--pre-command-handler-1))

    ;; when ergoemacs-mode is on, activate hooks and unset global keys, else do inverse
    (if (and ergoemacs-mode (not (equal ergoemacs-mode 0)))
	(progn
	  (ergoemacs-unset-redundant-global-keys)

	  ;; alt+n is the new "Quit" in query-replace-map
	  (ergoemacs-unset-global-key query-replace-map "\e")
	  (define-key query-replace-map ergoemacs-keyboard-quit-key 'exit-prefix))
      ;; if ergoemacs was disabled: restore original keys
      (ergoemacs-restore-global-keys))

    ;; install the mode-hooks
    (dolist (hook ergoemacs-hook-list)
      (funcall modify-hook (car hook) (cdr hook)))

    ;; enable advices
    (funcall modify-advice 'global-set-key 'around 'ergoemacs-global-set-key-advice)
    (funcall modify-advice 'global-unset-key 'around 'ergoemacs-global-unset-key-advice)
    (funcall modify-advice 'local-set-key 'around 'ergoemacs-local-set-key-advice)
    (funcall modify-advice 'local-unset-key 'around 'ergoemacs-local-unset-key-advice)

    ;; update advices
    (ad-activate 'global-set-key)
    (ad-activate 'global-unset-key)
    (ad-activate 'local-set-key)
    (ad-activate 'local-unset-key)
    )
  )

;;----------------------------------------------------------------------
;; ErgoEmacs replacements for local- and global-set-key

(defun ergoemacs-global-set-key (key command)
  "Set a key in the ergoemacs-keymap, thus
making it globally active. This allow to redefine
any key unbound or claimed by ergoemacs."
  (interactive)
  (define-key ergoemacs-keymap key command))

(defun ergoemacs-global-unset-key (key)
  "Removes a key from the ergoemacs-keymap."
  (interactive)
  (ergoemacs-global-set-key key nil))

(defvar ergoemacs-local-keymap nil
  "Local ergoemacs keymap")
(make-variable-buffer-local 'ergoemacs-local-keymap)

(defun ergoemacs-local-set-key (key command)
  "Set a key in the ergoemacs local map."
  ;; install keymap if not already installed
  (interactive)
  (progn
    (unless ergoemacs-local-keymap
      (setq ergoemacs-local-keymap (copy-keymap ergoemacs-keymap))
      (add-to-list 'minor-mode-overriding-map-alist (cons 'ergoemacs-mode ergoemacs-local-keymap)))
    ;; add key
    (define-key ergoemacs-local-keymap key command)))

(defun ergoemacs-local-unset-key (key)
  "Unset a key in the ergoemacs local map."
  (ergoemacs-local-set-key key nil))

;;----------------------------------------------------------------------
;; ErgoEmacs advices for local- and global-set-key

(defadvice global-set-key (around ergoemacs-global-set-key-advice (key command))
  "This let you use global-set-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-global-set-key key command)
    ad-do-it))

(defadvice global-unset-key (around ergoemacs-global-unset-key-advice (key))
  "This let you use global-unset-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-global-unset-key key)
    ad-do-it))

(defadvice local-set-key (around ergoemacs-local-set-key-advice (key command))
  "This let you use local-set-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-set-key key command)
    ad-do-it))

(defadvice local-unset-key (around ergoemacs-local-unset-key-advice (key))
  "This let you use local-unset-key as usual when ergoemacs-mode is enabled."
  (if (fboundp 'ergoemacs-mode)
      (ergoemacs-local-unset-key key)
    ad-do-it))

;;----------------------------------------------------------------------
;; ErgoEmacs minor mode

(define-minor-mode ergoemacs-mode
  "Toggle ergoemacs keybinding mode.
This minor mode changes your emacs keybindings.
Without argument, toggles the minor mode.
If optional argument is 1, turn it on.
If optional argument is 0, turn it off.
Argument of t or nil should not be used.
For full documentation, see:
URL `http://xahlee.org/emacs/ergonomic_emacs_keybinding.html'

If you turned on by mistake, the shortcut to call execute-extended-command is M-a."
  nil
  :lighter " ErgoEmacs"	;; TODO this should be nil (it is for testing purposes)
  :global t
  :keymap ergoemacs-keymap

  (ergoemacs-hook-modes)
  )

(provide 'ergoemacs-mode)
