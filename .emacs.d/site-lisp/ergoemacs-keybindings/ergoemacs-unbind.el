;-*- coding: utf-8 -*-

;; this file define keys that we want to set/unset because they are already defined by ergoemacs minor mode

(require 'edmacro)

(defconst ergoemacs-redundant-keys
  '( "C-/"
     "C-0"
     "C-1"
     "C-2"
     "C-3"
     "C-4"
     "C-5"
     "C-6"
     "C-7"
     "C-8"
     "C-9"
     "C-<next>"
     "C-<prior>"
     "C-@"
     "C-M-%"
     "C-_"
     "C-a"
     "C-b"
     "C-d"
     "C-e"
     "C-f"
     "C-j"
     "C-k"
     "C-l"
     "C-n"
     "C-o"
     "C-p"
     "C-r"
     "C-s"
     "C-t"
     "C-v"
     "C-w"
     "C-x 0"
     "C-x 1"
     "C-x 2"
     "C-x 3"
     "C-x 5 0"
     "C-x 5 2"
     "C-x C-d"
     "C-x C-f"
     "C-x C-s"
     "C-x C-w"
     "C-x d"
     "C-x h"
     "C-x o"
     "C-y"
     "C-z"
     "M--"
     "M-0"
     "M-1"
     "M-2"
     "M-3"
     "M-4"
     "M-5"
     "M-6"
     "M-7"
     "M-8"
     "M-9"
     "M-<"
     "M->"
     "M-@"
     "M-\\"
     "M-a"
     "M-b"
     "M-c"
     "M-d"
     "M-e"
     "M-f"
     "M-h"
     "M-i"
     "M-j"
     "M-k"
     "M-l"
     "M-m"
     "M-n"
     "M-o"
     "M-p"
     "M-q"
     "M-r"
     "M-s"
     "M-t"
     "M-u"
     "M-v"
     "M-w"
     "M-x"
     "M-y"
     "M-z"
     "M-{"
     "M-}"
     )
  )

;; Some exceptions we don't want to unset.
;; "C-g" 'keyboard-quit
;; "C-i" 'indent-for-tab-command
;; "C-m" 'newline-and-indent
;; "C-q" 'quote-insert
;; "C-u" 'universal-argument
;; "C-h" ; (help-map)
;; "C-x" ; (ctl-x-map)
;; "C-c" ; (prefix)
;; "M-g" ; (prefix)

(defvar ergoemacs-overridden-global-keys '()
  "Alist to store overridden keyboard shortcuts in
  `current-global-map' and other maps. Each item looks like '(MAP KEY OLD-COMMAND).")

(defun ergoemacs-unset-global-key (map key-s)
  "Sets to nil the associated command for the specified key in specified map.
It is like:

  \(define-key map (kbd key-s) nil))

But it saves the old command associated with the
specified key, so we can restore it when ergoemacs minor mode is
disabled at `ergoemacs-restore-global-keys'."
  (let (key oldcmd)
    (setq key (edmacro-parse-keys key-s))
    ;; get the old command associated with this key
    (setq oldcmd (lookup-key map key))
    ;; save that shortcut in ergoemacs-overridden-global-keys
    (if oldcmd
	(add-to-list 'ergoemacs-overridden-global-keys (cons map (cons key-s (cons oldcmd nil)))))
    ;; redefine the key in the ergoemacs-keymap
    (define-key map key nil)
    )
  )

(defun ergoemacs-unset-redundant-global-keys ()
  "Unsets redundant keyboard shortcuts that should not be used in ErgoEmacs."
  (mapc (lambda (x)
	  (ergoemacs-unset-global-key (current-global-map) x))
	ergoemacs-redundant-keys)
  )

(defun ergoemacs-restore-global-keys ()
  "Restores all keyboard shortcuts that were overwritten by `ergoemacs-unbind-global-key'."
  (mapc (lambda (x)
	  (define-key
	    (car x)
	    (edmacro-parse-keys (car (cdr x)))
	    (car (cdr (cdr x))))
	  )
	ergoemacs-overridden-global-keys)
  (setq ergoemacs-overridden-global-keys '()) ; clear the list
  )

;; Based on describe-key-briefly
(defun where-is-old-binding (&optional key)
  "Print the name of the function KEY invoked before to start ErgoEmacs minor mode."
  (interactive
   (let ((enable-disabled-menus-and-buttons t)
	 (cursor-in-echo-area t)
	 saved-yank-menu)
     (unwind-protect
	 (let (key)
	   ;; If yank-menu is empty, populate it temporarily, so that
	   ;; "Select and Paste" menu can generate a complete event.
	   (when (null (cdr yank-menu))
	     (setq saved-yank-menu (copy-sequence yank-menu))
	     (menu-bar-update-yank-menu "(any string)" nil))
	   (setq key (read-key-sequence "Describe old key (or click or menu item): "))
	   ;; If KEY is a down-event, read and discard the
	   ;; corresponding up-event.  Note that there are also
	   ;; down-events on scroll bars and mode lines: the actual
	   ;; event then is in the second element of the vector.
	   (and (vectorp key)
		(let ((last-idx (1- (length key))))
		  (and (eventp (aref key last-idx))
		       (memq 'down (event-modifiers (aref key last-idx)))))
		(read-event))
	   (list key))
       ;; Put yank-menu back as it was, if we changed it.
       (when saved-yank-menu
	 (setq yank-menu (copy-sequence saved-yank-menu))
	 (fset 'yank-menu (cons 'keymap yank-menu))))))

  (let (key-desc item-key item-cmd old-cmd)
    (setq key-desc (key-description key))
    (setq item ergoemacs-overridden-global-keys)
    (while (and item (not old-cmd))
      (setq item-key (car (cdr (car item))))
      (setq item-cmd (car (cdr (cdr (car item)))))
      (if (string= item-key key-desc)
	  (setq old-cmd item-cmd))
      (setq item (cdr item))
      )
    (if old-cmd
	(with-temp-buffer
	  (where-is old-cmd t)
	  (message "Key %s was bound to %s which is now invoked by %s"
		   key-desc old-cmd (buffer-string))
	  )
      (message "Key %s was not bound to any command" key-desc)
      )
    )
  )
