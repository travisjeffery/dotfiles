;;; tj.el --- 
;; 
;; Filename: tj.el
;; Description: 
;; Author: Travis Jeffery
;; Maintainer: 
;; Created: Wed Jan  6 21:12:10 2021 (-0500)
;; Version: 
;; Package-Requires: ((projectile) (dired) (s) (markdown-mode))
;; Last-Updated: 
;;           By: 
;;     Update #: 0
;; URL: 
;; Doc URL: 
;; Keywords: 
;; Compatibility: 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Commentary: 
;; 
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Change Log:
;; 
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;; Code:

(require 'dired)
(require 's)

(setq
 initial-major-mode 'fundamental-mode
 tramp-default-method "ssh"
 ;; show all buffers, otherwise, some can be hidden under C-x b
 buffers-menu-max-size nil
 debugger-stack-frame-as-list t
 user-full-name "Travis Jeffery"
 user-mail-address "tj@travisjeffery.com"
 ;; rescan the buffer contents to update jump targets
 imenu-auto-rescan t
 ;; config changes made through the customize UI will be stored here
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 ;; 'comment-indent-new-line continues comments
 comment-multi-line t
 ;; Save whatever's in the system clipboard before replcaing it with the Emacs' txt.
 save-interprogram-paste-before-kill t
 read-process-output-max (* 1024 1024)
 ;; Always load newest byte code
 load-prefer-newer t
 ;; warn when opening files bigger than 100MB
 large-file-warning-threshold 100000000
 sentence-end-double-space nil

 ;; disable the annoying bell ring
 ring-bell-function 'ignore

 ;; disable startup screen
 inhibit-startup-screen t

 eldoc-idle-delay 0

 ;; nice scrolling
 scroll-margin 0
 scroll-conservatively 100000
 scroll-preserve-screen-position 1

 ;; more useful frame title, that show either a file or a
 ;; buffer name (if the buffer isn't visiting a file)
 frame-title-format
 '((:eval
    (if (buffer-file-name)
        (abbreviate-file-name (buffer-file-name))
      "%b")))


 ;; Emacs modes typically provide a standard means to change the
 ;; indentation width -- eg. c-basic-offset: use that to adjust your
 ;; personal indentation width, while maintaining the style (and
 ;; meaning) of any files you load.
 indent-tabs-mode nil ;; don't use tabs to indent
 tab-width 8 ;; but maintain correct appearance

 ;; Newline at end of file
 require-final-newline t

 ;; hippie expand is dabbrev expand on steroids
 hippie-expand-try-functions-list
 '(try-expand-dabbrev
   try-expand-dabbrev-all-buffers
   try-expand-dabbrev-from-kill
   try-complete-file-name-partially
   try-complete-file-name
   try-expand-all-abbrevs
   try-expand-list
   try-expand-line
   try-complete-lisp-symbol-partially
   try-complete-lisp-symbol)

 ;; store all backup and autosave files in the tmp dir
 backup-directory-alist `((".*" . ,temporary-file-directory))
 auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
 markdown-command "multimarkdown"
 gc-cons-threshold 300000000)

;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))

(setq-default
 indent-tabs-mode nil
 fill-column 100
 ;; open help, ack, etc. in the same window
 ;; same-window-regexps '(".")
 same-window-regexps nil)

;; handle long lines
(global-so-long-mode t)

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; keep cursor static
(blink-cursor-mode -1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; delete the selection with a keypress
(delete-selection-mode t)

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

(defun tj-base64-encode-region-no-break ()
  (interactive)
  (base64-encode-region (mark) (point) t))

(defun tj-copy-line-as-kill (&optional arg)
  "Save the current line as if killed, but don't kill it. If ARG set, then save ARG lines."
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn
       (if arg
           (forward-visible-line arg)
         (end-of-visible-line))
       (point)))))

(defun tj-gist (&optional arg)
  "Gist current buffer. If ARG set, then post to public."
  (interactive "P")
  (let* ((cmd (list "gh gist create" (buffer-file-name)))
         (cmd
          (if arg
              (nconc cmd (list "--public"))
            cmd))
         (cmd (string-join cmd " ")))
    (async-shell-command cmd)))

(defun tj-goland ()
  "Open current project in Goland."
  (interactive)
  (async-shell-command (format "goland %s" (projectile-project-root))))

(defun tj-keyboard-quit-dwim ()
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'tj-keyboard-quit-dwim)

(defun tj-newline-and-indent-up ()
  "Open a new line above the current line."
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun tj-reload-dir-locals-for-current-buffer ()
  "Reload dir locals for the current buffer."
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun tj-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
  (interactive)
  (let ((dir (projectile-project-root)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (tj-reload-dir-locals-for-current-buffer)))))

(defun tj-what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor."
  (interactive)
  (let (input
        tmp
        p1
        p2)
    (save-excursion
      (re-search-backward "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point))
      (re-search-forward "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point)))

    (setq input (buffer-substring-no-properties p1 p2))

    (let ((case-fold-search nil))
      (setq tmp (replace-regexp-in-string "^0x" "" input))
      (setq tmp (replace-regexp-in-string "^#x" "" tmp))
      (setq tmp (replace-regexp-in-string "^#" "" tmp)))

    (message "Hex %s is %d" tmp (string-to-number tmp 16))))

(defun tj-toggle-window-split ()
  "Toggle window split."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd
              (not
               (and (<= (car this-win-edges) (car next-win-edges))
                    (<= (cadr this-win-edges) (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges) (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd
              (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd
              (other-window 1))))))

(defun tj-newline-and-indent ()
  "Newline under the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun tj-eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its VALUE."
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun tj-comment-line ()
  "Comment the current line or region."
  (interactive)
  (call-interactively #'comment-line)
  (unless (region-active-p)
    (forward-line -1)))

(defun tj-kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (remove-if-not 'buffer-file-name (buffer-list)))))

(defun tj-kill-other-buffer ()
  "Kill the other window's buffer."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))

(defun tj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line."
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display
           nil
         (or col 1))))))

;; global keybinds
(cl-loop
 for (key . fn) in
 '(("M-;" . tj-comment-line)
   ("M-g M-c" . switch-to-completions)
   ("C-RET" . other-window)
   ("C-x C-S-f" . find-file-at-point)
   ("C-z" . delete-other-windows)
   ("C-c q" . tj-kill-other-buffer)
   ;; use hippie-expand instead of dabbrev
   ("C-/" . undo)
   ("M-/" . hippie-expand)
   ;; replace buffer-menu with ibuffer
   ("C-x C-b" . bufler)
   ("C-h A" . apropos)
   ;; align code in a pretty way
   ("C-x \\" . align-regexp)
   ("C-h C-f" . find-function)
   ;; misc useful keybindings
   ("C-c <" . tj-insert-open-and-close-tag))
 do (global-set-key (kbd key) fn))

(defun tj-multi-line-to-one-line (beg end)
  "Convert the lines between BEG and END into one line and copy \
it in to the kill ring, when 'transient-mark-mode' is enabled. If
no region is active then only the current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.

Current position is preserved."
  (interactive "r")
  (let (str
        (orig-pos (point-marker)))
    (save-restriction
      (widen)
      (when (and transient-mark-mode (not (use-region-p)))
        (setq
         beg (line-beginning-position)
         end (line-beginning-position 2)))

      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (unless (= (point) (line-beginning-position))
        (setq end (line-beginning-position 2)))

      (goto-char beg)
      (setq str
            (replace-regexp-in-string
             "[ \t]*\n" ""
             (replace-regexp-in-string "^[ \t]+" "" (buffer-substring-no-properties beg end))))
      ;; (message "str=%s" str)
      (kill-new str)
      (goto-char orig-pos))))

(defun tj-spongebob (start end)
  "Convert string from START to END to SpOnGeBoB meme."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((upcase? (not (s-uppercase? (char-to-string (char-after))))))
      (while (not (eq end (point)))
        (setq upcase? (not upcase?))
        (let* ((curchar (char-after))
               (newchar
                (if upcase?
                    (upcase curchar)
                  (downcase curchar))))
          (delete-char 1)
          (insert-char newchar))))))

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(defun tj-format-sql-region (beg end)
  "Format SQL in region from BEG to END."
  (interactive "r")
  (save-excursion (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun tj-format-sql-buffer ()
  "Format SQL in buffer."
  (interactive)
  (tj-format-sql-region (point-min) (point-max)))

(defun tj-thesaurus ()
  "Browse thesaurus."
  (interactive)
  (tj--browse-word "https://www.merriam-webster.com/thesaurus/%s"))

(defun tj-dictionary ()
  "Browse dictionary."
  (interactive)
  (tj--browse-word "https://merriam-webster.com/dictionary/%s"))

(defun tj--browse-word (url)
  (let ((word
         (or (and (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end)))
             (read-string "Word: "))))
    (browse-url (format url word))))

(defun tj-sql-mode-hook ()
  (add-hook 'after-save-hook 'tj-format-sql-buffer nil t))
;; (add-hook 'sql-mode-hook 'tj-sql-mode-hook)
(remove-hook 'sql-mode-hook 'tj-sql-mode-hook)

(defun tj-prog-mode-hook ()
  (setq font-lock-maximum-decoration 1)
  (font-lock-mode -1))
(add-hook 'prog-mode-hook 'tj-prog-mode-hook)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(setq-default grep-command "rg --no-heading")

;; improve find file at point to handle line numbers
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let*
      ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
       (name
        (or (condition-case nil
                (and
                 (not (string-match "//" string)) ; foo.com://bar
                 (substitute-in-file-name string))
              (error nil))
            string))
       (line-number-string
        (and (string-match ":[0-9]+" name) (substring name (1+ (match-beginning 0)) (match-end 0))))
       (line-number (and line-number-string (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice ffap-guesser (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let*
      ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
       (name
        (or (condition-case nil
                (and
                 (not (string-match "//" string)) ; foo.com://bar
                 (substitute-in-file-name string))
              (error nil))
            string))
       (line-number-string
        (and (string-match ":[0-9]+" name) (substring name (1+ (match-beginning 0)) (match-end 0))))
       (line-number (and line-number-string (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
        (setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defun tj-commas-to-new-lines (start end)
  "Convert commas to commas with new-lines from START to END.
Useful to take a long list of arguments on one-line and split
them across multiple lines."
  (interactive "r")
  (let* ((in (buffer-substring-no-properties start end))
         (out (s-replace ", " ",\n" in)))
    (save-excursion
      (delete-region start end)
      (insert out))))

(defun tj-wrap-with-tags ()
  "Generates an open and close HTML snippet using the current word."
  (interactive)
  (let ((body (buffer-substring (region-beginning) (region-end))))
    (goto-char (region-beginning))
    (delete-char (string-width body))
    (yas-expand-snippet
     (concat
      "<${1:tag}$2>"
      body
      "</${1:$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text)"
      "(match-string 0 yas-text))}>"))))
;;(define-key markdown-mode-map (kbd "C-c C-w") 'tj-wrap-with-tags)

(defun tj-insert-open-and-close-tag ()
  "Generates an open and close HTML snippet using the current word."
  (interactive)
  (let ((inserting-new-tag nil))
    (if (looking-back "[-A-Za-z0-9:_]")
        (progn
          (set-mark-command nil)
          (while (looking-back "[-A-Za-z0-9:_]")
            (backward-char)))
      (setq inserting-new-tag t)
      (set-mark-command nil)
      (insert "p")
      (exchange-point-and-mark))
    (let ((tag (buffer-substring (region-beginning) (region-end))))
      (delete-char (string-width tag))
      (cond
       ((string-match "\\`[bh]r\\'" tag)
        (insert (concat "<" tag ">")))
       ((string-match
         (concat
          "\\`\\(?:img\\|meta\\|link\\|" "input\\|base\\|area\\|col\\|" "frame\\|param\\)\\'")
         tag)
        (yas-expand-snippet (concat "<" tag " $1>$0")))
       (t
        (yas-expand-snippet
         (if inserting-new-tag
             (concat
              "<${1:"
              tag
              "}>$0</${1:"
              "$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text) "
              "(match-string 0 yas-text))}>")
           (concat "<" tag "$1>$0</" tag ">"))))))))
;;(define-key markdown-mode-map (kbd "C-c <") 'tj-insert-open-and-close-tag)

(add-hook 'focus-out-hook 'garbage-collect)

(defun tj-prowritingaid-to-markdown-format ()
  "Fix quotes after copying from ProWritingAid."
  (interactive)
  (let ((replacements '(("“" . "\"") ("”" . "\"") ("’" . "'") ("‘" . "'") (" " . " "))))
    (cl-loop
     for (key . value) in replacements do
     (progn
       (goto-char 0)
       (replace-string key value)))))

(defun tj-remove-prag-prog-code-tags ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (replace-regexp
     "^.*// END.*
"
     ""))

  (save-excursion
    (goto-char 0)
    (replace-regexp
     "^.*// START.*
"
     "")))

(defun tj-arrayify (beg end)
  "Wrap each line from BEG to END in quotes and join them in a line."
  (interactive "r")
  (replace-region-contents
   beg end
   (lambda ()
     (->
      (buffer-substring-no-properties beg end)
      (split-string "\n")
      (->> (remove "") (cl-mapcar (lambda (x) (format "\"\%s\"" x))))
      (string-join ", "))))
  (end-of-line))

(defun tj-browse-urls (beg end)
  "Browse the URL on every line between BEG and END."
  (interactive "r")
  (goto-char beg)
  (while (< (point) end)
    (beginning-of-line)
    (browse-url-at-point)
    (forward-line)))

(defun tj-yank-prepend (space)
  (interactive "sSpace: ")
  (tj-yank-rectangle t space))

(defun tj-yank-append (space)
  (interactive "sSpace: ")
  (tj-yank-rectangle nil space))

(defun tj-pull-request ()
  "Create a pull request."
  (interactive)
  (condition-case nil (call-interactively 'forge-create-pullreq)))

(defun tj-yank-rectangle (prepend space)
  (interactive)
  (save-excursion
    (let ((lines (split-string (current-kill 0) "\n")))
      (dolist (line lines)
        (goto-char
         (if prepend
             (line-beginning-position)
           (line-end-position)))
        (unless prepend
          (insert space))
        (insert line)
        (if prepend
            (insert space))
        (unless (zerop (forward-line))
          (insert "\n"))))))

(defun tj-revert-all-file-buffers ()
  "Refresh all open file buffers without confirmation.
Buffers in modified (not yet saved) state in Emacs will not be reverted. They
will be reverted though if they were modified outside Emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(defun tj-kill-file-name ()
  "Put the current file name on the clipboard."
  (interactive)
  (let ((filename
         (if (equal major-mode 'dired-mode)
             default-directory
           (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defadvice backward-kill-word (around fix activate)
  (cl-flet ((kill-region (b e) (delete-region b e))) ad-do-it))

(defun tj-fill-paragraph (&optional arg)
  "When called with prefix argument ARG call `fill-paragraph'.
Otherwise split the current paragraph into one sentence per line."
  (interactive "P")
  (if (not arg)
      (save-excursion
        (let ((fill-column 12345678)) ;; relies on dynamic binding
          (fill-paragraph) ;; this will not work correctly if the paragraph is
          ;; longer than 12345678 characters (in which case the
          ;; file must be at least 12MB long. This is unlikely.)
          (let ((end
                 (save-excursion
                   (forward-paragraph 1)
                   (backward-sentence)
                   (point-marker)))) ;; remember where to stop
            (beginning-of-line)
            (while (progn
                     (forward-sentence)
                     (<= (point) (marker-position end)))
              (just-one-space) ;; leaves only one space, point is after it
              (delete-char -1) ;; delete the space
              (newline) ;; and insert a newline
              ))))
    ;; otherwise do ordinary fill paragraph
    (fill-paragraph P)))

(defun tj-apply-function-to-region (fn)
  "Apply function FN to region."
  (interactive "XFunction to apply to region: ")
  (save-excursion
    (let* ((beg (region-beginning))
           (end (region-end))
           (resulting-text (funcall fn (buffer-substring-no-properties beg end))))
      (kill-region beg end)
      (insert resulting-text))))

(defun tj-vterm ()
  "Switch to current vterm buffer if exists or create and switch otherwise."
  (interactive)
  (if (get-buffer vterm-buffer-name)
      (switch-to-buffer vterm-buffer-name)
    (vterm)))

(defun tj-find-duplicate-lines ()
  "Show all duplicate lines in the current buffer."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((lines (make-hash-table :test 'equal)))
      (while (not (eobp))
        (when-let* ((line
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
                    (count (gethash line lines 0))
                    (_ (not (string-empty-p line))))
          (puthash line (+ count 1) lines))
        (forward-line))
      (when-let ((lines
                  (cl-loop
                   for
                   line
                   being
                   the
                   hash-keys
                   of
                   lines
                   using
                   (hash-values count)
                   when
                   (> count 1)
                   collect
                   (format "^%s$" (regexp-quote line))))
                 (empty (length lines)))
        (occur (format "\\(%s\\)" (string-join lines "\\|")))))))

(defun tj-font-size ()
  "Font size for this computer."
  (interactive)
  (if (equal (window-system) 'ns)
      14
    11))

(defun tj-zoom-reset ()
  "Reset font size."
  (interactive)
  (set-face-attribute 'default nil :height (* 10 (tj-font-size))))

(defun tj-zoom-in ()
  "Increase font size by 25 points."
  (interactive)
  (set-face-attribute 'default nil :height (+ (face-attribute 'default :height) 25)))

(defun tj-zoom-out ()
  "Decrease font size by 10 points."
  (interactive)
  (set-face-attribute 'default nil :height (- (face-attribute 'default :height) 25)))

(global-set-key (kbd "C-x C-+") 'tj-zoom-in)
(global-set-key (kbd "C-x C--") 'tj-zoom-out)
(global-set-key (kbd "C-x C-0") 'tj-zoom-reset)

(defun tj-set-face (face style)
  "Reset a FACE and make it inherit STYLE."
  (set-face-attribute face nil
                      :foreground 'unspecified
                      :background 'unspecified
                      :family 'unspecified
                      :slant 'unspecified
                      :weight 'unspecified
                      :height 'unspecified
                      :underline 'unspecified
                      :overline 'unspecified
                      :box 'unspecified
                      :inherit style))

(defun tj-theme ()
  (interactive)

  (when (display-graphic-p)
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1))

  (defgroup tj-theme nil
    "Faces for tj-theme."
    :prefix "face-")

  (defface face-critical nil
    "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
    :group 'tj-theme)

  (defface face-popout nil
    "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
    :group 'tj-theme)

  (defface face-strong nil
    "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
    :group 'tj-theme)

  (defface face-salient nil
    "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."

    :group 'tj-theme)

  (defface face-faded nil
    "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
    :group 'tj-theme)

  (defface face-subtle nil
    "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
    :group 'tj-theme)

  (setq frame-background-mode 'light)
  (set-background-color "#ffffff")
  (set-foreground-color "#333333")
  (set-face-attribute 'default nil
                      :foreground (face-foreground 'default)
                      :background (face-background 'default))
  (set-face-attribute 'face-critical nil :foreground "#ffffff" :background "#ff6347")
  (set-face-attribute 'face-popout nil :foreground "#333333")
  (set-face-attribute 'face-strong nil :foreground "#333333" :weight 'regular)
  (set-face-attribute 'face-salient nil :foreground "#00008b" :weight 'light)
  (set-face-attribute 'face-faded nil :foreground "#999999" :weight 'light)
  (set-face-attribute 'face-subtle nil :background "#f0f0f0")

  (set-face-attribute 'mode-line nil
                      :background (face-background 'default)
                      :box '(:line-width 1 :color "#999999"))

  (set-face-attribute 'mode-line-inactive nil
                      :background (face-background 'default)
                      :box '(:line-width 1 :color "#999999"))

  ;; Structural
  (tj-set-face 'bold 'face-strong)
  (tj-set-face 'italic 'face-faded)
  (tj-set-face 'bold-italic 'face-strong)
  (tj-set-face 'region 'face-subtle)
  (tj-set-face 'highlight 'face-subtle)
  (tj-set-face 'fixed-pitch 'default)
  (tj-set-face 'fixed-pitch-serif 'default)
  (tj-set-face 'variable-pitch 'default)
  (tj-set-face 'cursor 'default)

  ;; Semantic
  (tj-set-face 'shadow 'face-faded)
  (tj-set-face 'success 'face-salient)
  (tj-set-face 'warning 'face-popout)
  (tj-set-face 'error 'face-critical)

  ;; General
  (tj-set-face 'buffer-menu-buffer 'face-strong)
  (tj-set-face 'minibuffer-prompt 'face-strong)
  (tj-set-face 'link 'face-salient)
  (tj-set-face 'fringe 'face-faded)
  (tj-set-face 'isearch 'face-strong)
  (tj-set-face 'isearch-fail 'face-faded)
  (tj-set-face 'lazy-highlight 'face-subtle)
  (tj-set-face 'trailing-whitespace 'face-subtle)
  (tj-set-face 'show-paren-match 'face-popout)
  (tj-set-face 'show-paren-mismatch 'face-normal)
  (set-face-attribute 'tooltip nil :height 0.85)

  ;; Programmation mode
  (tj-set-face 'font-lock-comment-face 'face-faded)
  (tj-set-face 'font-lock-doc-face 'face-faded)
  (tj-set-face 'font-lock-string-face 'face-popout)
  (tj-set-face 'font-lock-constant-face 'face-salient)
  (tj-set-face 'font-lock-warning-face 'face-popout)
  (tj-set-face 'font-lock-function-name-face 'face-strong)
  (tj-set-face 'font-lock-variable-name-face 'face-strong)
  (tj-set-face 'font-lock-builtin-face 'face-salient)
  (tj-set-face 'font-lock-type-face 'face-salient)
  (tj-set-face 'font-lock-keyword-face 'face-salient)

  ;; Documentation
  (with-eval-after-load 'info
    (tj-set-face 'info-menu-header 'face-strong)
    (tj-set-face 'info-header-node 'face-normal)
    (tj-set-face 'Info-quoted 'face-faded)
    (tj-set-face 'info-title-1 'face-strong)
    (tj-set-face 'info-title-2 'face-strong)
    (tj-set-face 'info-title-3 'face-strong)
    (tj-set-face 'info-title-4 'face-strong))

  ;; Message
  (with-eval-after-load 'message
    (tj-set-face 'message-cited-text 'face-faded)
    (tj-set-face 'message-header-cc 'default)
    (tj-set-face 'message-header-name 'face-strong)
    (tj-set-face 'message-header-newsgroups 'default)
    (tj-set-face 'message-header-other 'default)
    (tj-set-face 'message-header-subject 'face-salient)
    (tj-set-face 'message-header-to 'face-salient)
    (tj-set-face 'message-header-xheader 'default)
    (tj-set-face 'message-mml 'face-popout)
    (tj-set-face 'message-separator 'face-faded))

  ;; Outline
  (with-eval-after-load 'outline
    (tj-set-face 'outline-1 'face-strong)
    (tj-set-face 'outline-2 'face-strong)
    (tj-set-face 'outline-3 'face-strong)
    (tj-set-face 'outline-4 'face-strong)
    (tj-set-face 'outline-5 'face-strong)
    (tj-set-face 'outline-6 'face-strong)
    (tj-set-face 'outline-7 'face-strong)
    (tj-set-face 'outline-8 'face-strong))

  ;; Interface
  (with-eval-after-load 'cus-edit
    (tj-set-face 'widget-field 'face-subtle)
    (tj-set-face 'widget-button 'face-strong)
    (tj-set-face 'widget-single-line-field 'face-subtle)
    (tj-set-face 'custom-group-subtitle 'face-strong)
    (tj-set-face 'custom-group-tag 'face-strong)
    (tj-set-face 'custom-group-tag-1 'face-strong)
    (tj-set-face 'custom-comment 'face-faded)
    (tj-set-face 'custom-comment-tag 'face-faded)
    (tj-set-face 'custom-changed 'face-salient)
    (tj-set-face 'custom-modified 'face-salient)
    (tj-set-face 'custom-face-tag 'face-strong)
    (tj-set-face 'custom-variable-tag 'default)
    (tj-set-face 'custom-invalid 'face-popout)
    (tj-set-face 'custom-visibility 'face-salient)
    (tj-set-face 'custom-state 'face-salient)
    (tj-set-face 'custom-link 'face-salient))

  (with-eval-after-load 'term
    (set-face-attribute 'term-color-yellow nil :foreground "#A2734C"))

  ;; Package
  (with-eval-after-load 'package
    (tj-set-face 'package-description 'default)
    (tj-set-face 'package-help-section-name 'default)
    (tj-set-face 'package-name 'face-salient)
    (tj-set-face 'package-status-avail-obso 'face-faded)
    (tj-set-face 'package-status-available 'default)
    (tj-set-face 'package-status-built-in 'face-salient)
    (tj-set-face 'package-status-dependency 'face-salient)
    (tj-set-face 'package-status-disabled 'face-faded)
    (tj-set-face 'package-status-external 'default)
    (tj-set-face 'package-status-held 'default)
    (tj-set-face 'package-status-incompat 'face-faded)
    (tj-set-face 'package-status-installed 'face-salient)
    (tj-set-face 'package-status-new 'default)
    (tj-set-face 'package-status-unsigned 'default)

    ;; Button face is hardcoded, we have to redefine the relevant
    ;; function
    (defun package-make-button (text &rest properties)
      "Insert button labeled TEXT with button PROPERTIES at point.
PROPERTIES are passed to `insert-text-button', for which this
function is a convenience wrapper used by `describe-package-1'."
      (let ((button-text
             (if (display-graphic-p)
                 text
               (concat "[" text "]")))
            (button-face
             (if (display-graphic-p)
                 '(:box
                   `(:line-width 1 :color "#999999" :style nil)
                   :foreground "#999999"
                   :background "#F0F0F0")
               'link)))
        (apply #'insert-text-button button-text 'face button-face 'follow-link t properties))))

  ;; Flyspell
  (with-eval-after-load 'flyspell
    (tj-set-face 'flyspell-duplicate 'face-popout)
    (tj-set-face 'flyspell-incorrect 'face-popout))

  ;; Ido 
  (with-eval-after-load 'ido
    (tj-set-face 'ido-first-match 'face-salient)
    (tj-set-face 'ido-only-match 'face-faded)
    (tj-set-face 'ido-subdir 'face-strong))

  ;; Diff
  (with-eval-after-load 'diff-mode
    (tj-set-face 'diff-header 'face-faded)
    (tj-set-face 'diff-file-header 'face-strong)
    (tj-set-face 'diff-context 'default)
    (tj-set-face 'diff-removed 'face-faded)
    (tj-set-face 'diff-changed 'face-popout)
    (tj-set-face 'diff-added 'face-salient)
    (tj-set-face 'diff-refine-added '(face-salient face-strong))
    (tj-set-face 'diff-refine-changed 'face-popout)
    (tj-set-face 'diff-refine-removed 'face-faded)
    (set-face-attribute 'diff-refine-removed nil :strike-through t))

  ;; org-agende
  (with-eval-after-load 'org-agenda
    (tj-set-face 'org-agenda-calendar-event 'default)
    (tj-set-face 'org-agenda-calendar-sexp 'face-faded)
    (tj-set-face 'org-agenda-clocking 'face-faded)
    (tj-set-face 'org-agenda-column-dateline 'face-faded)
    (tj-set-face 'org-agenda-current-time 'face-faded)
    (tj-set-face 'org-agenda-date 'face-salient)
    (tj-set-face 'org-agenda-date-today '(face-salient face-strong))
    (tj-set-face 'org-agenda-date-weekend 'face-faded)
    (tj-set-face 'org-agenda-diary 'face-faded)
    (tj-set-face 'org-agenda-dimmed-todo-face 'face-faded)
    (tj-set-face 'org-agenda-done 'face-faded)
    (tj-set-face 'org-agenda-filter-category 'face-faded)
    (tj-set-face 'org-agenda-filter-effort 'face-faded)
    (tj-set-face 'org-agenda-filter-regexp 'face-faded)
    (tj-set-face 'org-agenda-filter-tags 'face-faded)
    ;; fixes issue #18 (tj-set-face 'org-agenda-property-face                     'face-faded)
    (tj-set-face 'org-agenda-restriction-lock 'face-faded)
    (tj-set-face 'org-agenda-structure 'face-faded))

  ;; org mode
  (with-eval-after-load 'org
    (tj-set-face 'org-archived 'face-faded)
    (tj-set-face 'org-block 'face-faded)
    (tj-set-face 'org-block-begin-line 'face-faded)
    (tj-set-face 'org-block-end-line 'face-faded)
    (tj-set-face 'org-checkbox 'face-faded)
    (tj-set-face 'org-checkbox-statistics-done 'face-faded)
    (tj-set-face 'org-checkbox-statistics-todo 'face-faded)
    (tj-set-face 'org-clock-overlay 'face-faded)
    (tj-set-face 'org-code 'face-faded)
    (tj-set-face 'org-column 'face-faded)
    (tj-set-face 'org-column-title 'face-faded)
    (tj-set-face 'org-date 'face-faded)
    (tj-set-face 'org-date-selected 'face-faded)
    (tj-set-face 'org-default 'face-faded)
    (tj-set-face 'org-document-info 'face-faded)
    (tj-set-face 'org-document-info-keyword 'face-faded)
    (tj-set-face 'org-document-title 'face-faded)
    (tj-set-face 'org-done 'default)
    (tj-set-face 'org-drawer 'face-faded)
    (tj-set-face 'org-ellipsis 'face-faded)
    (tj-set-face 'org-footnote 'face-faded)
    (tj-set-face 'org-formula 'face-faded)
    (tj-set-face 'org-headline-done 'face-faded)
    ;;  (tj-set-face 'org-hide                                     'face-faded)
    ;;  (tj-set-face 'org-indent                                   'face-faded)
    (tj-set-face 'org-latex-and-related 'face-faded)
    (tj-set-face 'org-level-1 'face-strong)
    (tj-set-face 'org-level-2 'face-strong)
    (tj-set-face 'org-level-3 'face-strong)
    (tj-set-face 'org-level-4 'face-strong)
    (tj-set-face 'org-level-5 'face-strong)
    (tj-set-face 'org-level-6 'face-strong)
    (tj-set-face 'org-level-7 'face-strong)
    (tj-set-face 'org-level-8 'face-strong)
    (tj-set-face 'org-link 'face-salient)
    (tj-set-face 'org-list-dt 'face-faded)
    (tj-set-face 'org-macro 'face-faded)
    (tj-set-face 'org-meta-line 'face-faded)
    (tj-set-face 'org-mode-line-clock 'face-faded)
    (tj-set-face 'org-mode-line-clock-overrun 'face-faded)
    (tj-set-face 'org-priority 'face-faded)
    (tj-set-face 'org-property-value 'face-faded)
    (tj-set-face 'org-quote 'face-faded)
    (tj-set-face 'org-scheduled 'face-faded)
    (tj-set-face 'org-scheduled-previously 'face-faded)
    (tj-set-face 'org-scheduled-today 'face-faded)
    (tj-set-face 'org-sexp-date 'face-faded)
    (tj-set-face 'org-special-keyword 'face-faded)
    (tj-set-face 'org-table 'default)
    (tj-set-face 'org-tag 'face-faded)
    (tj-set-face 'org-tag-group 'face-faded)
    (tj-set-face 'org-target 'face-faded)
    (tj-set-face 'org-time-grid 'face-faded)
    (tj-set-face 'org-todo 'face-popout)
    (tj-set-face 'org-upcoming-deadline 'face-faded)
    (tj-set-face 'org-verbatim 'face-faded)
    (tj-set-face 'org-verse 'face-faded)
    (tj-set-face 'org-warning 'face-popout))

  ;; Mu4e
  (with-eval-after-load 'mu4e
    (tj-set-face 'mu4e-attach-number-face 'face-strong)
    (tj-set-face 'mu4e-cited-1-face 'face-faded)
    (tj-set-face 'mu4e-cited-2-face 'face-faded)
    (tj-set-face 'mu4e-cited-3-face 'face-faded)
    (tj-set-face 'mu4e-cited-4-face 'face-faded)
    (tj-set-face 'mu4e-cited-5-face 'face-faded)
    (tj-set-face 'mu4e-cited-6-face 'face-faded)
    (tj-set-face 'mu4e-cited-7-face 'face-faded)
    (tj-set-face 'mu4e-compose-header-face 'face-faded)
    (tj-set-face 'mu4e-compose-separator-face 'face-faded)
    (tj-set-face 'mu4e-contact-face 'face-salient)
    (tj-set-face 'mu4e-context-face 'face-faded)
    (tj-set-face 'mu4e-draft-face 'face-faded)
    (tj-set-face 'mu4e-flagged-face 'face-faded)
    (tj-set-face 'mu4e-footer-face 'face-faded)
    (tj-set-face 'mu4e-forwarded-face 'face-faded)
    (tj-set-face 'mu4e-header-face 'default)
    (tj-set-face 'mu4e-header-highlight-face 'face-subtle)
    (tj-set-face 'mu4e-header-key-face 'face-strong)
    (tj-set-face 'mu4e-header-marks-face 'face-faded)
    (tj-set-face 'mu4e-header-title-face 'face-strong)
    (tj-set-face 'mu4e-header-value-face 'default)
    (tj-set-face 'mu4e-highlight-face 'face-popout)
    (tj-set-face 'mu4e-link-face 'face-salient)
    (tj-set-face 'mu4e-modeline-face 'face-faded)
    (tj-set-face 'mu4e-moved-face 'face-faded)
    (tj-set-face 'mu4e-ok-face 'face-faded)
    (tj-set-face 'mu4e-region-code 'face-faded)
    (tj-set-face 'mu4e-replied-face 'face-salient)
    (tj-set-face 'mu4e-special-header-value-face 'default)
    (tj-set-face 'mu4e-system-face 'face-faded)
    (tj-set-face 'mu4e-title-face 'face-strong)
    (tj-set-face 'mu4e-trashed-face 'face-faded)
    (tj-set-face 'mu4e-unread-face 'face-strong)
    (tj-set-face 'mu4e-url-number-face 'face-faded)
    (tj-set-face 'mu4e-view-body-face 'default)
    (tj-set-face 'mu4e-warning-face 'face-faded))

  (set-default 'cursor-type '(box . 1))
  (set-face-attribute 'face-strong nil :weight 'bold)
  (tj-set-face 'font-lock-string-face nil)
  (tj-set-face 'font-lock-variable-name-face nil)
  (tj-set-face 'font-lock-function-name-face nil)
  (tj-set-face 'face-popout 'face-strong))

(tj-theme)

(provide 'tj)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tj.el ends here
