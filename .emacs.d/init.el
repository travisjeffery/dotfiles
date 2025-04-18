;; -*- fill-column: 65; lexical-binding: t; -*-

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Don't remove anything above.

(use-package
  emacs
  :ensure nil
  :custom
  (enable-recursive-minibuffers nil)
  (completions-max-height 20)
  (completions-format 'one-column)
  (completion-auto-help 'always)
  (completion-auto-select 'second-tab)
  (tab-always-indent 'complete)
  (initial-major-mode 'fundamental-mode)
  (tramp-default-method "sshx")
  ;; show all buffers, otherwise, some can be hidden under C-x b)
  (buffers-menu-max-size nil)
  (debugger-stack-frame-as-list t)
  (user-full-name "Travis Jeffery")
  (user-mail-address "tj@travisjeffery.com")
  ;; rescan the buffer contents to update jump targets
  (imenu-auto-rescan t)
  ;; config changes made through the customize UI will be stored here
  (custom-file
   (expand-file-name "custom.el" user-emacs-directory))
  ;; 'comment-indent-new-line continues comments
  (comment-multi-line t)
  ;; Save whatever's in the system clipboard before replcaing it with the Emacs' txt.
  (save-interprogram-paste-before-kill t)
  (read-process-output-max (* 1024 1024))
  ;; Always load newest byte code
  (load-prefer-newer t)
  ;; warn when opening files bigger than 100MB
  (large-file-warning-threshold 100000000)
  (sentence-end-double-space nil)
  ;; disable the annoying bell ring
  (ring-bell-function 'ignore)
  ;; disable startup screen
  (inhibit-startup-screen t)
  (eldoc-idle-delay 0)
  ;; nice scrolling
  (scroll-margin 0)
  (scroll-conservatively 100000)
  (scroll-preserve-screen-position 1)
  (native-comp-async-report-warnings-errors nil)
  ;; more useful frame title, that show either a file or a
  ;; buffer name (if the buffer isn't visiting a file 
  ;; Emacs modes typically provide a standard means to change the
  ;; indentation width -- eg. c-basic-offset: use that to adjust your
  ;; personal indentation width, while maintaining the style (and
  ;; meaning) of any files you load.
  (indent-tabs-mode nil) ;; don't use tabs to indent
  (tab-width 8) ;; but maintain correct appearance
  ;; smart tab behavior - indent or complete
  (grep-command "rg --no-heading")
  (fill-column 100)
  (same-window-regexps nil)
  ;; Newline at end of file
  (require-final-newline t)
  (frame-title-format
   '(:eval
     (if (buffer-file-name)
         (abbreviate-file-name (buffer-file-name))
       (buffer-name))
     "%b"))
  ;; hippie expand is dabbrev expand on steroids)
  (hippie-expand-try-functions-list
   '(try-expand-dabbrev
     try-expand-dabbrev-all-buffers
     try-expand-dabbrev-from-kill
     try-complete-file-name-partially
     try-complete-file-name
     try-expand-all-abbrevs
     (try-expand-list)
     try-expand-line
     try-complete-lisp-symbol-partially
     try-complete-lisp-symbol))
  ;; store all backup and autosave files in the tmp dir)
  (backup-directory-alist `((".*" . ,temporary-file-directory)))
  (auto-save-file-name-transforms
   `((".*" ,temporary-file-directory t)))
  (gc-cons-threshold 300000000)
  (major-mode-remap-alist '((python-mode . python-ts-mode)))
  (async-shell-command-buffer 'new-buffer)
  (ring-bell-function #'ignore)
  (backup-by-copying t)
  (delete-old-versions t)
  (kept-new-versions 10)
  (kept-old-versions 2)
  (vc-make-backup-files t)
  (version-control t)
  :diminish auto-revert-mode
  :diminish completion-preview-mode
  :config
  (global-completion-preview-mode 1)
  ;; handle long lines
  (global-so-long-mode t)
  ;; hide ui noise
  (tool-bar-mode -1)
  (menu-bar-mode -1)
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

  (defun tj-newline-and-indent ()
    "Open a new line above the current line."
    (interactive)
    (line-move -1)
    (end-of-line)
    (newline-and-indent))

  (defun tj-dir-locals-reload-buffer ()
    "Reload dir locals for the current buffer."
    (interactive)
    (let ((enable-local-variables :all))
      (hack-dir-local-variables-non-file-buffer)))

  (defun tj-dir-locals-reload-buffers ()
    "For every buffer with the same `default-directory` as the current buffer's, reload dir-locals."
    (interactive)
    (let ((dir (projectile-project-root)))
      (dolist (buffer (buffer-list))
        (with-current-buffer buffer
          (when (equal default-directory dir))
          (dir-locals-reload-buffer)))))

  (defun tj-hexadecimal-to-decimal-at-point ()
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

  (defun tj-window-split-toggle ()
    "Toggle window split."
    (interactive)
    (if (= (count-windows) 2)
        (let* ((this-win-buffer (window-buffer))
               (next-win-buffer (window-buffer (next-window)))
               (this-win-edges (window-edges (selected-window)))
               (next-win-edges (window-edges (next-window)))
               (this-win-2nd
                (not
                 (and (<= (car this-win-edges)
                          (car next-win-edges))
                      (<= (cadr this-win-edges)
                          (cadr next-win-edges)))))
               (splitter
                (if (= (car this-win-edges)
                       (car (window-edges (next-window))))
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

  (defun tj-comment-line ()
    "Comment the current line or region."
    (interactive)
    (call-interactively #'comment-line)
    (unless (region-active-p)
      (forward-line -1)))

  (defun tj-kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc
     'kill-buffer
     (delq
      (current-buffer)
      (remove-if-not 'buffer-file-name (buffer-list)))))

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
               (replace-regexp-in-string
                "^[ \t]+"
                ""
                (buffer-substring-no-properties beg end))))
        ;; (message "str=%s" str)
        (kill-new str)
        (goto-char orig-pos))))

  (defun tj-spongebob (start end)
    "Convert string from START to END to SpOnGeBoB meme."
    (interactive "r")
    (save-excursion
      (goto-char start)
      (let ((upcase?
             (not (s-uppercase? (char-to-string (char-after))))))
        (while (not (eq end (point)))
          (setq upcase? (not upcase?))
          (let* ((curchar (char-after))
                 (newchar
                  (if upcase?
                      (upcase curchar)
                    (downcase curchar))))
            (delete-char 1)
            (insert-char newchar))))))

  (defun tj-convert-multi-line-to-one-line (beg end)
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
               (replace-regexp-in-string
                "^[ \t]+"
                ""
                (buffer-substring-no-properties beg end))))
        ;; (message "str=%s" str)
        (kill-new str)
        (goto-char orig-pos))))

  (defun tj-spongebob (start end)
    "Convert string from START to END to SpOnGeBoB meme."
    (interactive "r")
    (save-excursion
      (goto-char start)
      (let ((upcase?
             (not (s-uppercase? (char-to-string (char-after))))))
        (while (not (eq end (point)))
          (setq upcase? (not upcase?))
          (let* ((curchar (char-after))
                 (newchar
                  (if upcase?
                      (upcase curchar)
                    (downcase curchar))))
            (delete-char 1)
            (insert-char newchar))))))

  (defun tj-thesaurus ()
    "Browse thesaurus."
    (interactive)
    (tj--browse-word
     "https://www.merriam-webster.com/thesaurus/%s"))

  (defun tj-dictionary ()
    "Browse dictionary."
    (interactive)
    (tj--browse-word "https://merriam-webster.com/dictionary/%s"))

  (defun tj--browse-word (url)
    (let ((word
           (or (and (region-active-p)
                    (buffer-substring-no-properties
                     (region-beginning) (region-end)))
               (read-string "Word: "))))
      (browse-url (format url word))))

  ;; improve find file at point to handle line numbers
  (defvar ffap-file-at-point-line-number nil
    "Variable to hold line number from the last `ffap-file-at-point' call.")

  (defadvice ffap-file-at-point
      (after ffap-store-line-number activate)
    "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
    (let*
        ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or
           (condition-case nil
               (and
                (not (string-match "//" string)) ; foo.com://bar
                (substitute-in-file-name string))
             (error nil))
           string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name
                          (1+ (match-beginning 0))
                          (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
      (if (and line-number (> line-number 0))
          (setq ffap-file-at-point-line-number line-number)
        (setq ffap-file-at-point-line-number nil))))

  (defadvice ffap-guesser (after ffap-store-line-number activate)
    "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
    (let*
        ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
         (name
          (or
           (condition-case nil
               (and
                (not (string-match "//" string)) ; foo.com://bar
                (substitute-in-file-name string))
             (error nil))
           string))
         (line-number-string
          (and (string-match ":[0-9]+" name)
               (substring name
                          (1+ (match-beginning 0))
                          (match-end 0))))
         (line-number
          (and line-number-string
               (string-to-number line-number-string))))
      (if (and line-number (> line-number 0))
          (setq ffap-file-at-point-line-number line-number)
        (setq ffap-file-at-point-line-number nil))))

  (defadvice find-file (after ffap-goto-line-number activate)
    "If `ffap-file-at-point-line-number' is non-nil goto this line."
    (when ffap-file-at-point-line-number
      (with-no-warnings
        (goto-line ffap-file-at-point-line-number))
      (setq ffap-file-at-point-line-number nil)))

  (defadvice find-file-at-point
      (after ffap-goto-line-number activate)
    "If `ffap-file-at-point-line-number' is non-nil goto this line."
    (when ffap-file-at-point-line-number
      (with-no-warnings
        (goto-line ffap-file-at-point-line-number))
      (setq ffap-file-at-point-line-number nil)))

  (defun tj-convert-commas-to-new-lines (start end)
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
    (let ((body
           (buffer-substring (region-beginning) (region-end))))
      (goto-char (region-beginning))
      (delete-char (string-width body))
      (yas-expand-snippet
       (concat
        "<${1:tag}$2>"
        body
        "</${1:$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text)"
        "(match-string 0 yas-text))}>"))))


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
      (let ((tag
             (buffer-substring (region-beginning) (region-end))))
        (delete-char (string-width tag))
        (cond
         ((string-match "\\`[bh]r\\'" tag)
          (insert (concat "<" tag ">")))
         ((string-match
           (concat
            "\\`\\(?:img\\|meta\\|link\\|"
            "input\\|base\\|area\\|col\\|"
            "frame\\|param\\)\\'")
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

  (defun tj-format-prowritingaid-to-markdown ()
    "Fix quotes after copying from ProWritingAid."
    (interactive)
    (let ((replacements
           '(("“" . "\"")
             ("”" . "\"")
             ("’" . "'")
             ("‘" . "'")
             (" " . " "))))
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
        (buffer-substring-no-properties
         beg end)
        (split-string "\n")
        (->>
         (remove "")
         (cl-mapcar (lambda (x) (format "\"\%s\"" x))))
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

  (defun tj-revert-all-buffers ()
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
                (revert-buffer
                 :ignore-auto
                 :noconfirm
                 :preserve-modes))
            ;; Otherwise, kill the buffer.
            (let
                (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buf)
              (message
               "Killed non-existing/unreadable file buffer: %s"
               filename))))))
    (message
     "Finished reverting buffers containing unmodified files."))

  (defun tj-copy-file-name-as-kill ()
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
          (let
              ((fill-column 12345678)) ;; relies on dynamic binding
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
             (resulting-text
              (funcall fn
                       (buffer-substring-no-properties beg end))))
        (kill-region beg end)
        (insert resulting-text))))

  (defun tj-find-duplicate-lines ()
    "Show all duplicate lines in the current buffer."
    (interactive)
    (save-excursion
      (goto-char 0)
      (let ((lines (make-hash-table :test 'equal)))
        (while (not (eobp))
          (when-let* ((line
                       (buffer-substring-no-properties
                        (line-beginning-position)
                        (line-end-position)))
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
          (occur
           (format "\\(%s\\)" (string-join lines "\\|")))))))

  :hook ((focus-out . garbage-collect))
  :bind
  (("C-g" . tj-keyboard-quit-dwim)
   ("M-;" . tj-comment-line)
   ("M-g M-c" . switch-to-completions)
   ("M-/" . hippie-expand)
   ("C-h A" . apropos)
   ;; align code in a pretty way
   ("C-x \\" . align-regexp)
   ("C-h C-f" . find-function)
   ;; misc useful keybindings
   ("C-c q" . tj-kill-other-buffer)
   ("M-*" . dictionary-lookup-definition)
   ("C-c <" . tj-insert-open-and-close-tag)
   ("C-c f" . find-file-at-point)))

(use-package pyenv-mode :ensure t :demand t)

(use-package direnv :ensure t :demand t :config (direnv-mode 1))

;; needs to be up early because there's some issue around packages depending on it the wrong way.

;; (use-package project :ensure (:wait t) :demand t)

(use-package xclip :config (xclip-mode 1) :ensure t :demand t)

(use-package verb :ensure t :demand t)

(use-package
  no-littering
  :custom
  (auto-save-file-name-transforms
   `((".*" ,(expand-file-name "auto-save/"
                              user-emacs-var-directory)
      t)))

  (backup-directory-alist
   `((".*" .
      ,(expand-file-name "backup/" user-emacs-var-directory))))
  :config (auto-save-mode 1)
  :ensure t
  :demand t)

(use-package compat :ensure nil :demand t)

(use-package hydra :ensure t :demand t)

(use-package major-mode-hydra :after hydra :ensure t :demand t)

(use-package
  bufler
  :after major-mode-hydra
  :bind (("C-x C-b" . bufler))
  :ensure t
  :demand t)

(use-package
  marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode)
  :ensure t
  :demand t)

(use-package
  goto-last-point
  :diminish
  :ensure t
  :demand t
  :config (goto-last-point-mode 1)
  :bind (("C-x ," . goto-last-point)))

(use-package bind-key :ensure nil :demand t)

(use-package
  sqlformat
  :custom
  (sqlformat-command 'pgformatter)
  (sqlformat-args '("-B" "-e"))
  :ensure t
  :demand t)

(use-package
  undo-tree
  :config
  (add-to-list
   'undo-tree-history-directory-alist
   '("." . "~/.emacs.d/var/undo-tree"))
  (global-undo-tree-mode 1)
  :diminish
  :ensure t
  :demand t)

(use-package
  lisp-mode
  :ensure nil
  :diminish
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
	  Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  :demand t)

(use-package
  browse-kill-ring
  :config
  (defun tj-replace-blank-kill (args)
    (let ((string (car args))
          (replace (cdr args))
          (last (car-safe kill-ring)))
      (when (and last (string-blank-p last))
        (setq replace t))
      (list string replace)))
  (advice-add 'kill-new :filter-args #'tj-replace-blank-kill)
  :bind (("M-y" . browse-kill-ring))
  :ensure t
  :demand t)

(use-package
  ielm
  :config (add-hook 'ielm-mode-hook #'eldoc-mode)
  :ensure nil
  :demand t)

(use-package
  avy
  :bind (("<C-return>" . avy-goto-char-timer))
  :config (avy-setup-default)
  :custom (avy-background t)
  :ensure t
  :demand t)

(use-package ipcalc :ensure t :demand t)

(use-package transient :ensure t :demand t)

(use-package
  igist
  :ensure t
  :demand t)

(use-package vterm :ensure t :demand t
  :bind (("C-c $" . vterm)))

(use-package
  with-editor
  :ensure t
  :demand t
  :config
  (add-hook 'eshell-mode-hook 'with-editor-export-editor))

(use-package
  magit
  :diminish magit-wip-mode
  :custom
  (magit-log-margin-width 18)
  (magit-refs-margin '(t age magit-log-margin-width t 18))
  (vc-follow-symlinks t)
  (magit-commit-ask-to-stage 'stage)
  (magit-refresh-status-buffer t)
  :config
  (magit-wip-mode 1)
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)

  (defun magit-key-mode--add-default-options (arguments)
    (if (eq (car arguments) 'pulling)
        (list 'pulling (list "--rebase"))
      arguments)
    (if (eq (car arguments) 'pushing)
        (list 'pushing (list "-u"))
      arguments))
  (advice-add
   'magit-key-mode
   :filter-args #'magit-key-mode--add-default-options)
  :bind
  (:map
   magit-diff-mode-map
   (("C-o" . magit-diff-visit-file-other-window)))
  :bind (("C-x g" . magit-status))
  :ensure t
  :demand t)

(use-package
  copy-as-format
  :custom (copy-as-format-default "github")
  :ensure t
  :demand t)

(use-package
  abbrev
  :diminish
  :ensure nil
  :custom (save-abbrevs 'silently)
  :config (abbrev-mode 1)
  :demand t)

(use-package
  jinx
  :ensure t
  :demand t
  :hook (markdown-mode . jinx-mode)
  :hook (org-mode . jinx-mode)
  :bind (("M-$" . jinx-correct) ("C-M-$" . jinx-languages)))

(use-package
  flymake-languagetool
  :ensure t
  :hook
  ((text-mode . flymake-languagetool-load)
   (latex-mode . flymake-languagetool-load)
   (org-mode . flymake-languagetool-load)
   (markdown-mode . flymake-languagetool-load))
  :init
  ;; Local Server Configuration
  (setq flymake-languagetool-server-jar
        "/usr/share/java/languagetool/languagetool-server.jar"))

(use-package
  compile
  :custom
  (compilation-max-output-line-length nil)
  (compilation-auto-jump-to-first-error t)
  (compilation-scroll-output t)
  :ensure nil
  :init (require 'grep)
  :no-require
  :bind (("M-O" . show-compilation))
  :bind
  (:map
   compilation-mode-map
   (("z" . delete-window)
    ("RET" . tj-compile-goto-error-same-window)))
  :bind
  (:map
   compilation-minor-mode-map
   ("RET" . tj-compile-goto-error-same-window))
  :bind
  (:map
   compilation-button-map
   ("RET" . tj-compile-goto-error-same-window))
  :bind (:map grep-mode-map ("RET" . tj-compile-goto-error-same-window))
  :preface
  (defun tj-compile-goto-error-same-window ()
    (interactive)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-same-window)
             (inhibit-same-window . nil))))
      (call-interactively #'compile-goto-error)))
  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match
                      "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))
  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set
     (make-local-variable 'comint-last-output-start)
     (point-marker)))
  :hook ((compilation-filter . compilation-ansi-color-process-output))
  :demand t)

(use-package
  comint
  :custom (shell-prompt-pattern "^; ")
  :ensure nil
  :demand t)

(use-package
  dired-toggle
  :preface
  (defun tj-dired-toggle-hook ()
    (interactive)
    (setq-local
     visual-line-fringe-indicators '(nil right-curly-arrow)
     word-wrap nil))
  :hook (dired-toggle-mode . tj-dired-toggle--hook)
  :ensure t
  :demand t)

(use-package
  dired-quick-sort
  :ensure t
  :demand t
  :config (dired-quick-sort-setup))

(use-package disk-usage :ensure t :demand t)

(use-package dired-filter :ensure t :demand t)

(use-package
  expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t
  :demand t)

(use-package
  re-builder
  :ensure nil
  :bind (:map reb-mode-map ("M-%" . reb-query-replace))
  :custom (reb-re-syntax 'string)
  :config
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive (progn
                   (barf-if-buffer-read-only)
                   (list
                    (query-replace-read-to
                     (reb-target-binding reb-regexp)
                     "Query replace"
                     t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp
       (reb-target-binding reb-regexp) to-string)))
  :demand t)

(use-package
  edit-indirect
  :bind (("C-x '" . edit-indirect-region))
  :ensure t
  :demand t)

;; hashicorp config language
(use-package hcl-mode :ensure t :demand t)

(use-package
  restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (defun tj-response-loaded-hook ()
    (flycheck-mode 0))
  (add-hook
   'restclient-response-loaded-hook 'tj-response-loaded-hook)
  (defun tj-restclient-hook ()
    (setq-local indent-line-function 'js-indent-line))
  (add-hook 'restclient-mode-hook 'tj-restclient-hook)
  :ensure t
  :demand t)

(use-package
  ediff
  :ensure nil
  :config
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents
       ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents
       ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
  :init (defvar tj-ediff-keymap (make-sparse-keymap))
  (setq
   ediff-split-window-function 'split-window-vertically
   ediff-merge-split-window-function 'split-window-vertically
   ediff-window-setup-function 'ediff-setup-windows-plain
   ediff-diff-options ""
   ediff-custom-diff-options "-u")
  :bind-keymap ("C-x C-=" . tj-ediff-keymap)
  :bind
  (:map
   tj-ediff-keymap
   ("b" . ediff-buffers)
   ("f" . ediff-files)
   ("r" . ediff-revision)
   ("l" . ediff-regions-linewise)
   ("w" . ediff-regions-wordwise)
   ("c" . compare-windows))
  :demand t)

(use-package
  git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind ("C-x G" . git-link)
  :ensure t
  :demand t)

(use-package git-modes :ensure t :demand t)

(use-package
  github-pullrequest
  :commands (github-pullrequest-new github-pullrequest-checkout)
  :ensure t
  :demand t)

(use-package
  gitpatch
  :commands gitpatch-mail
  :ensure t
  :demand t)

(use-package
  flycheck
  :diminish
  :custom
  (flycheck-idle-change-delay 4)
  (flycheck-check-syntax-automatically '(save mode-enable))
  :ensure t
  :demand t)

(use-package
  flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))
  :ensure t
  :demand t)

(use-package flycheck-clj-kondo :ensure t :demand t)

(use-package magit-diff-flycheck :ensure t :demand t)

(use-package
  google-this
  :bind ("C-c /" . google-this-search)
  :ensure t
  :demand t)

(use-package
  goto-last-change
  :bind ("C-x C-/" . goto-last-change)
  :ensure t
  :demand t)

(use-package
  ialign
  :bind ("C-x C-a" . ialign-interactive-align)
  :ensure t
  :demand t)

(use-package
  operate-on-number
  :bind ("C-# C-#" . operate-on-number-at-point)
  :ensure t
  :demand t)

(use-package
  shift-number
  :bind
  (("C-# -" . shift-number-down) ("C-# +" . shift-number-up))
  :ensure t
  :demand t)

(use-package diminish :ensure t :demand t)

(use-package
  eldoc
  :diminish eldoc-mode
  :custom (eldoc-echo-area-use-multiline-p nil)
  :bind (("C-c C-c" . eldoc))
  :ensure nil
  :demand t)

(use-package go-add-tags :after go-mode :ensure t :demand t)

(use-package gotest :after go-mode :ensure t :demand t)

(use-package
  go-errcheck
  :ensure-system-package ((errcheck . "go install github.com/kisielk/errcheck@latest"))
  :after go-mode-abbrev-table
  :config
  (defun tj-go-errcheck ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (go-errcheck nil nil nil)))
  :ensure t
  :demand t)

(use-package
  go-mode
  :bind
  (:map
   go-mode-map
   ("M-j" . comment-indent-new-line)
   ("C-c C-t" . go-test-current-file)
   ("C-c M-t" . go-test-current-test))

  :config

    (defun eglot-format-and-organize-imports ()
    "Update the imports and format the file, analogous to commands like Go's `goimports`
but agnostic to language, mode, and server."
    (interactive)
    (let* ((actions (eglot-code-actions (point-min) (point-max) "source.organizeImports")))
      (when actions
        (eglot-execute (eglot--current-server-or-lose) (car actions)))
      (eglot-format-buffer)))

  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'eglot-format-and-organize-imports nil t)))

  (defun tj-find-go-project-root (dir)
    "Find go project root for DIR."
    (if (and dir
             (not
              (f-descendant-of-p
               dir
               (or (getenv "GOPATH")
                   (concat (getenv "HOME") "/go")))))
        (let ((result (locate-dominating-file dir "go.mod")))
          (if result
              (cons 'transient (expand-file-name result))
            (cons 'transient dir)))
      (when dir
        (cons 'transient dir))))

  (setq tab-width 8)
  (setq-local compilation-read-command nil)

  (set-face-foreground 'go-test--ok-face "forest green")
  (set-face-foreground 'go-test--standard-face "dark orange")

  (defun go-test-current-test ()
    "Launch go test on the current test."
    (interactive)
    (cl-destructuring-bind
        (test-suite test-name) (go-test--get-current-test-info)
      (let ((test-flag
             (if (> (length test-suite) 0)
                 "-testify.m "
               "-run "))
            (additional-arguments
             (if go-test-additional-arguments-function
                 (funcall go-test-additional-arguments-function
                          test-suite
                          test-name)
               "")))
        (when test-name
          (if (go-test--is-gb-project)
              (go-test--gb-start
               (s-concat
                "-test.v=true -test.run=" test-name "\\$ ."))
            (go-test--go-test
             (s-concat
              test-flag
              test-name
              additional-arguments
              "\\$ .")))))))

  (defun tj-go-hook ()
    (setq
     imenu-generic-expression
     '(("type"
        "^[ \t]*type *\\([^ \t\n\r\f]*[ \t]*\\(struct\\|interface\\)\\)"
        1)
       ("func" "^func *\\(.*\\)" 1)))

    (setq-local project-find-functions
                (list #'tj-find-go-project-root #'project-try-vc))
    (setq case-fold-search t)
    (setq go-test-args "-timeout 60s -race -v")
    (which-function-mode -1)
    (flycheck-mode 1)
    (highlight-symbol-mode 1)
    (electric-pair-mode 1)
    (show-paren-mode 1)
    (selected-minor-mode 1))

  (if (not (string-match "go" compile-command))
      (set
       (make-local-variable 'compile-command)
       "go build -v && go test -v && go vet"))

  :hook ((go-mode . tj-go-hook))
  :ensure (:wait t)
  :demand t)

(use-package
  ws-butler
  :ensure t
  :demand t
  :diminish
  :config (ws-butler-global-mode 1))

;; (use-package winner :ensure t :demand t :diminish :config (winner-mode 1))

(use-package
  eacl
  :config
  (setq eacl-grep-program
        "grep --exclude-dir=.git --exclude-dir=vendor")
  :bind (("C-x C-l" . eacl-complete-line))
  :ensure t
  :demand t)

(use-package go-gen-test :after go-mode :ensure t :demand t)

(use-package
  embrace
  :config (setq embrace-show-help-p nil)
  :bind ("C-x C-y" . embrace-commander)
  :ensure t
  :demand t)

(use-package
  ripgrep
  :config (setq ripgrep-arguments '("--hidden"))
  :ensure t
  :demand t)

(use-package web-beautify :ensure t :demand t)

(use-package
  elisp-slime-nav
  :diminish
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode))
  :ensure t
  :demand t)

(use-package
  paren
  :diminish show-paren-mode
  :config
  ;; (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (show-paren-mode 1)
  :ensure nil
  :demand t)

(use-package
  uniquify
  :ensure nil
  :config
  (setq
   uniquify-buffer-name-style 'forward
   uniquify-separator "/"
   ;; rename after killing uniquified
   uniquify-after-kill-buffer-p t
   ;; don't muck with special buffers
   uniquify-ignore-buffers-re "^\\*")
  :demand t)

(use-package
  saveplace
  :diminish
  :ensure nil
  :config
  (defconst savefile-dir
    (expand-file-name "savefile" user-emacs-var-directory))
  ;; create the savefile dir if it doesn't exist
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir))
  (setq save-place-file
        (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t)
  :demand t)

(use-package
  hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :demand t)

(use-package
  recentf
  :after saveplace
  :config (add-to-list 'recentf-exclude user-emacs-var-directory)
  (setq
   recentf-save-file (expand-file-name "recentf" savefile-dir)
   recentf-max-saved-items 500
   recentf-max-menu-items 15
   ;; disable recentf-cleanup on Emacs start, because it can cause
   ;; problems with remote files
   recentf-auto-cleanup 'never)
  (recentf-mode 1)
  :bind (("C-x C-r" . recentf-open-files))
  :ensure nil
  :demand t)

(use-package
  windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings)
  :ensure nil
  :demand t)

(use-package
  highlight-symbol
  :diminish
  :config (setq highlight-symbol-idle-delay 0.1)
  :bind
  (("M-p" . highlight-symbol-prev)
   ("M-n" . highlight-symbol-next)
   ("M-'" . highlight-symbol-query-replace))
  :hook (prog-mode . highlight-symbol-mode)
  :ensure t
  :demand t)

(use-package
  diffview
  :commands (diffview-current diffview-region diffview-message)
  :ensure t
  :demand t)

(use-package
  dired
  :ensure nil
  :bind (("C-x d" . dired-jump))
  :bind
  (:map
   dired-mode-map
   ("z" . delete-window)
   ("e" . ora-ediff-files)
   ("l" . dired-up-directory)
   ("Y" . ora-dired-rsync)
   ("<tab>" . tj-dired-switch-window)
   ("M-!" . async-shell-command)
   ("M-G"))
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))
  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))
  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))
  (defun dired-double-jump (first-dir second-dir)
    (interactive (list
                  (read-directory-name "First directory: "
                                       (expand-file-name "~")
                                       nil
                                       nil
                                       "dl/")
                  (read-directory-name "Second directory: "
                                       (expand-file-name "~")
                                       nil
                                       nil
                                       "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))
  (defun tj-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))
  (defun ora-dired-rsync (dest)
    (interactive (list
                  (expand-file-name
                   (read-file-name
                    "Rsync to: "
                    (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files nil current-prefix-arg))
          (tmtxt/rsync-command "rsync -arvz --progress "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat
               tmtxt/rsync-command
               (shell-quote-argument file)
               " ")))
      (setq tmtxt/rsync-command
            (concat
             tmtxt/rsync-command (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))
  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2
                 (if (cdr files)
                     (cadr files)
                   (read-file-name
                    "file: "
                    (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook
             'ediff-after-quit-hook-internal
             `(lambda ()
                (setq ediff-after-quit-hook-internal nil)
                (set-window-configuration ,wnd))))
        (error "No more than 2 files should be marked"))))
  :config
  (defun dired-back-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))
  (define-key
   dired-mode-map
   (vector 'remap 'beginning-of-buffer)
   'dired-back-to-top)
  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (define-key
   dired-mode-map
   (vector 'remap 'end-of-buffer)
   'dired-jump-to-bottom)

  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j (dired-jump)
  (require 'dired-x)

  (defadvice dired-omit-startup
      (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode)
    dired-mode-map)
  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and (not (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value (dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value (dired-move-to-filename))))
  (defadvice dired-previous-line
      (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and (not (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value (dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))
  (defvar dired-omit-regexp-orig
    (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not
                     (string=
                      (file-name-directory file) parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|"
                 "")
               "\\("
               (mapconcat #'(lambda (str)
                              (concat
                               "^"
                               (regexp-quote
                                (substring str
                                           13
                                           (if (= ?/
                                                  (aref
                                                   str
                                                   (1- (length
                                                        str))))
                                               (1- (length str))
                                             nil)))
                               "$"))
                          (split-string omitted-files "\n" t)
                          "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig))))

  :demand t)

(use-package
  dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow))
  :ensure t
  :demand t)

(use-package package-lint :ensure t :demand t)

(use-package
  dired-ranger
  :bind
  (:map
   dired-mode-map
   ("W" . dired-ranger-copy)
   ("X" . dired-ranger-move)
   ("Y" . dired-ranger-paste))
  :ensure t
  :demand t)

(use-package
  sh-script
  :ensure nil
  :custom
  (sh-basic-indentation 2)
  (sh-basic-offset 2)
  :mode (("\\.bats$" . sh-mode) ("Dockerfile" . sh-mode))
  :demand t)

(use-package
  visual-replace
  :ensure t
  :demand t
  :config (visual-replace-global-mode 1))

(use-package
  easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark)
  :ensure t
  :demand t)

(use-package
  easy-kill-extras
  :config
  (global-set-key (kbd "M-@") 'easy-mark-word)
  (global-set-key (kbd "C-M-@") 'easy-mark-sexp)
  (global-set-key [remap zap-to-char] 'easy-mark-to-char)

  ;; Integrate `expand-region' functionality with easy-kill
  (define-key easy-kill-base-map (kbd "o") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "i") 'easy-kill-er-unexpand)

  ;; Add the following tuples to `easy-kill-alist', preferrably by
  ;; using `customize-variable'.
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list
   'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list
   'easy-kill-alist '(?T string-up-to-char-backward ""))
  :ensure t
  :demand t)

(use-package
  exec-path-from-shell
  :custom
  (exec-path-from-shell-variables
   '("PATH"
     "MANPATH"
     "GOROOT"
     "GOPATH"
     "JAVA_HOME"
     "JAVA_OPTS"
     "RUST_SRC_PATH"
     "VAULT_ADDR"
     "GOPRIVATE"))
  :config (exec-path-from-shell-initialize)
  :ensure t
  :demand t)

(use-package kubedoc :ensure t :demand t)

(use-package
  move-text
  :bind (("M-P" . move-text-up) ("M-N" . move-text-down))
  :ensure t
  :demand t)

(use-package
  whitespace
  :custom
  (whitespace-line-column 76) ;; limit line length
  (whitespace-style '(face empty lines trailing))
  :config (global-whitespace-mode 1)
  :diminish
  :ensure nil
  :demand t)

(use-package htmlize
  :ensure t
  :demand t)

(use-package
  markdown-mode
  :custom
  (markdown-command
   "pandoc --section-divs --from=markdown_github --highlight-style=haddock --self-contained -f markdown+smart --to=html5 --css=$HOME/.config/css/style.css")
  :config
  (unless (executable-find "pandoc")
    (message "install pandoc"))
  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :hook ((markdown-mode . writegood-mode))
  :ensure t
  :demand t)

(use-package
  forge
  :custom
  (add-to-list 'forge-alist '("gh"
                              "api.github.com"
                              "github.com"
                              forge-github-repository))
  (forge-topic-list-limit '(3 . -1))
  (forge-pull-notifications nil)
  :ensure t
  :demand t)

(use-package writegood-mode :ensure t :demand t)

(use-package
  yaml-mode
  :mode ("\\.yaml" . yaml-mode)
  :ensure t
  :demand t)

(use-package yaml-pro :ensure t :demand t)

(use-package
  org
  :ensure nil
  :hook ((org-mode . visual-line-mode) (org-mode . auto-fill-mode))
  :custom
  (org-todo-keywords
   '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
     (sequence
      "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)")))
  (org-todo-keyword-faces
   '(("TODO" . org-todo)
     ("DONE" . (:foreground "black" :weight bold))))
  (org-archive-location "~/.archive.org::* Archived Tasks")
  (org-src-lang-modes
   '(("screen" . sh)
     ("ocaml" . tuareg)
     ("elisp" . emacs-lisp)
     ("lisp" . lisp)
     ("ditaa" . artist)
     ("asymptote" . asy)
     ("cl" . lisp)
     ("dot" . graphviz-dot)))
  (org-startup-folded nil)
  (org-startup-indented t)
  (org-indent-indentation-per-level 0)
  (org-hide-leading-stars nil)
  (org-clock-persist 'history)
  (org-src-tab-acts-natively t)
  (org-directory (expand-file-name "~/"))
  (org-default-notes-file (expand-file-name "~/notes.org"))
  (org-agenda-files '("~/"))

  ;; activate single letter commands at beginning of a headline.
  (org-use-speed-commands t)

  (org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; use fast todo selection with C-c C-t
  (org-use-fast-todo-selection t)

  (org-refile-targets
   (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
  (org-refile-use-outline-path t)
  (org-outline-path-complete-in-steps nil)
  (org-refile-allow-creating-parent-nodes (quote confirm))
  (org-agenda-dim-blocked-tasks nil)
  (org-agenda-compact-blocks t)
  (org-agenda-custom-commands
   (quote (("d" todo nil)
           ("c" todo "DONE|DEFERRED|CANCELLED" nil)
           ("w" todo "WAITING" nil)
           ("W" agenda "" ((org-agenda-ndays 21)))
           ("A" agenda ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if
                 (quote notregexp) "\\=.*\\[#A\\]")))
             (org-agenda-ndays 1)
             (org-agenda-overriding-header
              "Today's Priority #A tasks: ")))
           ("u" alltodo ""
            ((org-agenda-skip-function
              (lambda nil
                (org-agenda-skip-entry-if
                 (quote scheduled)
                 (quote deadline)
                 (quote regexp)
                 "\n]+>")))
             (org-agenda-overriding-header
              "Unscheduled TODO entries: "))))))

  (org-capture-templates
   (quote
    (("t"
      "Todo"
      entry
      (file "~/capture.org")
      "* TODO %?\n%U\n%a\n"
      :clock-in t
      :clock-resume t)
     ("n"
      "Note"
      entry
      (file "~/capture.org")
      "* %? :NOTE:\n%U\n%a\n"
      :clock-in t
      :clock-resume t)
     ("j"
      "Journal"
      entry
      (function org-journal-find-location)
      "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?"))))
  :bind
  (("C-x C-g c" . org-capture)
   ("C-x C-g t" . org-todo-list)
   ("C-x C-g m" . orgs-tagsview))
  :config
  (org-clock-persistence-insinuate)
  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))
  :demand t)

(use-package
  org-roam
  :ensure t
  :demand t
  :init (defvar tj-org-roam-keymap (make-sparse-keymap))
  :custom
  (org-roam-directory (file-truename "~/roam"))
  (org-roam-v2-ack t)
  :bind-keymap ("C-c r" . tj-org-roam-keymap)
  :bind
  (:map
   tj-org-roam-keymap
   ("g" . org-roam-graph)
   ("i" . org-roam-node-insert)
   ("f" . org-roam-node-find)
   ("c" . org-roam-capture))
  :config (org-roam-setup))

(use-package
  treesit-auto
  :demand t
  :ensure t
  :config
  (global-treesit-auto-mode))

(use-package
  org-journal
  :after org
  :custom
  (org-journal-dir "~/journal")
  (org-journal-file-format "%Y%m%d.org")
  :ensure t
  :demand t)

(use-package org-web-tools :demand t :ensure t)

(use-package
  alert
  :commands (alert)
  :custom (alert-default-style 'notifier)
  :ensure t
  :demand t)

(use-package
  ert
  :demand t
  :bind
  ((:map ert-results-mode-map ("o" . #'ace-link-help)))
  :ensure nil)

(use-package
  ace-link
  :after ert
  :config (ace-link-setup-default)
  :ensure t
  :demand t)

(use-package
  ace-mc
  :after multiple-cursors
  :bind
  (:map
   tj-mc-keymap
   ("h" . ace-mc-add-multiple-cursors)
   ("M-h" . ace-mc-add-single-cursor))
  :ensure t
  :demand t)

(use-package
  multiple-cursors
  :after selected
  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark))
  :config (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  :init (defvar tj-mc-keymap (make-sparse-keymap))
  :bind-keymap ("C-;" . tj-mc-keymap)
  :bind
  (:map
   tj-mc-keymap
   ("C-;" . mc/edit-lines)
   ("C-e" . mc/edit-ends-of-lines)
   ("C-a" . mc/edit-beginnings-of-lines)
   ("a" . mc/mark-all-dwim)
   ("C-x" . reactivate-mark)
   ("C-SPC" . mc/mark-pop)
   ("f" . mc/mark-next-like-this-symbol)
   ("b" . mc/mark-previous-like-this-symbol)
   ;; Extra multiple cursors stuff
   ("%" . mc/insert-numbers))
  :ensure t
  :demand t)

(use-package
  mc-extras
  :after multiple-cursors
  :bind
  (:map
   tj-mc-keymap
   ("M-C-f" . mc/mark-next-sexps)
   ("M-C-b" . mc/mark-previous-sexps)
   ("C-d" . mc/remove-current-cursor)
   ("C-k" . mc/remove-cursors-at-eol)
   ("M-d" . mc/remove-duplicated-cursors)
   ("|" . mc/move-to-column)
   ("~" . mc/compare-chars))
  :ensure t
  :demand t)

(use-package
  phi-search
  :after multiple-cusors
  :bind
  (:map
   mc/keymap ("C-r" . phi-search-backward) ("C-s" . phi-search))
  :ensure t
  :demand t)

(use-package journalctl-mode :ensure t :demand t)

(use-package ace-jump-mode :demand t :ensure t)

(use-package
  browse-url
  :bind (("C-x x" . browse-url-at-point))
  :ensure nil
  :demand t)

(defun eshell-execute-current-line ()
  "Insert current line at the end of the buffer."
  (interactive)
  (let ((command
         (buffer-substring
          (save-excursion
            (beginning-of-line)
            (point))
          (save-excursion
            (end-of-line)
            (point)))))
    (eshell--execute-command command t)))

(defun eshell-insert-command (text &optional func)
  "Insert TEXT command at the end of the buffer and call 'eshell-send-input or FUNC if given."
  (interactive)
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell--execute-command (command save-excursion?)
  (let ((body (lambda nil (eshell-insert-command command))))
    (if save-excursion?
        (save-excursion (funcall body))
      (funcall body))))

(defun eshell/clear ()
  "Clear eshell's buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(use-package
  em-hist
  :demand t
  :ensure nil
  :bind
  (:map eshell-hist-mode-map ("M-r" . consult-history)))

(use-package
  spacious-padding
  :demand t
  :ensure t
  :config (spacious-padding-mode 1))

(use-package
  modus-themes
  :demand t
  :ensure t
  :config (load-theme 'modus-vivendi :no-confirm))

(use-package
  zop-to-char
  :bind (("M-z" . zop-up-to-char) ("M-Z" . zop-to-char))
  :ensure t
  :demand t)

(use-package
  fontaine
  :ensure t
  :demand t
  :custom
  (fontaine-presets
   '((regular)
     (large :default-height 140)
     (presentation :default-height 160)
     (t
      :default-family "IBM Plex Mono"
      :variable-pitch-family "IBM Plex Sans"
      :default-height 120)))
  :config
  (fontaine-set-preset
   (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

(use-package
  crux
  :custom (user-init-file (concat user-emacs-directory "init.el"))
  :bind
  (("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c H" . crux-cleanup-buffer-or-region)
   ("C-M-z" . crux-indent-defun)
   ("C-c u" . crux-view-url)
   ("C-c w" . crux-swap-windows)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c k" . crux-kill-other-buffers)
   ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c I" . crux-find-user-init-file)
   ("C-c S" . crux-find-shell-init-file)
   ("C-S-j" . crux-top-join-line)
   ("C-^" . crux-top-join-line)
   ("C-k" . kill-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([(shift return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c s" . crux-ispell-word-then-abbrev))
  :ensure t
  :demand t)

(use-package
  diff-hl
  :config
  (global-diff-hl-mode 1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
  :ensure t
  :demand t)

(use-package
  which-key
  :diminish which-key-mode
  :config (which-key-mode 1)
  :ensure t
  :demand t)

(use-package
  goto-chg
  :bind
  (("C-x ." . goto-last-change)
   ("C-x /" . goto-last-change-reverse))
  :ensure t
  :demand t)

(use-package
  color-moccur
  :bind (("C-c o" . occur) ("C-c O" . moccur))
  :ensure t
  :demand t)

(use-package
  ace-window
  :diminish
  :bind* ("<C-M-return>" . ace-window)
  :ensure t
  :demand t)

(use-package
  minibuffer
  :bind
  (:map
   completion-in-region-mode-map
   ("C-n" . minibuffer-next-completion)
   ("C-p" . minibuffer-previous-completion)
   ("<return>" . minibuffer-choose-completion)
   :map
   minibuffer-local-completion-map
   ("C-n" . minibuffer-next-completion)
   ("C-p" . minibuffer-previous-completion)
   ("<return>" . minibuffer-choose-completion))
  :ensure nil
  :config
  (defun tj-minibuffer-setup-hook ()
    (electric-pair-mode 0)
    (setq truncate-lines nil)
    (setq gc-cons-threshold most-positive-fixnum))
  (defun tj-minibuffer-exit-hook ()
    (electric-pair-mode 1)
    (setq gc-cons-threshold 800000))
  (add-hook 'minibuffer-setup-hook #'tj-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'tj-minibuffer-exit-hook)
  :demand t)

(use-package
  puni
  :diminish
  :bind (("M-S" . puni-splice))
  :ensure t
  :demand t)

(use-package
  eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table))
  :ensure t
  :demand t)

(use-package
  selected
  :diminish selected-minor-mode
  :config (selected-global-mode 1)
  :ensure t
  :demand t)

;; temporarily highlight changes from yanking, etc
(use-package
  volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode 1)
  :ensure t
  :demand t)

(use-package pcmpl-args :ensure t :demand t)

(use-package
  em-unix
  :after eshell
  :ensure nil
  :config
  (unintern 'eshell/su nil)
  (unintern 'eshell/sudo nil)
  :demand t)

(use-package em-smart :after eshell :ensure nil :demand t)

(use-package
  eshell
  :custom
  (eshell-history-file-name (file-truename "~/.zsh_history"))
  (eshell-prompt-function 'tj-eshell-prompt)
  (eshell-where-to-jump 'end)
  (eshell-review-quick-commands t)
  (eshell-smart-space-goes-to-end t)
  :config
  (defun tj-hist-load ()
    (cl-flet
        ((unmetafy
           (input)
           (let ((i 0)
                 output)
             (while-let ((char (nth i input))
                         (inc-and-char
                          (if (= char #x83)
                              ;; Skip meta character and unmetafy.
                              `(2 . ,(logxor (nth (1+ i) input) 32))
                            ;; Advance as usual.
                            `(1 . ,char))))
               (cl-incf i (car inc-and-char))
               (setq output (cons (cdr inc-and-char) output)))
             (decode-coding-string (apply #'unibyte-string
                                          (nreverse output))
                                   'utf-8-unix
                                   t))))
      (let ((hist-file eshell-history-file-name))
        (with-temp-buffer
          (insert
           (mapconcat (-compose #'unmetafy #'string-to-list)
                      (s-lines (f-read-bytes hist-file))
                      "\n"))
          (write-file hist-file)))))

  (defun tj-eshell-exit (&optional arg)
    "Exit eshell and kill the current frame."
    (interactive "P")
    (slot/unmetafy)
    (eshell-write-history)
    (save-buffers-kill-terminal))

  (defun tj-eshell-prompt ()
    "$ ")

  (add-to-list
   'eshell-expand-input-functions
   'eshell-expand-history-references)

  :hook (eshell-mode . tj-eshell-hook)
  :hook (eshell-hist-load . tj-hist-load)
  :hook (eshell-exit-hook . tj-eshell-exit)
  :ensure nil
  :demand t)

(use-package
  eshell-bookmark
  :after eshell
  :hook (eshell-mode . eshell-bookmark-setup)
  :ensure t
  :demand t)

(use-package
  eshell-up
  :after eshell
  :commands eshell-up
  :ensure t
  :demand t)

(use-package eshell-z :after eshell :ensure t :demand t)

(use-package
  fancy-narrow
  :commands (fancy-narrow-to-region fancy-widen)
  :ensure t
  :demand t)

(use-package wgrep :ensure t :demand t)

(use-package json-snatcher :ensure t :demand t)

(use-package ctrlf :ensure t :demand t
  :custom
  (ctrlf-default-search-style 'regexp)
  (ctrlf-alternate-search-style 'fuzzy)
  (ctrlf-go-to-end-of-match nil) :config (ctrlf-mode 1))

(use-package
  visual-regexp
  :bind ("M-&" . vr/query-replace)
  :ensure t
  :demand t)

(use-package
  avy-zap
  :bind
  (("C-c e" . zap-up-to-char)
   ("C-c E" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim))
  :ensure t
  :demand t)

(use-package
  backup-walker
  :commands backup-walker-start
  :ensure t
  :demand t)

(use-package
  change-inner
  :bind
  (("M-i" . change-inner)
   ("M-o" . change-outer)
   ("C-x M-i" . copy-inner)
   ("C-x M-o" . copy-outer))
  :ensure t
  :demand t)

(use-package
  protobuf-mode
  :mode "\\.proto\\'"
  :commands (protobuf-mode)
  :hook
  (protobuf-mode
   .
   (lambda ()
     (c-add-style "tj-protobuf-style" tj-protobuf-style t)
     (setq imenu-generic-expression
           tj-protobuf-imenu-generic-expression)))
  :config
  (defconst tj-protobuf-style
    '((c-basic-offset . 2) (indent-tabs-mode . nil)))
  (setq tj-protobuf-imenu-generic-expression
        '(("Message" "^message *\\([a-zA-Z0-9_]+\\)" 1)
          ("Service" "^service *\\([a-zA-Z0-9_]+\\)" 1)))
  :ensure t
  :demand t)

(use-package
  bm
  :commands
  (bm-repository-load
   bm-buffer-save bm-buffer-save-all bm-buffer-restore)
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook
   'kill-emacs-hook
   (lambda nil
     (bm-buffer-save-all)
     (bm-repository-save)))
  :ensure t
  :demand t)

(use-package
  terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode)
  :ensure t
  :demand t)

(use-package
  multifiles
  :bind ("C-!" . mf/mirror-region-in-multifile)
  :ensure t
  :demand t)

(use-package
  toggle-quotes
  :bind ("C-\"" . toggle-quotes)
  :ensure t
  :demand t)

(define-key
 occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(use-package
  highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background
   'highlight-indentation-current-column-face "#c3b3b3")
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-hook
   'yaml-mode-hook 'highlight-indentation-current-column-mode)
  :ensure t
  :demand t)

(use-package
  indent-tools
  :config (add-hook 'yaml-mode-hook 'indent-tools-minor-mode)
  :ensure t
  :demand t)

(use-package
  resmacro
  :ensure nil
  :bind (("C-x (" . resmacro-start-macro))
  :demand t)

(use-package
  paredit
  :ensure t
  :demand t
  :diminish
  :hook ((emacs-lisp-mode . paredit-mode)))

(use-package wordswitch :ensure nil :demand t)

(use-package titlecase :ensure nil :demand t)

(use-package
  unfill
  :bind (("M-Q" . unfill-paragraph))
  :ensure t
  :demand t)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(use-package
  dot-mode
  :config
  (setq dot-mode-global-mode t)
  (dot-mode 1)
  :bind (("C-." . dot-mode-execute))
  :ensure t
  :demand t)

(use-package
  iedit
  :init (setq iedit-toggle-key-default (kbd "C-:"))
  :ensure t
  :demand t)

(use-package frog-jump-buffer :ensure t :demand t)

(use-package github-review :ensure t :demand t)

(use-package
  orderless
  :demand t
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrride nil))

(use-package
  consult
  :ensure t
  :demand t
  ;; Replace bindings. Lazily loaded by `use-package'.
  :bind
  (
   ;; C-c bindings in `mode-specific-map'
   ("C-c k" . consult-kmacro)
   ("C-c i" . consult-info)

   ([remap Info-search] . consult-info)

   ;; C-x bindings in `ctl-x-map'
   ("C-x C-l" . consult-line)
   ("C-c a" . ripgrep-regexp)
   ("C-x M-:" . consult-complex-command) ;; orig. repeat-complex-command
   ("C-x b" . consult-buffer) ;; orig. switch-to-buffer
   ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ("C-x 5 b" . consult-buffer-other-frame) ;; orig. switch-to-buffer-other-frame
   ("C-x r b" . consult-bookmark) ;; orig. bookmark-jump
   ("C-x p b" . consult-project-buffer) ;; orig. project-switch-to-buffer
   ;; Custom M-# bindings for fast register access
   ("M-#" . consult-register-load)
   ("M-'" . consult-register-store) ;; orig. abbrev-prefix-mark (unrelated)
   ("C-M-#" . consult-register)

   ;; Other custom bindings
   ("M-y" . consult-yank-pop) ;; orig. yank-pop

   ;; M-g bindings in `goto-map'
   ("M-g e" . consult-compile-error)
   ("M-g f" . consult-flymake) ;; Alternative: consult-flycheck
   ("M-g g" . consult-goto-line) ;; orig. goto-line
   ("M-g M-g" . consult-goto-line) ;; orig. goto-line
   ("M-g o" . consult-outline) ;; Alternative: consult-org-heading
   ("M-g m" . consult-mark)
   ("M-g k" . consult-global-mark)
   ("M-g i" . consult-imenu)
   ("M-g I" . consult-imenu-multi)

   :map
   isearch-mode-map
   ("M-e" . consult-isearch-history) ;;  isearch-edit-string

   ;; Minibuffer history
   :map
   minibuffer-local-map
   ("M-r" . consult-history)) ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq
   register-preview-delay 0.5
   register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add
   #'register-preview
   :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq
   xref-show-xrefs-function #'consult-xref
   xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key "M-.")
  ;; (setq consult-preview-key '("S-<down>" "S-<up>"))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key
   '(:debounce 0.2 any)
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   consult-xref
   consult--source-bookmark
   consult--source-file-register
   consult--source-recent-file
   consult--source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; "C-+"

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (keymap-set consult-narrow-map (concat consult-narrow-key " ?") #'consult-narrow-help)
  )

(global-set-key (kbd "M-f") 'forward-to-word)

;; Add extensions
(use-package
  cape
  :ensure t
  :demand t
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
  )

(use-package
  vertico
  :init (vertico-mode 1)
  :config
  (setq vertico-multiform-commands
        '((consult-line buffer)
          (consult-line-thing-at-point buffer)
          (consult-recent-file buffer)
          (consult-mode-command buffer)
          (consult-complex-command buffer)
          (embark-bindings buffer)
          (consult-locate buffer)
          (consult-project-buffer buffer)
          (consult-ripgrep buffer)
          (consult-fd buffer)))
  :bind
  (:map
   vertico-map
   ("C-k" . kill-whole-line)
   ("C-o" . vertico-next-group)
   ("<tab>" . minibuffer-complete)
   ("M-S-<return>" . vertico-exit-input)
   ("M-<return>" . minibuffer-force-complete-and-exit))
  :ensure t
  :demand t)

;; save search history
(use-package
  savehist
  :init (savehist-mode 1)
  :ensure nil
  :demand t)

(use-package
  tiny
  :demand t
  :ensure t
  :bind (("C-x t" . tiny-expand)))

(use-package
  rust-mode
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (defun tj-rust-hook ()
    (setq
     rust-format-on-save t
     indent-tabs-mode nil))
  :hook ((rust-mode . tj-rust-hook))
  :ensure t
  :demand t)

(use-package
  flycheck-vale
  :config (flycheck-vale-setup)
  :ensure t
  :demand t)

(use-package
  proced-narrow
  :after proced
  :bind (:map proced-mode-map ("/" . proced-narrow))
  :ensure t
  :demand t)

(use-package go-mod :ensure nil :demand t)

(use-package
  shim
  :ensure (:type git :host github :repo "twlz0ne/shim.el")
  :after projectile
  :config (shim-init-go)
  :demand t)

(use-package
  eglot
  :ensure-system-package ((gopls . "go install golang.org/x/tools/gopls@latest"))
  :custom
  (eglot-report-progress nil)
  :config
  (setq eglot-extend-to-xref t)
  (setq eglot-confirm-server-initiated-edits nil)
  (setq-default eglot-workspace-configuration
                '((:gopls . ((gofumpt . t) (staticcheck . t)))))
  (defun tj-eglot-organize-imports ()
    (interactive)
    (eglot-code-actions nil nil "source.organizeImports" t))
  (add-hook 'before-save-hook 'tj-eglot-organize-imports nil t)
  (add-hook 'before-save-hook 'eglot-format-buffer)
  :bind (("C-c C-r" . eglot-rename))
  :hook
  (go-mode . eglot-ensure)
  (java-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  :ensure t
  :demand t)

(use-package
  lice
  :config
  (define-derived-mode
    license-mode
    fundamental-mode
    "License"
    "Major mode for editing LICENSE files."
    (setq comment-start nil))
  (add-to-list 'auto-mode-alist '("LICENSE\\'" . license-mode))
  :ensure t
  :demand t)

(use-package
  hideshow
  :ensure nil
  :diminish hs-minor-mode
  :demand t)

(use-package
  kubernetes
  :commands (kubernetes-overview)
  :bind
  (("C-c C-k d" . kubernetes-dispatch)
   ("C-c C-k o" . kubernetes-overview))
  :config
  (setq
   kubernetes-poll-frequency 5
   kubernetes-redraw-frequency 5)
  :ensure t
  :demand t)

(use-package
  projectile
  :diminish
  :config
  (setq
   projectile-enable-caching t
   projectile-indexing-method 'alien
   projectile-mode-line nil
   projectile-sort-order 'modification-time
   projectile-switch-project-action #'projectile-commander)

  (add-to-list
   'projectile-globally-ignored-directories "Godeps/_workspace")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list
   'projectile-globally-ignored-directories "node_modules")

  (projectile-global-mode 1)

  (def-projectile-commander-method
   ?a
   "Run ripgrep on project."
   (call-interactively #'projectile-ripgrep))

  :bind
  (("C-x p t" . projectile-toggle-between-implementation-and-test)
   ("C-x p p" . projectile-switch-project)
   ("C-x p f" . projectile-find-file)
   ("C-x p c" . projectile-compile-project)
   ("C-x p i" . projectile-invalidate-cache))
  :ensure t
  :demand t)

(use-package
  ledger-mode
  :ensure t
  :demand t
  :init (setq ledger-clear-whole-transactions 1)
  :mode
  (("\\.ledger\\'" . ledger-mode) ("\\.dat\\'" . ledger-mode)))

(use-package
  delsel
  :ensure nil ; no need to install it as it is built-in
  :hook (after-init . delete-selection-mode))

(use-package
  elmacro
  :ensure t
  :demand t
  :diminish
  :config (elmacro-mode 1))

(use-package typit :ensure t :demand t)

(defun tj-raise-frame-and-give-focus ()
  (when window-system
    (let ((mouse-autoselect-window 1))
      (select-frame-set-input-focus (selected-frame)))))

(add-hook 'server-switch-hook 'tj-raise-frame-and-give-focus)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package
  server
  :no-require
  :hook ((after-init . server-start))
  :ensure nil
  :demand t)

(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
