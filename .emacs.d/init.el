(require 'cl)

(add-to-list 'load-path "~/.emacs.d/site-lisp/slime")
(add-to-list 'load-path "~/.emacs.d/site-lisp/swank-js")
(require 'slime)
(slime-setup '(slime-repl slime-js))

(global-set-key [f5] 'slime-js-reload)
(add-hook 'js3-mode-hook
          (lambda ()
            (slime-js-minor-mode 1)))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-eshell
                      slime
                      scpaste
                      slime-fuzzy
                      slime-repl
                      clojure-mode
                      ruby-electric
                      clojure-test-mode
                      eldoc-eval
                      durendal
                      move-text
                      diminish
                      whole-line-or-region
                      markdown-mode
                      marmalade
                      browse-kill-ring
                      textmate
                      full-ack
                      wrap-region
                      magit
                      coffee-mode
                      rinari
                      browse-url
                      anything
                      anything-match-plugin
                      js2-mode
                      ace-jump-mode
                      autopair
                      tumble
                      iy-go-to-char
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun site-lisp (library)
  (interactive)
  (let* ((file (symbol-name library))
         (normal (concat "~/.emacs.d/site-lisp/" file))
         (normal-file (concat "~/.emacs.d/site-lisp/" file ".el"))
         )
    (cond
     ((file-directory-p normal) (add-to-list 'load-path normal) (require library))
     ((file-directory-p normal-file) (add-to-list 'load-path normal-file) (require library))
     ((file-directory-p normal-file) (require library)))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(require 'auto-complete-config)
(require 'auto-complete)

(setq-default save-place 1)

(setenv "PATH"
        (concat
         (expand-file-name "~/bin") ":"
         "/usr/local/bin" ":"
         "/usr/bin" ":"
         "/bin" ":"))

(setq exec-path
      '((expand-file-name "~/bin")
        "/usr/local/bin"
        "/usr/bin"
        "/bin"))

(let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
  (when (file-readable-p plist)
    (let ((dict (cdr (assq #'dict (cdar (xml-parse-file plist))))))
      (while dict
        (if (and (listp (car dict))
                 (eq 'key (caar dict)))
            (setenv (car (cddr (car dict)))
                    (car (cddr (car (cddr dict))))))
        (setq dict (cdr dict))))

    ;; Configure exec-path based on the new PATH
    (setq exec-path nil)
    (mapc #'(lambda (path)
              (add-to-list 'exec-path path))
          (nreverse (split-string (getenv "PATH") ":")))))

(set-default-font "Inconsolata-14")

(setq browse-url-browser-function 'browse-url-default-macosx-browser
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ido-handle-duplicate-virtual-buffers 2
      rinari-tags-file-name "TAGS"
      auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.god" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ru" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.gemspec" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ronn?" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.js$" . js3-mode) auto-mode-alist)
      coffee-tab-width 2
      ack-prompt-for-directory 1
      erc-nick "travisjeffery"
      erc-autojoin-channels-alist '(("freenode.net" "#github" "#logicalawesome"
                                     "#rip" "#resque" "#{" "#sinatra" "#redis"
                                     "#coffeescript" "#math" "#vim" "#emacs"
                                     "##javascript" "#thechangelog" "#clojure"))
      scroll-margin 30
      ;; from the docs: If the value is greater than 100, redisplay will never recenter point,
      ;; but will always scroll just enough text to bring point into view, even
      ;; if you move far away.
      scroll-conservatively 101
      scroll-preserve-screen-position 1

      backup-by-copying 1
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory 1))
      delete-old-versions 1
      kept-new-versions 6
      kept-old-versions 2
      version-control 1
      fill-column 79
      autopair-autowrap 1
      cua-enable-cua-keys 0
      ediff-split-window-function 'split-window-vertically
      zencoding-preview-default 0
      linum-format " %d "
      use-dialog-box 0
      js3-consistent-level-indent-inner-bracket 1
      )

(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(global-set-key (kbd "C-c p") 'duplicate-line)

(add-to-list 'load-path "~/.emacs.d/site-lisp/ac-slime")
(add-to-list 'load-path "~/.emacs.d/site-lisp/hippie-expand-slime")
(add-to-list 'load-path "~/.emacs.d/site-lisp/elisp-slime-nav")
(global-set-key (kbd "M-'") 'comment-add)
;; (add-to-list 'Load-path "~/.emacs.d/site-lisp/hl-sexp")
(site-lisp 'hl-sexp)
(require 'hl-sexp)
(require 'elisp-slime-nav)
(require 'hippie-expand-slime)
(require 'ac-slime)
(add-to-list 'load-path "~/.emacs.d/")

(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp)

(site-lisp 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

(add-to-list 'load-path "~/.emacs.d/site-lisp/eproject")
(require 'eproject)
;; (eproject-maybe-turn-on)

(add-to-list 'load-path "~/.emacs.d/site-lisp/js3-mode")
(autoload 'js3-mode "js3" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))

(add-to-list 'load-path "~/.emacs.d/site-lisp/flymake-coffee")
(site-lisp 'flymake-coffee)

(add-to-list 'load-path "~/.emacs.d/site-lisp/git-emacs")
(require 'git-emacs)
(require 'git-blame)

(require 'autopair)

(add-to-list 'load-path "~/.emacs.d/site-lisp/twittering-mode")
(require 'twittering-mode)

(add-to-list 'load-path "~/.emacs.d/site-lisp/jade-mode")
(require 'sws-mode)
(require 'jade-mode)
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))

(require 'init-clojure)
(require 'init-flyspell)

(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill))

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

(global-set-key (kbd "C-c r") 'query-replace-regexp)
(defalias 'qrr 'query-replace-regexp)

(defun dos2unix ()
  (interactive)
  (beginning-of-buffer)
  (while
      (search-forward "\r")
    (replace-match "")))

(add-to-list 'load-path "~/.emacs.d/site-lisp/key-chord")
(require 'key-chord)
(key-chord-mode 1)


(global-auto-revert-mode 1)
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)
(setq x-select-enable-clipboard t)

(require 'init-editing-utils)
(require 'init-utils)
(require 'init-slime)

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(eval-after-load "anything"
  '(require 'anything-match-plugin))

(global-set-key (kbd "C-\\")  'comment-or-uncomment-region-or-line)

(add-to-list 'load-path "~/.emacs.d/site-lisp/nxhtml")

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map [?l] 'dired-up-directory)
     (define-key dired-mode-map [tab] 'other-window)
     (define-key dired-mode-map [(meta ?s) ?f] 'find-grep)))

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c R") 'replace-string)
(global-set-key (kbd "M-z") 'zap-upto-char)
(global-set-key (kbd "M-i") 'textmate-goto-symbol)
(global-set-key (kbd "A-F") 'ack)
(global-set-key (kbd "C-TAB") 'other-window)
;; (global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-*") 'isearch-forward-at-point)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-unset-key (kbd "C-x m"))

(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))
(add-hook 'js3-mode-hook '(lambda ()
                            (paredit-mode -1)
                            (autopair-mode 1)))
(add-hook 'coffee-mode-hook '(lambda ()
                               (paredit-mode -1)
                               (autopair-mode 1)))

(require 'peg)
(add-to-list 'load-path "~/.emacs.d/site-lisp/haskell-emacs/src")
(require 'hs)
(site-lisp 'haskell-mode)
(require 'haskell-mode)
(require 'haskell-indent)
;; (add-hook 'hs-mode-hook 'hs-mode-start-hook)
(add-hook 'hs-mode-hook '(lambda ()
                           (turn-on-haskell-indent)
                           (haskell-indent-mode -1)
                           (haskell-indentation-mode 1)
                           (paredit-mode -1)
                           (autopair-mode)))

;; Setup associations with file types.
(add-to-list 'auto-mode-alist (cons "\\.hs\\'" 'hs-mode))
(add-to-list 'auto-mode-alist (cons "\\.cabal\\'" 'hs-cabal-mode))
(add-to-list 'auto-mode-alist '("\\.hcr\\'" . hs-core-mode))

(require 'init-javascript)
(require 'init-rails)
(autopair-global-mode)
(cua-mode 1)

(defvar autopair-modes '(r-mode ruby-mode js-mode python-mode html-mode))
(defun turn-on-autopair-mode () (autopair-mode 1))
(dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))
(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on"
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)))
(ad-activate 'paredit-mode)
(add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/solarized")
(add-to-list 'load-path "~/.emacs.d/site-lisp/solarized")
(load-theme 'solarized-light 1)
(fringe-mode 0)

(textmate-mode)

(add-to-list 'load-path "~/.emacs.d/site-lisp/zencoding")
(require 'zencoding-mode)
(add-hook 'nxhtml-mode-hook 'zencoding-mode)
(add-hook 'zencoding-mode-hook '(lambda ()
                                  (define-key zencoding-mode-map (kbd "C-c ,") 'zencoding-expand-line)))

(add-hook 'python-mode-hook '(lambda ()
                               (textmate-mode)
                               (esk-paredit-nonlisp)
                               (electric-layout-mode)
                               (flyspell-prog-mode)))
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda ()
                             (textmate-mode)
                             (esk-paredit-nonlisp)
                             (electric-layout-mode)
                             (ruby-electric-mode)
                             (flyspell-prog-mode)))
(add-hook 'emacs-lisp-mode-hook '(lambda ()
                                (enable-paredit-mode)
                                (electric-layout-mode)
                             
                                (flyspell-prog-mode)))


(global-linum-mode)
(wrap-region-global-mode)

(add-hook 'coffee-mode-hook '(lambda ()
                               (define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-file)
                               (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-repl)
                               (esk-paredit-nonlisp)))

(defun zap-upto-char (arg char)
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input) char)))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (- (point) 1)))
  (backward-char 1))

(defun clean-slate ()
  (interactive)
  (let ((buffers (buffer-list)) (safe '("*scratch*")))
    (while buffers
      (when (not (member (car buffers) safe))
        (kill-buffer (car buffers))
        (setq buffers (cdr buffers))))))

(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-ste-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let * ((end (progn (skip-syntax-forward "w_") (point)))
            (begin (progn (skip-syntax-backward "w_") (point))))
         (if (eq begin end)
             (isearch-forward regexp-p no-recursive-edit)
           (setq isearch-initial-string (buffer-substring begin end))
           (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
           (isearch-forward regexp-p no-recursive-edit)))))

(defun isearch-occur ()
  "Invoike `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote iserach-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(defun insert-emdash ()
  (interactive)
  (insert "—"))
(global-set-key (kbd "C-M-_") 'insert-emdash)

(global-set-key (kbd "C-M-=") 'esk-indent-buffer)

(add-hook 'js-mode-hook (lambda ()
                          (defun js--proper-indentation (parse-status)
                            "Return the proper indentation for the current line."
                            (save-excursion
                              (back-to-indentation)
                              (cond
                               ((looking-at ",")
                                (let ((spos
                                       (save-excursion
                                         (while (looking-back "}[\t\n ]*")
                                           (backward-sexp)
                                           (if (looking-back ",[\t\n ]*")
                                               (re-search-backward ",[\t\n ]*")))

                                         (cond
                                          ((looking-back "\\(,\\|(\\)[ \t]*\\<\\([^:=\n\t ]+\\)[ \t\n]*")
                                           (re-search-backward "\\(,\\|(\\)[ \t]*\\<\\([^:=\n\t ]+\\)[ \t\n]*" (point-min) t)
                                           (+ (current-column) 2))

                                          ((re-search-backward "\\<\\([^:=\n\t ]+\\)[ \t]*\\(:\\|=\\)" (point-min) t)
                                           (current-column))
                                          (t
                                           nil)))))
                                  (if spos
                                      (- spos 2)
                                    (+ js-indent-level js-expr-indent-offset))))
                               ((nth 4 parse-status)
                                (js--get-c-offset 'c (nth 8 parse-status)))
                               ((nth 8 parse-status) 0) ; inside string
                               ((js--ctrl-statement-indentation))
                               ((eq (char-after) ?#) 0)
                               ((save-excursion (js--beginning-of-macro)) 4)
                               ((nth 1 parse-status)
                                ;; A single closing paren/bracket should be indented at the
                                ;; same level as the opening statement. Same goes for
                                ;; "case" and "default".
                                (let ((same-indent-p (looking-at
                                                      "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                                      (continued-expr-p (js--continued-expression-p)))
                                  (goto-char (nth 1 parse-status)) ; go to the opening char
                                  (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                                      (progn ; nothing following the opening paren/bracket
                                        (skip-syntax-backward " ")
                                        (when (eq (char-before) ?\)) (backward-list))
                                        (back-to-indentation)
                                        (cond (same-indent-p
                                               (current-column))
                                              (continued-expr-p
                                               (+ (current-column) (* 2 js-indent-level)
                                                  js-expr-indent-offset))
                                              (t
                                               (+ (current-column) js-indent-level
                                                  (case (char-after (nth 1 parse-status))
                                                    (?\( js-paren-indent-offset)
                                                    (?\[ js-square-indent-offset)
                                                    (?\{ js-curly-indent-offset))))))
                                    ;; If there is something following the opening
                                    ;; paren/bracket, everything else should be indented at
                                    ;; the same level.
                                    (unless same-indent-p
                                      (forward-char)
                                      (skip-chars-forward " \t"))
                                    (current-column))))

                               ((js--continued-expression-p)
                                (+ js-indent-level js-expr-indent-offset))
                               (t 0))))))

(defun goto-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))


(setq textmate-find-files-command "git ls-tree --full-tree --name-only -r HEAD")
(load custom-file 'noerror)

(site-lisp 'expand-region)
(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)

(eshell)
(with-current-buffer "*eshell*" (setq pcomplete-cycle-completions nil))
(set-face-foreground 'eshell-prompt "#2075c7")

(global-set-key (kbd "C-c s") 'mark-sexp)
(global-set-key (kbd "M-@") 'mark-sexp)

(add-to-list 'load-path "~/.emacs.d/site-lisp/gist.el")
(require 'gist)

(require 'init-auto-complete)
(global-auto-complete-mode)
(require 'init-lisp)
(require 'gnus)
(setq nnml-directory "~/gmail")
(setq message-directory "~/gmail")
(setq gnus-ignored-newsgroups "^to\\.\\|^[0-9. ]+\\( \\|$\\)\\|^[\”]\”[#’()]")

(setq gnus-secondary-select-methods
      '((nnimap "home"
                (nnimap-address "imap.server1.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))
        (nnimap "work"
                (nnimap-address "imap.server2.com")
                (nnimap-server-port 993)
                (nnimap-stream ssl)
                (nnimap-authinfo-file "~/.authinfo"))))

(add-to-list 'load-path "~/.emacs.d/site-lisp/find-file-in-project")
(require 'find-file-in-project)
(setq ffip-try-tags 0)
(setq ido-work-directory-list-ignore-regexps '("\\.git$"))

(defun convert-markdown-to-textfile ()
  "Convert the markdown file that you're current editing into a textile file. Note: requires `pandoc` to be installed."
  (interactive)
  (let ((markdown-file-name (buffer-file-name))
        (textile-file-name (concat (file-name-sans-extension (buffer-file-name)) ".textile")))
    (if (executable-find "pandoc")
        (progn
          (shell-command (format "pandoc -s %s -o %s"  markdown-file-name textile-file-name))
          (find-file textile-file-name))
      (error "%s" "The pandoc executable is required and either can't be found or is not installed"))))


;; ----------
;; -- Keybindings
;; ----------

;; -- Setup

(defvar my-keymap (make-keymap)
  "A keymap containing my custom keybindings.")

(define-minor-mode my-keys-mode
  "A minor mode that encapsulates my custom keybindings."
  :init-value t
  :keymap my-keymap)

(defmacro my-with-keymap (keymap &rest body)
  "Within BODY, `my-key' defines keys on KEYMAP by default."
  (declare (indent 1))
  `(let ((my-keymap ,keymap)) ,@body))

(defmacro my-key (key fn &optional map)
  `(define-key ,(or map 'my-keymap) (kbd ,key) ',fn))

(defmacro my-map (key name)
  (let ((varname (intern (concat (symbol-name name) "-map"))))
    `(progn
       (define-prefix-command ',name ',varname)
       (define-key my-keymap (kbd ,key) ,varname))))

(defmacro my-unset (key)
  `(global-unset-key (kbd ,key)))

(defmacro my-strong-unset (key)
  `(my-key ,key keyboard-quit))


;; -- Terminal hacks
;;
;; The input standard for terminals, having evolved in a time before graphical
;; interfaces, doesn't have nearly the same level of support for modifier keys
;; as X does. Since I use all sorts of modifier keys, often in combination with
;; one another, this presents a problem. How do I get bindings involving two or
;; more of control, meta, and shift to work?
;;
;; Luckily, the X toolkit and xterm (which uses it) are vastly customizable.
;; Through the Xresources config file, which is fed to xrdb, I can tell xterm
;; that any key event (involving as many modifiers as I want) should produce
;; any arbitrary string. In addition, Emacs has low-level but lisp-accessible
;; facilities for saying that a particular string of characters should translate
;; to a particular key sequence.

(setf (cddr key-translation-map)
      (eval-when-compile
        (let ((chars (string-to-list "qwertyuiop]\\asdfghjklzxcvbnm"))
              (meta (string-to-char (kbd "ESC"))))
          (flet ((add-prefix (prefix char)
                             (read-kbd-macro (concat prefix "-" (char-to-string char))))
                 (make-bindings (binding-prefix)
                                (mapcar
                                 (lambda (char)
                                   (cons (string-to-char (add-prefix "C" char))
                                         (add-prefix binding-prefix char)))
                                 chars)))
            ;; C-M-S- bindings are a slightly weird case. None of them work by
            ;; default, so we have to bind them en masse. In addition, they're
            ;; actually included in two of the subdivisions below. The A keymap
            ;; is the "correct" keymap for them, but they also appear in the S
            ;; keymap. This is because, since we emit the S prefix for *all*
            ;; C-S- characters, typing C-S-M (in that order) will generate the S
            ;; prefix before generating the full prefix. Thus we have to process
            ;; the A prefix *after* processing the S sequence.
            (let ((cms-bindings
                   `(,meta keymap ,@(make-bindings "C-M-S")
                           (?: . ,(kbd "C-M-:"))
                           (?\s . ,(kbd "C-M-S-SPC")))))
              ;; I reserve the prefix M-& for my translation map, because it's a
              ;; pain to reach anyway.
              `((,meta keymap
                       (?& keymap
                           ;; The C keymap handles C-M- bindings. There are only
                           ;; a couple of these, since C-M- works without issue
                           ;; for alphabetic characters.
                           (?C keymap
                               (?\; . ,(kbd "C-M-;"))
                               (?. . ,(kbd "C-M-.")))
                           ;; The M keymap handles M-S- bindings. There's only
                           ;; one of these, since M-S- works almost all the time.
                           (?M keymap
                               (?\s . ,(kbd "M-S-SPC")))
                           ;; The S keymap handles C-S- and (sometimes) C-M-S
                           ;; bindings. All C-S- bindings we care about don't
                           ;; work, so we bind them en masse.
                           (?S keymap
                               (,meta keymap (?& keymap (?A keymap ,cms-bindings)))
                               ,@(make-bindings "C-S"))
                           ;; The A keymap (sometimes) handles C-M-S- bindings.
                           (?A keymap
                               ,cms-bindings)))))))))

(define-key isearch-mode-map (kbd "M-n") 'isearch-delete-char)
(define-key isearch-mode-map (kbd "M-O") 'isearch-ring-advance)
(define-key isearch-mode-map (kbd "M-I") 'isearch-ring-retreat)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(defun x-clipboard-only-yank ()
  "Insert the clipboard contents (but never killed text) at the mark"
  (interactive)
  (insert (x-get-clipboard)))

(add-to-list 'load-path "~/.emacs.d/site-lisp/ergoemacs-keybindings")
(setenv "ERGOEMACS_KEYBOARD_LAYOUT" "us")
(load "~/.emacs.d/site-lisp/ergoemacs-keybindings/ergoemacs-mode")
(ergoemacs-mode 1)

(add-to-list 'load-path "~/.emacs.d/site-lisp/evil")
(add-to-list 'load-path "~/.emacs.d/site-lisp/surround")
(require 'evil)
(require 'surround)
(global-surround-mode 1)
;; (evil-mode 1)

(add-to-list 'load-path "~/.emacs.d/site-lisp/undo-tree")
(require 'undo-tree)
(global-undo-tree-mode 1)

(define-key ergoemacs-keymap (kbd "M-/") nil)
(define-key ergoemacs-keymap (kbd "M-/") 'hippie-expand)

(define-key ergoemacs-keymap [C-tab] 'paredit-splice-sexp)

(defun new-line-below-current ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(define-key ergoemacs-keymap (kbd "M-RET") 'new-line-below-current)

(defun new-line-above-current ()
  (interactive)
  (previous-line 1)
  (end-of-line)
  (newline-and-indent))
(define-key ergoemacs-keymap [(control return)] 'new-line-above-current)

(define-key ergoemacs-keymap (kbd "M-t") 'textmate-goto-symbol)
(define-key ergoemacs-keymap (kbd "M-T") 'textmate-goto-file)

