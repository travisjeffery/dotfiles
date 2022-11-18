(defvar bootstrap-version)

(let ((bootstrap-file (expand-file-name
                       "straight/repos/straight.el/bootstrap.el"
                       user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent
         'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(use-package use-package-ensure-system-package
  :straight t)

(use-package no-littering
  :after recentf
  :config
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))))

;; utility functions e.g. f-directory? etc.
(use-package f)

(use-package bufler)

;; add header and footers to files.
(use-package header2)

;; mainly use these two to hide regions matching regexps
(use-package zones)
(use-package isearch-prop)
(use-package isearch
  :straight (:type built-in)
  :hook (isearch-mode-end . (lambda ()
                              (when (and isearch-forward
                                         (number-or-marker-p isearch-other-end)
                                         (not mark-active)
                                         (not isearch-mode-end-hook-quit))
                                (goto-char isearch-other-end)))))

(require 'f)

(eval-and-compile
  (let ((lisp-dir (format "%s%s" user-emacs-directory "lisp")))
    (setq load-path
          (append
           (delete-dups load-path)
           (list lisp-dir)
           (f-directories lisp-dir)))))

(use-package bind-key)

(setq use-package-verbose t)

(use-package sqlformat
  :config
  (setq sqlformat-command 'pgformatter
        sqlformat-args '("-B" "-e")))

(eval-and-compile
  (mapc
   (lambda (entry)
     (define-prefix-command (cdr entry))
     (bind-key (car entry) (cdr entry)))
   '
   (("C-;" . tj-ctrl-semicolon-map) ;; mc
    ("C-c b" . tj-ctrl-c-b-map) ;; for bm
    ("C-c m" . tj-ctrl-c-m-map) ;; for org
    ("C-c o" . tj-ctrl-c-o-map) ;; for occur
    ("C-c y" . tj-ctrl-c-y-map) ;; aya
    ("C-c C-d" . tj-ctrl-c-c-c-d-map) ("M-i" . tj-m-i-map) ("M-o" . tj-m-o-map))))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode +1)
  :bind (:map undo-tree-map
              (("C-/" . nil))))

(use-package projectile
  :config

  (setq projectile-enable-caching t
        projectile-indexing-method 'alien
        projectile-mode-line nil
        projectile-sort-order 'modification-time
        projectile-switch-project-action #'projectile-commander)
  
  (add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")
  
  (projectile-global-mode 1)

  (def-projectile-commander-method ?a
    "Run ripgrep on project."
    (call-interactively #'projectile-ripgrep))
  
  :bind
  (("C-x p t" . projectile-toggle-between-implementation-and-test)
   ("C-x p p" . projectile-switch-project)
   ("C-x p f" . projectile-find-file)
   ("C-x p i" . projectile-invalidate-cache)))

(use-package visual-fill-column
  :config (add-hook 'text-mode-hook 'visual-line-mode))

(use-package lisp-mode
  :straight (:type built-in)
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
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package browse-kill-ring
  :config
  (defun tj-replace-blank-kill (args)
    (let ((string (car args))
          (replace (cdr args))
          (last (car-safe kill-ring)))
      (when (and last (string-blank-p last))
        (setq replace t))
      (list string replace)))
  (advice-add 'kill-new :filter-args #'tj-replace-blank-kill)
  :bind
  (("M-y" . browse-kill-ring)))

(use-package ielm
  :config (add-hook 'ielm-mode-hook #'eldoc-mode))

(use-package avy
  :bind (("M-T" . avy-goto-word-1) ("<C-return>" . avy-goto-char-timer))
  :config
  (avy-setup-default)
  (setq avy-background t))

(use-package ipcalc)

(use-package cider)

(use-package org-agenda-property)

(use-package clj-refactor)

(use-package clojure-mode
  :after smartparens
  :after flycheck-clj-kondo
  :config
  (setq clojure-indent-style 'always-align)
  :hook
  ((clojure-mode . eldoc-mode)
   (inf-clojure-mode . eldoc-mode)))

(use-package dashboard
  :init (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))
  :config
  (setq dashboard-banner-logo-title "Do the work."
        dashboard-startup-banner nil)
  (dashboard-setup-startup-hook))

(use-package magit
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  (setq vc-follow-symlinks t
        magit-refresh-status-buffer t)
  
  (magit-define-section-jumper
    magit-jump-to-recent-commits
    "Recent commits"
    recent
    "HEAD~10..HEAD")
  
  (defun tj-visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format
      "https://github.com/%s/pull/new/%s"
      (replace-regexp-in-string
       "\\`.+github\\.com:\\(.+\\)\\.git\\'"
       "\\1"
       (magit-get "remote" (magit-get-push-remote) "url"))
      (magit-get-current-branch))))
  (eval-after-load
      'magit
    '(define-key magit-mode-map "v" #'tj-visit-pull-request-url))

  (defun magit-key-mode--add-default-options (arguments)
    (if (eq (car arguments) 'pulling)
        (list 'pulling (list "--rebase"))
      arguments)
    (if (eq (car arguments) 'pushing)
        (list 'pushing (list "-u"))
      arguments))
  (advice-add 'magit-key-mode :filter-args #'magit-key-mode--add-default-options)
  :bind (:map magit-diff-mode-map (("C-o" . magit-diff-visit-file-other-window)))
  :bind (("C-x g" . magit-status) ("C-c g" . magit-file-dispatch)))

(use-package magit-diff-flycheck)

(use-package forge
  :config
  (setq
   forge-topic-list-limit
   '(3 . -1)
   forge-pull-notifications nil))

(use-package copy-as-format
  :init (setq copy-as-format-default "github"))

(use-package abbrev
  :straight (:type built-in)
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package compile
  :init (require 'grep)
  :no-require
  :bind (("C-c c" . compile) ("M-O" . show-compilation))
  :bind
  (:map
   compilation-mode-map
   (("z" . delete-window) ("RET" . tj-compile-goto-error-same-window)))
  :bind (:map compilation-minor-mode-map ("RET" . tj-compile-goto-error-same-window))
  :bind (:map compilation-button-map ("RET" . tj-compile-goto-error-same-window))
  :bind (:map grep-mode-map ("RET" . tj-compile-goto-error-same-window))
  :preface
  (defun tj-compile-goto-error-same-window ()
    (interactive)
    (let
        (
         (display-buffer-overriding-action
          '
          ((display-buffer-reuse-window display-buffer-same-window)
           (inhibit-same-window . nil))))
      (call-interactively #'compile-goto-error)))
  (defun show-compilation ()
    (interactive)
    (let
        (
         (it
          (catch 'found
            (dolist (buf (buffer-list))
              (when (string-match "\\*compilation\\*" (buffer-name buf))
                (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))
  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start) (point-marker)))
  :hook ((compilation-filter . compilation-ansi-color-process-output)))

(use-package comint
  :straight (:type built-in)
  :config (setq shell-prompt-pattern "^; "))

(use-package dired-toggle
  :preface
  (defun tj-dired-toggle-mode-hook ()
    (interactive)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow)
                word-wrap nil))
  :hook (dired-toggle-mode . tj-dired-toggle-mode-hook))

(use-package dired-narrow)

(use-package dired-filter)

(use-package git-timemachine
  :commands git-timemachine
  :bind (("s-g" . git-timemachine)))

(use-package smart-forward
  :config
  :bind
  (("M-<up>" . smart-up)
   ("M-<down>" . smart-down)
   ("M-<left>" . smart-backward)
   ("M-<right>" . smart-forward)))

(use-package ibuffer
  :straight (:type built-in)
  :config
  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-formats '((mark " "
                                (name 16 -1)
                                " " filename)
                          (mark modified read-only locked " "
                                (name 18 18 :left :elide)
                                " "
                                (size 9 -1 :right)
                                " "
                                (mode 16 16 :left :elide)
                                " " filename-and-process)))
  (setq ibuffer-saved-filter-groups nil))

(use-package re-builder
  :bind (:map reb-mode-map ("M-%" . reb-query-replace))
  :config
  (setq reb-re-syntax 'string)
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (progn
       (barf-if-buffer-read-only)
       (list
        (query-replace-read-to (reb-target-binding reb-regexp) "Query replace" t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp (reb-target-binding reb-regexp) to-string))))

(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region)))

;; hashicorp config language
(use-package hcl-mode)

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode)
  :config
  (defun tj-response-loaded-hook () (flycheck-mode 0))
  (add-hook 'restclient-response-loaded-hook 'tj-response-loaded-hook)
  (defun tj-restclient-hook () (setq-local indent-line-function 'js-indent-line))
  (add-hook 'restclient-mode-hook 'tj-restclient-hook))

(use-package ediff
  :config
  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff
     ediff-current-difference nil 'C nil
     (concat
      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
  (defun add-d-to-ediff-mode-map ()
    (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
  :init
  (setq ediff-split-window-function 'split-window-vertically
        ediff-merge-split-window-function 'split-window-vertically
        ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-diff-options ""
        ediff-custom-diff-options "-u")
  :bind
  (("C-c = b" . ediff-buffers)
   ("C-c = f" . ediff-files)
   ("C-c = r" . ediff-revision)
   ("C-c = l" . ediff-regions-linewise)
   ("C-c = w" . ediff-regions-wordwise)
   ("C-c = c" . compare-windows)))

(use-package git-link
  :commands (git-link git-link-commit git-link-homepage)
  :bind ("C-c G" . git-link))

(use-package git-modes)

(use-package github-pullrequest
  :commands (github-pullrequest-new github-pullrequest-checkout))

(use-package gitpatch
  :commands gitpatch-mail)

(use-package flycheck-clj-kondo)

(use-package google-this
  :bind ("C-c /" . google-this-search))

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package ialign
  :bind ("C-c [" . ialign-interactive-align))

(use-package operate-on-number
  :bind ("C-c #" . operate-on-number-at-point))

(use-package shift-number
  :bind (("C-c -" . shift-number-down) ("C-c +" . shift-number-up)))

(use-package diminish :demand t)

(use-package rjsx-mode
  :config (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package company-quickhelp :defer t)

(eval-after-load
    'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package eldoc
  :diminish
  :config
  (setq eldoc-echo-area-use-multiline-p nil)
  :bind
  (("C-c C-c" . eldoc)))

(use-package godoctor
  :after go-mode)

(use-package go-add-tags
  :after go-mode)

(use-package gotest
  :after go-mode)

(use-package go-errcheck
  :ensure-system-package ((go-errcheck . "go install github.com/kisielk/errcheck@latest"))
  :after go-mode-abbrev-table
  :config
  (defun tj-go-errcheck ()
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (go-errcheck nil nil nil))))

(use-package go-mode
  :init
  :ensure-system-package ((godef . "go install github.com/rogpeppe/godef@latest")
                          (goimports . "go install golang.org/x/tools/cmd/goimports@latest")
                          (golint . "go install golang.org/x/lint/golint@latest")
                          (gopls . "go install golang.org/x/tools/gopls@latest"))
  :bind
  (:map
   go-mode-map
   ("C-c C-c" . godoc-at-point)
   ("M-j" . comment-indent-new-line)
   ("M-b" . subword-backward)
   ("M-f" . subword-forward)
   ("M-d" . subword-kill)
   ("C-c C-t" . go-test-current-file)
   ("C-c M-t" . go-test-current-test)
   ("C-c C-e" . tj-go-err))
  :config
  
  
  (defun tj-go-err ()
    (interactive)
    (if (region-active-p)
        (let
            ((body (buffer-substring-no-properties (region-beginning) (region-end))))
          (goto-char (region-beginning))
          (delete-char (string-width body))
          (yas-expand-snippet
           (concat "if err := ${1:" body "}; err != nil {\n" "$0\n" "}")))
      (yas-expand-snippet (concat "if err != nil {\n$0\n}"))))
  
  (defun tj-find-go-project-root (dir)
    "Find go project root for DIR."
    (if
        (and
         dir
         (not
          (f-descendant-of-p
           dir
           (or (getenv "GOPATH") (concat (getenv "HOME") "/go")))))
        (let ((result (locate-dominating-file dir "go.mod")))
          (if result
              (cons 'transient (expand-file-name result))
            (cons 'transient dir)))
      (when dir
        (cons 'transient dir))))
  
  (setq gofmt-command "goimports"
        tab-width 8)
  (setq-local compilation-read-command nil)
  
  (defun tj-turn-on-gofmt-before-save ()
    (interactive)
    (add-hook 'before-save-hook 'gofmt t t))
  
  (defun tj-turn-off-gofmt-before-save ()
    (interactive)
    (remove-hook 'before-save-hook 'gofmt t))

  (set-face-foreground 'go-test--ok-face "forest green")
  (set-face-foreground 'go-test--standard-face "dark orange")

  (defun go-test-current-test ()
    "Launch go test on the current test."
    (interactive)
    (cl-destructuring-bind (test-suite test-name)
        (go-test--get-current-test-info)
      (let
          (
           (test-flag
            (if (> (length test-suite) 0)
                "-testify.m "
              "-run "))
           (additional-arguments
            (if go-test-additional-arguments-function
                (funcall go-test-additional-arguments-function test-suite test-name)
              "")))
        (when test-name
          (if (go-test--is-gb-project)
              (go-test--gb-start
               (s-concat "-test.v=true -test.run=" test-name "\\$ ."))
            (go-test--go-test
             (s-concat test-flag test-name additional-arguments "\\$ .")))))))
  
  (defun tj-go-hook ()
    (setq imenu-generic-expression
          '(("type" "^[ \t]*type *\\([^ \t\n\r\f]*[ \t]*\\(struct\\|interface\\)\\)" 1)
            ("func" "^func *\\(.*\\)" 1)))

    (setq-local project-find-functions (list #'tj-find-go-project-root #'project-try-vc))
    (setq case-fold-search t)
    (setq go-test-args "-timeout 60s -race -v")
    (font-lock-mode -1)
    (which-function-mode -1)
    (flycheck-mode 1)
    (tj-turn-on-gofmt-before-save)
    (highlight-symbol-mode)
    (subword-mode)
    (selected-minor-mode 1)
    (whitespace-mode -1)
    (electric-pair-mode 1)
    (smartparens-mode -1)
    
    (if (not (string-match "go" compile-command))
        (set
         (make-local-variable 'compile-command)
         "go build -v && go test -v && go vet")))
  
  :hook
  (go-mode . tj-go-hook))

(use-package winner
  :diminish
  :config (winner-mode +1)
  :bind (("M-[" . winner-undo) ("M-]" . winner-redo)))

(use-package eacl
  :config (setq eacl-grep-program "grep --exclude-dir=.git --exclude-dir=vendor")
  :bind (("C-x C-l" . eacl-complete-line)))

(use-package go-gen-test
  :after go-mode)

(use-package embrace
  :config (setq embrace-show-help-p nil)
  :bind ("C-c C-y" . embrace-commander))

(use-package ripgrep
  :config
  (setq ripgrep-arguments '("--hidden"))
  :bind
  :bind ("C-c a" . ripgrep-regexp))

(use-package web-beautify)

(use-package pt)

(use-package expand-region
  :config (er/add-html-mode-expansions)
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :diminish
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paren
  :diminish show-paren-mode
  :config
  (set-face-attribute 'show-paren-match nil :weight 'bold)
  (setq show-paren-style 'parenthesis)
  (setq show-paren-when-point-inside-paren t)
  (show-paren-mode +1))

(use-package uniquify
  :straight (:type built-in)
  :config
  (setq uniquify-buffer-name-style 'forward
        uniquify-separator "/"
        ;; rename after killing uniquified
        uniquify-after-kill-buffer-p t
        ;; don't muck with special buffers
        uniquify-ignore-buffers-re "^\\*"))

(use-package saveplace
  :diminish
  :straight (:type built-in)
  :config

  (defconst savefile-dir (expand-file-name "savefile" no-littering-var-directory))

  ;; create the savefile dir if it doesn't exist
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir))
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package hideshow
  :straight (:type built-in)
  :hook
  (prog-mode . hs-minor-mode))

(use-package recentf
  :config
  (setq
   recentf-save-file
   (expand-file-name "recentf" savefile-dir)
   recentf-max-saved-items 500
   recentf-max-menu-items 15
   ;; disable recentf-cleanup on Emacs start, because it can cause
   ;; problems with remote files
   recentf-auto-cleanup 'never)
  (recentf-mode +1)
  :bind
  (("C-x C-r" . recentf-open-files)))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package highlight-symbol
  :diminish
  :config (setq highlight-symbol-idle-delay 0.1)
  :bind (("M-p" . highlight-symbol-prev) ("M-n" . highlight-symbol-next))
  :hook
  (prog-mode . highlight-symbol-mode))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package dired
  :straight (:type built-in)
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
      (or (gethash pat mark-files-cache) (ignore (puthash pat t mark-files-cache)))))
  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))
  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list
      (read-directory-name
       "First directory: "
       (expand-file-name "~")
       nil
       nil
       "dl/")
      (read-directory-name
       "Second directory: "
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
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let
        (
         (files (dired-get-marked-files nil current-prefix-arg))
         (tmtxt/rsync-command "rsync -arvz --progress "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command (shell-quote-argument file) " ")))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))
  (defun ora-ediff-files ()
    (interactive)
    (let
        (
         (files (dired-get-marked-files))
         (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let
              (
               (file1 (car files))
               (file2
                (if (cdr files)
                    (cadr files)
                  (read-file-name "file: " (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      `
                      (lambda ()
                        (setq ediff-after-quit-hook-internal nil)
                        (set-window-configuration ,wnd))))
        (error "No more than 2 files should be marked"))))
  :config
  (defun dired-back-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))
  (define-key dired-mode-map (vector 'remap 'beginning-of-buffer)
    'dired-back-to-top)
  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))
  (define-key dired-mode-map (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x)
  (ignore-errors (unbind-key "M-s f" dired-mode-map))
  (defadvice dired-omit-startup
      (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode)
    dired-mode-map)
  (defadvice dired-next-line
      (around dired-next-line+ activate)
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
  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let
        (
         (file (expand-file-name ".git"))
         parent-dir)
      (while
          (and
           (not (file-exists-p file))
           (progn
             (setq parent-dir
                   (file-name-directory (directory-file-name (file-name-directory file))))
             ;; Give up if we are already at the root dir.
             (not (string= (file-name-directory file) parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let
              (
               (regexp (funcall dired-omit-regexp-orig))
               (omitted-files (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|"
                 "")
               "\\("
               (mapconcat
                #'
                (lambda (str)
                  (concat
                   "^"
                   (regexp-quote
                    (substring
                     str 13
                     (if (= ?/ (aref str (1- (length str))))
                         (1- (length str))
                       nil)))
                   "$"))
                (split-string omitted-files "\n" t) "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package dired-narrow
  :bind (:map dired-mode-map ("/" . dired-narrow)))

(use-package package-lint)

(use-package dired-ranger
  :bind
  (:map
   dired-mode-map
   ("W" . dired-ranger-copy)
   ("X" . dired-ranger-move)
   ("Y" . dired-ranger-paste)))

(use-package sh-mode
  :straight (:type built-in)
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

(use-package anzu
  :diminish
  :bind (("M-%" . anzu-query-replace-regexp) ("C-M-%" . anzu-query-replace))
  :hook (prog-mode . anzu-mode))

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package easy-kill-extras
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
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "")))

(use-package exec-path-from-shell
  :init
  (setq exec-path-from-shell-variables
        '("PATH" "MANPATH" "GOROOT" "GOPATH" "JAVA_HOME" "JAVA_OPTS" "RUST_SRC_PATH" "VAULT_ADDR" "GOPRIVATE"))
  :config (exec-path-from-shell-initialize))

(use-package kubedoc)

(use-package move-text
  :bind (("M-P" . move-text-up) ("M-N" . move-text-down)))

(use-package whitespace
  :config
  ;; (add-hook 'before-save-hook #'whitespace-cleanup)
  :config (setq whitespace-line-column 76) ;; limit line length
  (setq whitespace-style '(face empty lines trailing)))

(use-package docker)

(use-package dockerfile-mode
  :config (add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode)))

(use-package markdown-mode
  :defer 0
  :bind (:map markdown-mode-map ("C-c C-d" . nil))
  :config
  (unless (executable-find "pandoc")
    (message "install pandoc"))
  (setq markdown-command
        "pandoc --section-divs --from=markdown_github --highlight-style=haddock --self-contained -f markdown+smart --to=html5 --css=$HOME/.config/css/style.css")
  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :hook ((markdown-mode . writegood-mode)))

(use-package writegood-mode)

(use-package yaml-mode
  :mode ("\\.yaml" . yaml-mode))

(use-package org
  :straight (:type built-in)
  :hook ((org-mode . visual-line-mode)
         (org-mode . auto-fill-mode))
  :bind
  (("C-c m c" . org-capture)
   ("C-c m t" . org-todo-list)
   ("C-c m m" . orgs-tagsview)
   :map org-mode-map
   ("C-c C-d" . nil))  
  :config
  (defun tj-org-hook ()
    (setq org-hide-leading-stars nil))
  (setq org-todo-keywords
        (quote
         ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

  ;; where to archive subtree
  (setq org-archive-location "~/.archive.org::* Archived Tasks")
  
  (setq org-src-lang-modes
        '
        (("screen" . sh)
         ("ocaml" . tuareg)
         ("elisp" . emacs-lisp)
         ("lisp" . lisp)
         ("ditaa" . artist)
         ("asymptote" . asy)
         ("cl" . lisp)
         ("dot" . graphviz-dot)))
  
  (setq org-startup-folded nil)
  
  ;; enable org-indent-mode
  (setq org-startup-indented t)
  
  ;; but always to the left always
  (setq org-indent-indentation-per-level 0)
  
  ;; don't hide leading stars
  (setq org-hide-leading-stars nil)

  ;; save clock history across emacs sessions
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  
  (defun tj-org-capture ()
    (interactive)
    (find-file org-default-notes-file))

  ;; TAB in a code block behaves as if it were issues in the language major mode buffer.
  (setq org-src-tab-acts-natively t)

  (setq org-directory (expand-file-name "~/"))
  (setq org-default-notes-file (expand-file-name "~/notes.org"))
  
  (setq org-agenda-files '("~/"))
  
  ;; activate single letter commands at beginning of a headline.
  (setq org-use-speed-commands t)

  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  ;; use fast todo selection with C-c C-t
  (setq org-use-fast-todo-selection t)

  (setq org-refile-targets
        (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))
  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)
  (setq org-agenda-custom-commands
        (quote
         (("d" todo nil)
          ("c" todo "DONE|DEFERRED|CANCELLED" nil)
          ("w" todo "WAITING" nil)
          ("W" agenda "" ((org-agenda-ndays 21)))
          ("A"
           agenda ""
           (
            (org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
            (org-agenda-ndays 1)
            (org-agenda-overriding-header "Today's Priority #A tasks: ")))
          ("u"
           alltodo ""
           (
            (org-agenda-skip-function
             (lambda nil
               (org-agenda-skip-entry-if
                (quote scheduled)
                (quote deadline)
                (quote regexp)
                "\n]+>")))
            (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    (unless (eq org-journal-file-type 'daily)
      (org-narrow-to-subtree))
    (goto-char (point-max)))

  (setq org-capture-templates
        (quote
         (
          ("t"
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
  :hook (org-mode . tj-org-hook))

;; (use-package org-wild-notifier
;;   :config
;;   (setq org-wild-notifier-alert-time '(15 3 1))
;;   (setq org-wild-notifier-alert-times-property "NOTIFY_BEFORE")
;;   (setq alert-default-style 'libnotify)
;;   (org-wild-notifier-mode +1))

(use-package org-journal
  :after org
  :config
  (setq org-journal-dir "~/journal")
  (setq org-journal-file-format "%Y%m%d.org"))

(use-package org-web-tools
  :defer t)

(use-package alert
  :commands (alert)
  :init (setq alert-default-style 'notifier))

(use-package ert
  :defer t
  :bind
  ((:map ert-results-mode-map
         ("o" . #'ace-link-help))))

(use-package ace-link
  :after ert
  :config
  (ace-link-setup-default)
  :bind (("C-c M-o" . #'ace-link-addr)
         :map org-mode-map
         ("C-c C-o" . #'ace-link-org)
         :map gnus-summary-mode-map
         ("M-o" . #'ace-link-gnus)))

(use-package ace-mc
  :bind (("C-; h" . ace-mc-add-multiple-cursors) ("C-; M-h" . ace-mc-add-single-cursor)))

(use-package multiple-cursors
  :after selected
  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark))
  :config (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)
  :bind
  (("C-; C-;" . mc/edit-lines)
   ("C-; C-e" . mc/edit-ends-of-lines)
   ("C-; C-a" . mc/edit-beginnings-of-lines)
   ("C-; a" . mc/mark-all-dwim)
   ("C-; C-x" . reactivate-mark)
   ("C-; C-SPC" . mc/mark-pop)
   ("C-; f" . mc/mark-next-like-this-symbol)
   ("C-; b" . mc/mark-previous-like-this-symbol)
   ;; Extra multiple cursors stuff
   ("C-; %" . mc/insert-numbers)
   ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package mc-extras
  :after multiple-cursors
  :bind
  (("C-; M-C-f" . mc/mark-next-sexps)
   ("C-; M-C-b" . mc/mark-previous-sexps)
   ("C-; C-d" . mc/remove-current-cursor)
   ("C-; C-k" . mc/remove-cursors-at-eol)
   ("C-; M-d" . mc/remove-duplicated-cursors)
   ("C-; |" . mc/move-to-column)
   ("C-; ~" . mc/compare-chars)))

(use-package phi-search
  :bind (:map mc/keymap ("C-r" . phi-search-backward) ("C-s" . phi-search)))

(use-package journalctl-mode)

(use-package ace-jump-mode
  :defer t)

(use-package browse-url
  :bind (("C-c x" . browse-url-at-point)))

(defun eshell-execute-current-line ()
  "Insert current line at the end of the buffer."
  (interactive)
  (let
      (
       (command
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

(add-to-list 'completion-styles 'initials t)
(add-to-list 'completion-styles 'substring t)

(use-package company
  :diminish
  :hook (prog-mode . company-mode)
  :config (define-key company-active-map (kbd "<tab>") 'yas-next-field)
  (setq company-idle-delay nil)
  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test"))

(use-package cask-mode)

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char) ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :bind (("C-c i" . imenu-anywhere) ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq
   ispell-program-name
   "aspell" ; use aspell instead of ispell
   ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package flycheck
  :diminish
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable))
  (setq flycheck-idle-change-delay 4))

(use-package flycheck-rust
  :config
  (with-eval-after-load 'rust-mode
    (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(use-package crux
  :bind
  (("C-c d" . crux-duplicate-current-line-or-region)
   ("C-c n" . crux-cleanup-buffer-or-region)
   ("C-M-z" . crux-indent-defun)
   ("C-c u" . crux-view-url)
   ("C-c w" . crux-swap-windows)
   ("C-c D" . crux-delete-file-and-buffer)
   ("C-c r" . crux-rename-buffer-and-file)
   ("C-c k" . crux-kill-other-buffers)
   ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
   ("C-c I" . crux-find-user-init-file)
   ("C-c S" . crux-find-shell-init-file)
   ("s-r" . crux-recentf-find-file)
   ("C-S-j" . crux-top-join-line)
   ("C-^" . crux-top-join-line)
   ("C-k" . kill-line)
   ("C-<backspace>" . crux-kill-line-backwards)
   ("s-o" . crux-smart-open-line-above)
   ([remap move-beginning-of-line] . crux-move-beginning-of-line)
   ([(shift return)] . crux-smart-open-line)
   ([(control shift return)] . crux-smart-open-line-above)
   ([remap kill-whole-line] . crux-kill-whole-line)
   ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :diminish which-key-mode
  :config (which-key-mode +1))

(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 2
 vc-make-backup-files t
 version-control t)

(use-package goto-chg
  :bind (("C-c ." . goto-last-change) ("C-c ," . goto-last-change-reverse)))

(use-package color-moccur
  :bind (("C-c o o" . occur) ("C-c o O" . moccur)))

(use-package moccur-edit
  :after color-moccur)

(use-package ace-window
  :diminish
  :bind* ("<M-return>" . ace-window))

(use-package smartparens
  :diminish
  :config
  (defun tj-forward-sexp (arg)
    "Move to forward sexp. With a non-nil ARG, move to next sexp."
    (interactive "P")
    (if arg
        (sp-next-sexp)
      (sp-forward-sexp)))
  
  (require 'smartparens-config)
  (setq sp-ignore-modes-list
        '(minibuffer-inactive-mode eval-expression-minibuffer-setup))
  :bind*
  (:map
   smartparens-mode-map
   ("M-\"" . (lambda (&optional arg) (interactive "P") (sp-wrap-with-pair "\"")))
   ("M-I" . sp-splice-sexp)
   ("M-(" . sp-wrap-round)
   ("M-[" . sp-wrap-square)
   ("C-S-K" . sp-kill-sexp)
   ("C-M-'" . sp-wrap-curly)
   ("C-)" . sp-forward-slurp-sexp)
   ("C-}" . sp-forward-barf-sexp)
   ("C-{" . sp-backward-barf-sexp)
   ("C-(" . sp-backward-slurp-sexp)
   ("C-'" . sp-rewrap-sexp)
   ("M-S" . sp-split-sexp)
   ("M-s" . sp-splice-sexp)
   ("M-J" . sp-join-sexp)
   ("M-W" . sp-copy-sexp)
   ("C-M-f" . tj-forward-sexp))
  :hook ((prog-mode . smartparens-mode)
         (emacs-lisp-mode . smartparens-strict-mode)
         (lisp-interaction-mode . smartparens-strict-mode)
         (ielm-mode . smartparens-strict-mode)
         (clojure-mode . smartparens-strict-mode)
         (cider-repl-mode . smartparens-strict-mode)
         (lisp-mode . smartparens-strict-mode)
         (scheme-mode . smartparens-strict-mode)
         (eval-expression-minibuffer-setup . smartparens-strict-mode)))

(use-package minibuffer
  :after tj
  :straight (:type built-in)
  :config
  (defun tj-minibuffer-setup-hook ()
    (smartparens-mode 0)
    (subword-mode)
    (setq truncate-lines nil)
    (setq gc-cons-threshold most-positive-fixnum))
  (defun tj-minibuffer-exit-hook ()
    (setq gc-cons-threshold 800000))
  (add-hook 'minibuffer-setup-hook #'tj-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'tj-minibuffer-exit-hook))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (smartparens-strict-mode)))

(use-package selected
  :diminish selected-minor-mode
  :bind
  (:map
   selected-keymap
   ("s-[" . align-code)
   ("s-f" . fill-region)
   ("s-U" . unfill-region)
   ("s-d" . downcase-region)
   ("s-u" . upcase-region)
   ("s-s" . sort-lines))
  :config (selected-global-mode 1))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode +1))

(use-package pcmpl-args)

(use-package em-unix
  :after eshell
  :straight (:type built-in)
  :config
  (unintern 'eshell/su nil)
  (unintern 'eshell/sudo nil))

(use-package em-smart
  :after eshell
  :straight (:type built-in))

(use-package eshell
  :commands (eshell eshell-command)
  :hook (eshell-mode . (lambda () (yas-minor-mode 0)))
  :config
  (defun tj-eshell-prompt ()
    "; ")
  (setq eshell-prompt-function 'tj-eshell-prompt) 
 (setq eshell-prompt-regexp "^; ")
  (setq eshell-where-to-jump 'end)
  (setq eshell-review-quick-commands t)
  (setq eshell-smart-space-goes-to-end t)
  (add-to-list 'eshell-expand-input-functions 'eshell-expand-history-references)

  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return] 'eshell-isearch-return)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace] 'eshell-isearch-delete-char)
      (define-key map [delete] 'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")
  (defun tj-eshell-here ()
    (interactive)
    (let*
        (
         (dir
          (if (buffer-file-name)
              (f-dirname (buffer-file-name))
            (projectile-project-root))))
      (eshell-buffer-name (ff-basename dir))
      (eshell dir)))
  
  (defun tj-eshell-mode-hook ()
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))
  (add-hook 'eshell-mode-hook 'tj-eshell-mode-hook))

(use-package multi-eshell)

(use-package eshell-bookmark
  :after eshell
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-up
  :after eshell
  :commands eshell-up)

(use-package eshell-z
  :after eshell)

(use-package fancy-narrow
  :commands (fancy-narrow-to-region fancy-widen))

(use-package wgrep)

(use-package json-snatcher)

(use-package visual-regexp
  :bind
  ("M-&" . vr/query-replace)
  ("M-/" . vr/replace))

(use-package avy-zap
  :bind
  (("C-c e" . zap-up-to-char)
   ("C-c E" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package backup-walker
  :commands backup-walker-start)

(use-package change-inner
  :bind
  (("M-i" . change-inner)
   ("M-o" . change-outer)
   ("C-c M-i" . copy-inner)
   ("C-c M-o" . copy-outer)))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :commands (protobuf-mode)
  :hook
  (protobuf-mode
   .
   (lambda ()
     (subword-mode)
     (c-add-style "tj-protobuf-style" tj-protobuf-style t)
     (setq imenu-generic-expression tj-protobuf-imenu-generic-expression)))
  :config
  (defconst tj-protobuf-style '((c-basic-offset . 2) (indent-tabs-mode . nil)))
  (setq tj-protobuf-imenu-generic-expression
        '
        (("Message" "^message *\\([a-zA-Z0-9_]+\\)" 1)
         ("Service" "^service *\\([a-zA-Z0-9_]+\\)" 1))))

(use-package bm
  :bind
  (("C-c b b" . bm-toggle)
   ("C-c b n" . bm-next)
   ("C-c b l" . bm-show-all)
   ("C-c b p" . bm-previous))
  :commands (bm-repository-load bm-buffer-save bm-buffer-save-all bm-buffer-restore)
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook
            (lambda nil (bm-buffer-save-all) (bm-repository-save))))

(use-package terraform-mode
  :config (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package multifiles
  :bind ("C-!" . mf/mirror-region-in-multifile))

(use-package toggle-quotes
  :bind ("C-\"" . toggle-quotes))

(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(use-package highlight-indentation
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode))

(use-package indent-tools
  :config (add-hook 'yaml-mode-hook 'indent-tools-minor-mode))

(use-package resmacro
  :straight (:type built-in)
  :bind
  (("C-x (" . resmacro-start-macro)))

(use-package wordswitch
  :straight (:type built-in))

(use-package titlecase
  :straight (:type built-in))

(use-package unfill
  :bind (("M-Q" . unfill-paragraph)))

(autoload
  'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(use-package vdiff)

(use-package vdiff-magit)

(use-package dot-mode
  :config
  (setq dot-mode-global-mode t)
  (dot-mode))

(use-package iedit
  :config (setq iedit-toggle-key-default (kbd "C-:")))

(use-package frog-jump-buffer)

(use-package github-review)

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package vterm
  :custom (vterm-install t)
  :config
  (setq vterm-buffer-name "*shell*")
  (setq vterm-buffer-name-string "*shell*")
  (defun tj-vterm (title)
    (interactive "sTitle: ")
    (vterm (format "*%s*" title)))
  (defun vterm--rename-buffer-as-title (title)
    (when (ignore-errors (file-directory-p title))
      (cd-absolute title))
    (rename-buffer (format "term %s" title)))
  (add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)
  :bind
  
  (("C-x m" . tj-vterm)
   :map vterm-mode-map ("M-y" . vterm-yank)
        ))

(use-package vterm-toggle
  :after (vterm))

(use-package rainbow-delimiters)

(use-package rust-mode
  :ensure-system-package ((rls . "rustup component add rls"))
  :bind (:map rust-mode-map ("C-c C-c" . rust-run))
  :config
  (defun tj-rust-hook ()
    (setq rust-format-on-save t
          indent-tabs-mode nil))
  :hook ((rust-mode . tj-rust-hook)))

(use-package flycheck-vale
  :config (flycheck-vale-setup))

(use-package proced-narrow
  :after proced
  :bind (:map proced-mode-map ("/" . proced-narrow)))

(use-package with-editor
  :config
  (add-hook 'shell-mode-hook 'with-editor-export-editor)
  (add-hook 'term-exec-hook 'with-editor-export-editor)
  (add-hook 'eshell-mode-hook 'with-editor-export-editor)
  (add-hook 'vterm-mode-hook 'with-editor-export-editor))

(use-package go-mod
  :straight (:type built-in))

(use-package shim
  :after projectile
  :straight (:type built-in)
  :config (shim-init-go))
  
(use-package elisp-autofmt
  :straight (:type built-in)
  :hook (emacs-lisp-mode-hook . elisp-autofmt-save-hook-for-this-buffer))

(use-package eglot
  :config
  ;; eglot-organize-imports is hopefully a temporary stopgap until
  ;; https://github.com/joaotavora/eglot/issues/574 is addressed.
  (defun eglot-organize-imports ()
    "Offer to execute the source.organizeImports code action."
    (interactive)
    (unless (eglot--server-capable :codeActionProvider)
      (eglot--error "Server can't execute code actions!"))
    (let* ((server (eglot--current-server-or-lose))
           (actions (jsonrpc-request
                     server
                     :textDocument/codeAction
                     (list :textDocument (eglot--TextDocumentIdentifier))))
           (action (cl-find-if
                    (jsonrpc-lambda (&key kind &allow-other-keys)
                                    (string-equal kind "source.organizeImports" ))
                    actions)))
      (when action
        (eglot--dcase action
                      (((Command) command arguments)
                       (eglot-execute-command server (intern command) arguments))
                      (((CodeAction) edit command)
                       (when edit (eglot--apply-workspace-edit edit))
                       (when command
                         (eglot--dbind ((Command) command arguments) command
                                       (eglot-execute-command server (intern command) arguments))))))))

  (defun eglot-organize-imports-on-save ()
    (defun eglot-organize-imports-nosignal ()
      "Run eglot-organize-imports, but demote errors to messages."
      ;; Demote errors to work around
      ;; https://github.com/joaotavora/eglot/issues/411#issuecomment-749305401
      ;; so that we do not prevent subsequent save hooks from running
      ;; if we encounter a spurious error.
      (with-demoted-errors "Error: %s" (eglot-organize-imports)))
    (add-hook 'before-save-hook #'eglot-organize-imports-on-save))

  (add-hook 'go-mode-hook #'eglot-organize-imports-on-save)

  :bind
  (("C-c C-r" . eglot-rename))
  :hook
  (go-mode . eglot-ensure)
  (rust-mode . eglot-ensure))

(use-package lice
  :config
  (define-derived-mode license-mode fundamental-mode "License"
    "Major mode for editing LICENSE files."
    (setq comment-start nil))
  (add-to-list 'auto-mode-alist '("LICENSE\\'" . license-mode)))

(use-package restclient)

(use-package hideshow
  :straight (:type built-in)
  :diminish hs-minor-mode)

(use-package tj
  :straight (:type built-in)
  :diminish auto-revert-mode
  :config

  (condition-case err
    (let ((buffer (get-buffer-create "*todo*")))
      (with-current-buffer buffer
        (insert-file-contents "~/todo.org")
        (org-mode))
      (setq initial-buffer-choice buffer))
    (error (message "%s" error-message-string err)))
  
  (setq native-comp-async-report-warnings-errors nil)
  (setq tj-font-family "Hack"
        tj-font (font-spec :family tj-font-family
                           :size (tj-font-size))
        default-frame-alist (append (list (cons 'width  72)
                                          (cons 'height 40)
                                          (cons 'vertical-scroll-bars nil)
                                          (cons 'internal-border-width 24)
                                          (cons 'font (format "%s %d"
                                                              tj-font-family
                                                              (tj-font-size))))))
  (set-frame-font tj-font nil t)
  (add-hook 'after-init-hook 'tj-theme))

(defun tj-raise-frame-and-give-focus ()
  (when window-system
    (let ((mouse-autoselect-window 1))
      (select-frame-set-input-focus (selected-frame)))))

(add-hook 'server-switch-hook 'tj-raise-frame-and-give-focus)

(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package server
  :no-require
  :hook ((after-init . server-start)))

