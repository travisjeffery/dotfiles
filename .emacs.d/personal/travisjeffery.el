(when (not package-archive-contents)
  (package-refresh-contents))
;; Add in your own as you wish:
(defvar my-packages '(
                      auctex
                      browse-kill-ring
                      clojure-mode
                      coffee-mode
                      color-theme
                      deft
                      find-file-in-project
                      full-ack
                      gist
                      haml-mode
                      haskell-mode
                      idle-highlight-mode
                      ido-ubiquitous
                      magit
                      markdown-mode
                      paredit
                      projectile
                      python
                      rvm
                      sass-mode
                      save-visited-files
                      scss-mode
                      smex
                      slime
                      clojure-test-mode
                      starter-kit
                      textmate
                      undo-tree
                      yaml-mode
                      yari
                      yasnippet
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(vendor 'color-theme-solarized)
(vendor 'wrap-region)
(vendor 'fuzzy-find-in-project)
(vendor 'eproject)
(vendor 'tumble)
(vendor 'pomodoro)
(load "vimgolf")

(require 're-builder)
(setq reb-re-syntax 'string)

(setq cua-enable-cua-keys nil)           
(cua-mode t)                             

(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.tmp"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(let ((week (* 60 60 24 7))
      (current (float-time (current-time))))
  (dolist (file (directory-files temporary-file-directory t))
    (when (and (backup-file-name-p file)
               (> (- current (float-time (fifth (file-attributes file))))
                  week))
      (message file)
      (delete-file file))))

(color-theme-solarized-light)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil)

(setq
 scroll-margin 20
 scroll-conservatively 101
 scroll-preserve-screen-position 1)

(setq ido-create-new-buffer 'always)

(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t)


(setq kil-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

(tooltip-mode -1)
(setq tooltip-use-echo-area t)

(setq enable-recursive-minibuffers t)

(setq visible-bell t)

(wrap-region-global-mode 1)

(textmate-mode 1)

(require 'eproject-extras)

(define-project-type clojure (generic)
  (look-for "project.clj"))

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(browse-kill-ring-default-keybindings)

(setq ack-prompt-for-directory 1)
(put 'erase-buffer 'disabled nil)

(setenv "PATH" "/Users/travis/bin:/usr/local/bin:/Users/travis/.rbenv/shims:/Users/travis/.rbenv/bin:/Users/travis/.lein/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/X11/bin")

(add-hook 'ruby-mode-hook       'esk-paredit-nonlisp)
(add-hook 'espresso-mode-hook   'esk-paredit-nonlisp)

(push "/usr/local/bin" exec-path)

(if (file-exists-p "~/.rvm")
    (push "~/.rvm/bin" exec-path)
  (rvm-use-default))

(whitespace-mode 0)

(global-linum-mode 1)
(setq linum-format "  %d ")

(setq fringe-mode 0)

(setq delete-by-moving-to-trash t)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
