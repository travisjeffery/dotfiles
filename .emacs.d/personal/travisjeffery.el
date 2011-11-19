(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.tmp"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control 1)

(vendor 'color-theme-solarized)
(color-theme-solarized-light)

;(vendor 'evil)
;(evil-mode 1)
;(vendor 'surround)
;(global-surround-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-nonexistent-file-or-buffer nil)

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

(vendor 'wrap-region)
(wrap-region-global-mode 1)

(textmate-mode 1)

(vendor 'fuzzy-find-in-projec1)
(vendor 'eprojec1)
(require 'eproject-extras)

(define-project-type clojure (generic)
  (look-for "project.clj"))

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(browse-kill-ring-default-keybindings)

(setq ack-prompt-for-directory 1)

(setenv "PATH" "/Users/travis/.rvm/gems/ruby-head/bin:/Users/travis/.rvm/gems/ruby-head@global/bin:/Users/travis/.rvm/rubies/ruby-head/bin:/Users/travis/.rvm/bin:/Users/travis/bin:/usr/local/bin:/Users/travis/.rbenv/shims:/Users/travis/.rbenv/bin:/Users/travis/.lein/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/X11/bin")

(whitespace-mode 0)

(setq linum-format "  %d ")

(setq delete-by-moving-to-trash t)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

(vendor 'tumble)
