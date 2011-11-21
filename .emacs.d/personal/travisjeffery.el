(vendor 'color-theme-solarized)
;(vendor 'evil)
;(vendor 'surround)
(vendor 'wrap-region)
(vendor 'fuzzy-find-in-project)
(vendor 'eprojec1)
(vendor 'tumble)
(vendor 'pomodoro)

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

;(evil-mode 1)
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

(if (file-exists-p "~/.rvm")
    (rvm-use-default))

(whitespace-mode 0)

(load "vimgolf")

(global-linum-mode 1)
(setq linum-format "  %d ")

(setq delete-by-moving-to-trash t)
(setq browse-url-browser-function 'browse-url-default-macosx-browser)
