;;; -*- lexical-binding: t; -*-

(setq warning-suppress-types
      '((files lexical-binding)))

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

;; Faster font rendering in large/long-lived sessions.
(setq inhibit-compacting-font-caches t)

;; Speed up startup by temporarily disabling expensive file handlers (e.g. TRAMP).
(defvar tj--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook
 'after-init-hook
 (lambda ()
   (setq file-name-handler-alist tj--file-name-handler-alist)))

(let ((lisp-dir (expand-file-name (format "%s%s" user-emacs-directory "lisp"))))
  (setq load-path
        (append
         (delete-dups load-path)
         (list lisp-dir))))

(setq user-emacs-var-directory (expand-file-name "var" user-emacs-directory))
