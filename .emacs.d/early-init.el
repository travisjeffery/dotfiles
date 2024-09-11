(setq package-enable-at-startup nil)

(let ((lisp-dir (expand-file-name (format "%s%s" user-emacs-directory "lisp"))))
    (setq load-path
          (append
           (delete-dups load-path)
           (list lisp-dir))))

(setq user-emacs-var-directory (expand-file-name "var" user-emacs-directory))
