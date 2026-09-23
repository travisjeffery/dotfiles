;;; init.el --- User Emacs configuration -*- lexical-binding: t -*-

;; Load Omarchy integration (theme syncing, font syncing, file watchers).
;; Remove this line to opt out of Omarchy Emacs integration.
;; Pin the theme to modus-vivendi (see user-init.el) instead of following the
;; desktop. This has to precede the load: omarchy.el themes at load time, and it
;; paints the default face and `default-frame-alist' outside the theme system,
;; where `disable-theme' cannot reach it.
(advice-add 'omarchy-apply-theme :override #'ignore)
(load (expand-file-name "omarchy" user-emacs-directory))

;; Load the personal configuration migrated from the legacy ~/.emacs.d.
(load (expand-file-name "user-init" user-emacs-directory))

;;; init.el ends here
