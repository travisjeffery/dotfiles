;;; init.el --- User Emacs configuration -*- lexical-binding: t -*-

;; Load Omarchy integration (theme syncing, font syncing, file watchers).
;; Remove this line to opt out of Omarchy Emacs integration.
(load (expand-file-name "omarchy" user-emacs-directory))

;; Load the personal configuration migrated from the legacy ~/.emacs.d.
(load (expand-file-name "user-init" user-emacs-directory))

;;; init.el ends here
