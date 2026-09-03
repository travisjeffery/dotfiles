;;; omarchy.el --- Omarchy Emacs integration shim -*- lexical-binding: t -*-
;;; Managed by omarchy-emacs. Loads the package-installed implementation so
;;; that AUR upgrades propagate without re-running omarchy-emacs-setup.
;;; Put your personal customizations in init.el instead.

(let ((omarchy--system-file "/usr/share/omarchy-emacs/config/omarchy.el"))
  (when (file-exists-p omarchy--system-file)
    (load omarchy--system-file)))

;;; omarchy.el ends here
