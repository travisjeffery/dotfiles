;;; prelude-emacs-lisp.el --- Emacs Prelude: Nice config for Elisp programming.
;;
;; Copyright (c) 2011 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar.batsov@gmail.com>
;; URL: http://www.emacswiki.org/cgi-bin/wiki/Prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice config for Elisp Programming.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defgroup emacs-lisp nil
  "Prelude support for Emacs Lisp"
  :group 'prelude)

(defcustom prelude-enable-emacs-lisp-hook t
  "Enable Prelude's Emacs Lisp hook"
  :type 'boolean
  :group 'emacs-lisp)

(require 'prelude-lisp)

(defun prelude-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(when prelude-enable-emacs-lisp-hook
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook 'prelude-remove-elc-on-save)

  (add-hook 'emacs-lisp-mode-hook 'prelude-lisp-coding-hook)

  (add-hook 'ielm-mode-hook 'prelude-interactive-lisp-coding-hook)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))

(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)

(provide 'prelude-emacs-lisp)

;;; prelude-emacs-lisp.el ends here
