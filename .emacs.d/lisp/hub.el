;;; hub.el --- hub helps you win at git with emacs. ;;; -*- lexical-binding: t -*-
;;
;; Copyright © 2014 Travis Jeffery
;;
;; Author: Travis Jeffery <tj@travisjeffery.com>
;; URL: https://github.com/travisjeffery/hub.el
;; Version: 1.0.0
;; Keywords: github git
;; Package-Requires: ((transient))

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Here are the definitions of most of the functions added by Prelude.

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

(require 'cl-lib)
(require 'transient)

;; Interactive

(cl-defstruct (hub-command (:constructor hub-command-create)
                           (:copier nil))
  name action help)

(setq hub-commands (list
                      (hub-command-create :name "ci-status" :action "s" :help "Show the status of GitHub checks for a commit.")
                      (hub-command-create :name "browse" :action "b" :help "Open a GitHub page in the default browser.")
                      (hub-command-create :name "pull-request" :action "o" :help "Open a pull request on GitHub.")
                      (hub-command-create :name "pr" :action "c" :help "List or checkout GitHub pull requests.")
                      (hub-command-create :name "fork" :action "f" :help "Make a fork of a remote repository on GitHub and add as remote.")
                      (hub-command-create :name "create" :action "m" :help "Create this repository on GitHub and add GitHub as origin.")
                      (hub-command-create :name "delete" :action "d" :help "Delete a repository on GitHub.")
                      (hub-command-create :name "release" :action "r" :help "List or create GitHub releases.")
                      (hub-command-create :name "issue" :action "i" :help "List or create GitHub issues.")
                      (hub-command-create :name "sync" :action "y" :help "Fetch git objects from upstream and update branches.")))

(dolist (cmd hub-commands)
  (eval
   `(defun ,(intern (format "hub-%s" (hub-command-name cmd))) ()
      ,(hub-command-help cmd)
      (interactive)
      (message (hub--shell-command-to-string ,(hub-command-name cmd))))))

(defun hub--shell-command-to-string (args)
  (string-trim (shell-command-to-string (format "hub %s" args))))

(defun hub--action (cmd)
  (list (hub-command-action cmd) (hub-command-help cmd) (intern (format "hub-%s" (hub-command-name cmd)))))

(define-transient-command hub ()
  "Open hub to run commands against GitHub."
  (let ((actions (maplist hub--action hub-commands)))
    (message actions)))

(hub)
(provide 'hub)
;;; hub.el ends here
