;;; url.el --- Uniform Resource Locator retrieval tool
;; Author: Bill Perry <wmperry@gnu.org>
;; Version: $Revision: 1.15 $
;; Keywords: comm, data, processes, hypermedia

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Copyright (c) 1993 - 1996 by William M. Perry <wmperry@cs.indiana.edu>
;;; Copyright (c) 1996, 97, 98, 99, 2001 Free Software Foundation, Inc.
;;;
;;; This file is part of GNU Emacs.
;;;
;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.
;;;
;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Registered URI schemes: http://www.iana.org/assignments/uri-schemes

(eval-when-compile (require 'cl))
;; Don't require CL at runtime if we can avoid it (Emacs 21).
;; Otherwise we need it for hashing functions.  `puthash' was never
;; defined in the Emacs 20 cl.el for some reason.
(if (fboundp 'puthash)
    nil					; internal or CL is loaded
  (defalias 'puthash 'cl-puthash)
  (autoload 'cl-puthash "cl")
  (autoload 'gethash "cl")
  (autoload 'maphash "cl")
  (autoload 'make-hash-table "cl"))

(eval-when-compile
  (require 'mm-decode)
  (require 'mm-view))

(require 'mailcap)
(require 'url-vars)
(require 'url-cookie)
(require 'url-history)
(require 'url-expand)
(require 'url-privacy)
(require 'url-methods)
(require 'url-proxy)
(require 'url-parse)
(require 'url-util)

;; Fixme: customize? convert-standard-filename? 
;;;###autoload
(defvar url-configuration-directory "~/.url")

(defun url-do-setup ()
  "Setup the url package.
This is to avoid conflict with user settings if URL is dumped with
Emacs."
  (unless url-setup-done

    ;; Make OS/2 happy
    ;;(push '("http" "80") tcp-binary-process-input-services)

    (mailcap-parse-mailcaps)
    (mailcap-parse-mimetypes)
    
    ;; Register all the authentication schemes we can handle
    (url-register-auth-scheme "basic" nil 4)
    (url-register-auth-scheme "digest" nil 7)

    (setq url-cookie-file
	  (or url-cookie-file
	      (expand-file-name "cookies" url-configuration-directory)))
    
    (setq url-history-file
	  (or url-history-file
	      (expand-file-name "history" url-configuration-directory)))
  
    ;; Parse the global history file if it exists, so that it can be used
    ;; for URL completion, etc.
    (url-history-parse-history)
    (url-history-setup-save-timer)

    ;; Ditto for cookies
    (url-cookie-setup-save-timer)
    (url-cookie-parse-file url-cookie-file)

    ;; Read in proxy gateways
    (let ((noproxy (and (not (assoc "no_proxy" url-proxy-services))
			(or (getenv "NO_PROXY")
			    (getenv "no_PROXY")
			    (getenv "no_proxy")))))
      (if noproxy
	  (setq url-proxy-services
		(cons (cons "no_proxy"
			    (concat "\\("
				    (mapconcat
				     (lambda (x)
				       (cond
					((= x ?,) "\\|")
					((= x ? ) "")
					((= x ?.) (regexp-quote "."))
					((= x ?*) ".*")
					((= x ??) ".")
					(t (char-to-string x))))
				     noproxy "") "\\)"))
		      url-proxy-services))))

    ;; Set the password entry funtion based on user defaults or guess
    ;; based on which remote-file-access package they are using.
    (cond
     (url-passwd-entry-func nil)	; Already been set
     ((fboundp 'read-passwd)		; Use secure password if available
      (setq url-passwd-entry-func 'read-passwd))
     ((or (featurep 'efs)		; Using EFS
	  (featurep 'efs-auto))		; or autoloading efs
      (if (not (fboundp 'read-passwd))
	  (autoload 'read-passwd "passwd" "Read in a password" nil))
      (setq url-passwd-entry-func 'read-passwd))
     ((or (featurep 'ange-ftp)		; Using ange-ftp
	  (and (boundp 'file-name-handler-alist)
	       (not (featurep 'xemacs)))) ; ??
      (setq url-passwd-entry-func 'ange-ftp-read-passwd))
     (t
      (url-warn
       'security
       "(url-setup): Can't determine how to read passwords, winging it.")))
  
    (url-setup-privacy-info)
    (run-hooks 'url-load-hook)
    (setq url-setup-done t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Retrieval functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun url-retrieve (url callback &optional cbargs)
  "Retrieve URL asynchronously and call CALLBACK with CBARGS when finished.
The callback is called when the object has been completely retrieved, with
the current buffer containing the object, and any MIME headers associated
with it.  URL is either a string or a parsed URL.

Return the buffer URL will load into, or nil if the process has
already completed."
  (url-do-setup)
  (url-gc-dead-buffers)
  (if (stringp url)
       (set-text-properties 0 (length url) nil url))
  (if (not (vectorp url))
      (setq url (url-generic-parse-url url)))
  (if (not (functionp callback))
      (error "Must provide a callback function to url-retrieve"))
  (unless (url-type url)
    (error "Bad url: %s" (url-recreate-url url)))
  (let ((loader (url-scheme-get-property (url-type url) 'loader))
	(url-using-proxy (if (url-host url)
			     (url-find-proxy-for-url url (url-host url))))
	(buffer nil)
	(asynch (url-scheme-get-property (url-type url) 'asynchronous-p)))
    (if url-using-proxy
	(setq asynch t
	      loader 'url-proxy))
    (if asynch
	(setq buffer (funcall loader url callback cbargs))
      (setq buffer (funcall loader url))
      (if buffer
	  (save-excursion
	    (set-buffer buffer)
	    (apply callback cbargs))))
    (url-history-update-url url (current-time))
    buffer))

(defun url-retrieve-synchronously (url)
  "Retrieve URL synchronously.
Return the buffer containing the data, or nil if there are no data
associated with it (the case for dired, info, or mailto URLs that need
no further processing).  URL is either a string or a parsed URL."
  (url-do-setup)

  (lexical-let ((retrieval-done nil)
		(asynch-buffer nil))
    (setq asynch-buffer
	  (url-retrieve url (lambda (&rest ignored)
			      (url-debug 'retrieval "Synchronous fetching done (%S)" (current-buffer))
			      (setq retrieval-done t
				    asynch-buffer (current-buffer)))))
    (let ((proc (and asynch-buffer (get-buffer-process asynch-buffer))))
      (if (null proc)
	  ;; We do not need to do anything, it was a mailto or something
	  ;; similar that takes processing completely outside of the URL
	  ;; package.
	  nil
	(while (not retrieval-done)
	  (if (memq (process-status proc) '(closed exit signal failed))
	      ;; FIXME: It's not clear whether url-retrieve's callback is
	      ;; guaranteed to be called or not.  It seems that url-http
	      ;; decides sometimes consciously not to call it, so it's not
	      ;; clear that it's a bug, but even if we need to decide how
	      ;; url-http can then warn us that the download has completed.
              ;; In the mean time, we use this here workaround.
              (setq retrieval-done t)
            ;; We used to use `sit-for' here, but in some cases it wouldn't
            ;; work because apparently pending keyboard input would always
            ;; interrupt it before it got a chance to handle process input.
            ;; `sleep-for' was tried but it lead to other forms of
            ;; hanging.  --Stef
            (unless (accept-process-output proc)
              ;; accept-process-output returned nil, maybe because the process
              ;; exited (and may have been replaced with another).
              (setq proc (get-buffer-process asynch-buffer))))))
      asynch-buffer)))

(defun url-mm-callback (&rest ignored)
  (let ((handle (mm-dissect-buffer t)))
    (save-excursion
      (url-mark-buffer-as-dead (current-buffer))
      (set-buffer (generate-new-buffer (url-recreate-url url-current-object)))
      (if (eq (mm-display-part handle) 'external)
	  (progn
	    (set-process-sentinel
	     ;; Fixme: this shouldn't have to know the form of the
	     ;; undisplayer produced by `mm-display-part'.
	     (get-buffer-process (cdr (mm-handle-undisplayer handle)))
	     `(lambda (proc event)
		(mm-destroy-parts (quote ,handle))))
	    (message "Viewing externally")
	    (kill-buffer (current-buffer)))
	(display-buffer (current-buffer))
	(mm-destroy-parts handle)))))

(defun url-mm-url (url)
  "Retrieve URL and pass to the appropriate viewing application."
  (require 'mm-decode)
  (require 'mm-view)
  (url-retrieve url 'url-mm-callback nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Miscellaneous
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar url-dead-buffer-list nil)

(defun url-mark-buffer-as-dead (buff)
  (push buff url-dead-buffer-list))

(defun url-gc-dead-buffers ()
  (let ((buff))
    (while (setq buff (pop url-dead-buffer-list))
      (if (buffer-live-p buff)
	  (kill-buffer buff)))))

(cond
 ((fboundp 'display-warning)
  (defalias 'url-warn 'display-warning))
 ((fboundp 'warn)
  (defun url-warn (class message &optional level)
    (warn "(%s/%s) %s" class (or level 'warning) message)))
 (t
  (defun url-warn (class message &optional level)
    (save-excursion
      (set-buffer (get-buffer-create "*URL-WARNINGS*"))
      (goto-char (point-max))
      (save-excursion
	(insert (format "(%s/%s) %s\n" class (or level 'warning) message)))
      (display-buffer (current-buffer))))))

(provide 'url)

;;; url.el ends here
