;;; url-vars.el --- Variables for Uniform Resource Locator tool
;; Author: $Author: fx $
;; Created: $Date: 2002/04/22 09:25:02 $
;; Version: $Revision: 1.14 $
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

(require 'mm-util)
(eval-when-compile (require 'cl))

(defconst url-version (let ((x "$State: Exp $"))
			(if (string-match "State: \\([^ \t\n]+\\)" x)
			    (substring x (match-beginning 1) (match-end 1))
			  x))
  "Version number of URL package.")

(defgroup url nil
  "Uniform Resource Locator tool"
  :group 'hypermedia)

(defgroup url-file nil
  "URL storage"
  :prefix "url-"
  :group 'url)

(defgroup url-cache nil
  "URL cache"
  :prefix "url-"
  :prefix "url-cache-"
  :group 'url)

(defgroup url-mime nil
  "MIME options of URL"
  :prefix "url-"
  :group 'url)

(defgroup url-hairy nil
  "Hairy options of URL"
  :prefix "url-"
  :group 'url)


(defvar url-current-object nil
  "A parsed representation of the current url.")

(defvar url-current-mime-headers nil
  "A parsed representation of the MIME headers for the current url.")

(mapcar 'make-variable-buffer-local
	'(
	  url-current-object
	  url-current-referer
	  url-current-mime-headers
	  ))

(defcustom url-honor-refresh-requests t
  "*Whether to do automatic page reloads.
These are done at the request of the document author or the server via
the `Refresh' header in an HTTP response.  If nil, no refresh
requests will be honored.  If t, all refresh requests will be honored.
If non-nil and not t, the user will be asked for each refresh
request."
  :type '(choice (const :tag "off" nil)
		 (const :tag "on" t)
		 (const :tag "ask" 'ask))
  :group 'url-hairy)

(defcustom url-automatic-caching nil
  "*If non-nil, all documents will be automatically cached to the local disk."
  :type 'boolean
  :group 'url-cache)

;; Fixme: sanitize this.
(defcustom url-cache-expired
  (lambda (t1 t2) (>= (- (car t2) (car t1)) 5))
  "*A function determining if a cached item has expired.
It takes two times (numbers) as its arguments, and returns non-nil if
the second time is 'too old' when compared to the first time."
  :type 'function
  :group 'url-cache)

(defvar url-bug-address "w3-bugs@xemacs.org"
  "Where to send bug reports.")

(defcustom url-personal-mail-address nil
  "*Your full email address.
This is what is sent to HTTP servers as the FROM field in an HTTP
request."
  :type '(choice (const :tag "Unspecified" nil) string)
  :group 'url)

(defcustom url-directory-index-file "index.html"
  "*The filename to look for when indexing a directory.
If this file exists, and is readable, then it will be viewed instead of
using `dired' to view the directory."
  :type 'string
  :group 'url-file)

;; Fixme: this should have a setter which calls url-setup-privacy-info.
(defcustom url-privacy-level '(email)
  "*How private you want your requests to be.
HTTP has header fields for various information about the user, including
operating system information, email addresses, the last page you visited, etc.
This variable controls how much of this information is sent.

This should a symbol or a list.
Valid values if a symbol are:
none     -- Send all information
low      -- Don't send the last location
high     -- Don't send the email address or last location
paranoid -- Don't send anything

If a list, this should be a list of symbols of what NOT to send.
Valid symbols are:
email    -- the email address
os       -- the operating system info
lastloc  -- the last location
agent    -- Do not send the User-Agent string
cookie   -- never accept HTTP cookies

Samples:

 (setq url-privacy-level 'high)
 (setq url-privacy-level '(email lastloc))    ;; equivalent to 'high
 (setq url-privacy-level '(os))

::NOTE::
This variable controls several other variables and is _NOT_ automatically
updated.  Call the function `url-setup-privacy-info' after modifying this
variable."
  :type '(radio (const :tag "None (you believe in the basic goodness of humanity)"
		       :value none)
		(const :tag "Low (do not reveal last location)"
		       :value low)
		(const :tag "High (no email address or last location)"
		       :value high)
		(const :tag "Paranoid (reveal nothing!)"
		       :value paranoid)
		(checklist :tag "Custom"
			   (const :tag "Email address" :value email)
			   (const :tag "Operating system" :value os)
			   (const :tag "Last location" :value lastloc)
			   (const :tag "Browser identification" :value agent)
			   (const :tag "No cookies" :value cookie)))
  :group 'url)

(defvar url-inhibit-uncompression nil "Do not do decompression if non-nil.")

(defcustom url-uncompressor-alist '((".z"  . "x-gzip")
				    (".gz" . "x-gzip")
				    (".uue" . "x-uuencoded")
				    (".hqx" . "x-hqx")
				    (".Z"  . "x-compress")
				    (".bz2"  . "x-bzip2"))
  "*An alist of file extensions and appropriate content-transfer-encodings."
  :type '(repeat (cons :format "%v"
		       (string :tag "Extension")
		       (string :tag "Encoding")))
  :group 'url-mime)

(defcustom url-mail-command (if (fboundp 'compose-mail)
				'compose-mail
			      'url-mail)
  "*This function will be called whenever url needs to send mail.
It should enter a mail-mode-like buffer in the current window.
The commands `mail-to' and `mail-subject' should still work in this
buffer, and it should use `mail-header-separator' if possible."
  :type 'function
  :group 'url)

(defcustom url-proxy-services nil
  "*An alist of schemes and proxy servers that gateway them.
Looks like ((\"http\" . \"hostname:portnumber\") ...).  This is set up
from the ACCESS_proxy environment variables."
  :type '(repeat (cons :format "%v"
		       (string :tag "Protocol")
		       (string :tag "Proxy")))
  :group 'url)

(defcustom url-passwd-entry-func nil
  "*Symbol indicating which function to call to read in a password.
It will be set up depending on whether you are running EFS or ange-ftp
at startup if it is nil.  This function should accept the prompt
string as its first argument, and the default value as its second
argument."
  :type '(choice (const :tag "Guess" :value nil)
		 (const :tag "Use Ange-FTP" :value ange-ftp-read-passwd)
		 (const :tag "Use EFS"      :value efs-read-passwd)
		 (const :tag "Use Password Package" :value read-passwd)
		 (function :tag "Other"))
  :group 'url-hairy)

(defcustom url-standalone-mode nil
  "*Rely solely on the cache?"
  :type 'boolean
  :group 'url-cache)

(defvar url-mime-separator-chars (mapcar 'identity
					(concat "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
						"abcdefghijklmnopqrstuvwxyz"
						"0123456789'()+_,-./=?"))
  "Characters allowable in a MIME multipart separator.")

(defcustom url-bad-port-list
  '("25" "119" "19")
  "*List of ports to warn the user about connecting to.
Defaults to just the mail, chargen, and NNTP ports so you cannot be
tricked into sending fake mail or forging messages by a malicious HTML
document."
  :type '(repeat (string :tag "Port"))
  :group 'url-hairy)

(defvar url-mime-content-type-charset-regexp
  ";[ \t]*charset=\"?\\([^\"]+\\)\"?"
  "Regexp used in parsing `Content-Type' for a charset indication.")

(defvar url-request-data nil "Any data to send with the next request.")

(defvar url-request-extra-headers nil
  "A list of extra headers to send with the next request.
Should be an assoc list of headers/contents.")

(defvar url-request-method nil "The method to use for the next request.")

;; FIXME!!  (RFC 2616 gives examples like `compress, gzip'.)
(defvar url-mime-encoding-string nil
  "*String to send in the Accept-encoding: field in HTTP requests.")

;; `mm-mime-mule-charset-alist' in Gnus 5.8/9 contains elements whose
;; cars aren't valid MIME charsets/coding systems, at least in Emacs.
;; This gets it correct by construction in Emacs.  Fixme: DTRT for
;; XEmacs -- its `coding-system-list' doesn't have the BASE-ONLY arg.
(when (and (not (featurep 'xemacs))
	   (fboundp 'coding-system-list))
  (setq mm-mime-mule-charset-alist
	(apply
	 'nconc
	 (mapcar
	  (lambda (cs)
	    (when (and (coding-system-get cs 'mime-charset)
		       (not (eq t (coding-system-get cs 'safe-charsets))))
	      (list (cons (coding-system-get cs 'mime-charset)
			  (delq 'ascii
				(coding-system-get cs 'safe-charsets))))))
	  (coding-system-list 'base-only)))))

;; Perhaps the first few should actually be given decreasing `q's and
;; the list should be trimmed significantly.
;; Fixme: do something sane if we don't have `sort-coding-systems'
;; (Emacs 20, XEmacs).
(defun url-mime-charset-string ()
  "Generate a list of preferred MIME charsets for HTTP requests.
Generated according to current coding system priorities."
  (if (fboundp 'sort-coding-systems)
      (let ((ordered (sort-coding-systems
		      (let (accum)
			(dolist (elt mm-mime-mule-charset-alist)
			  (if (mm-coding-system-p (car elt))
			      (push (car elt) accum)))
			(nreverse accum)))))
	(concat (format "%s;q=1, " (pop ordered))
		(mapconcat 'symbol-name ordered ";q=0.5, ")
		";q=0.5"))))

(defvar url-mime-charset-string (url-mime-charset-string)
  "*String to send in the Accept-charset: field in HTTP requests.
The MIME charset corresponding to the most preferred coding system is
given priority 1 and the rest are given priority 0.5.")

(defun url-set-mime-charset-string ()
  (setq url-mime-charset-string (url-mime-charset-string)))
;; Regenerate if the language environment changes.
(add-hook 'set-language-environment-hook 'url-set-mime-charset-string)

;; Fixme: set from the locale.
(defcustom url-mime-language-string nil
  "*String to send in the Accept-language: field in HTTP requests.

Specifies the preferred language when servers can serve documents in
several languages.  Use RFC 1766 abbreviations, e.g.@: `en' for
English, `de' for German.  A comma-separated specifies descending
order of preference.  The ordering can be made explicit using `q'
factors defined by HTTP, e.g. `de,en-gb;q=0.8,en;q=0.7'.  `*' means
get the first available language (as opposed to the default)."
  :type '(radio
	  (const :tag "None (get default language version)" :value nil)
	  (const :tag "Any (get first available language version)" :value "*")
	  (string :tag "Other"))
  :group 'url-mime
  :group 'i18n)

(defvar url-mime-accept-string nil
  "String to send to the server in the Accept: field in HTTP requests.")

(defvar url-package-version nil
  "Version number of package using URL.")

(defvar url-package-name nil "Version number of package using URL.")

(defvar url-system-type nil
  "What type of system we are on.")
(defvar url-os-type nil
  "What OS we are on.")

(defcustom url-max-password-attempts 5
  "*Maximum number of times a password will be prompted for.
Applies when a protected document is denied by the server."
  :type 'integer
  :group 'url)

(defcustom url-temporary-directory (or (getenv "TMPDIR") "/tmp")
  "*Where temporary files go."
  :type 'directory
  :group 'url-file)

(defcustom url-show-status t
  "*Whether to show a running total of bytes transferred.
Can cause a large hit if using a remote X display over a slow link, or
a terminal with a slow modem."
  :type 'boolean
  :group 'url)

(defvar url-using-proxy nil
  "Either nil or the fully qualified proxy URL in use, e.g.
http://www.domain.com/")

(defvar url-slow-proxy-requires-waiting-for nil
  "Either nil or a number of seconds to wait before authentication or redirection")

(defcustom url-news-server nil
  "*The default news server from which to get newsgroups/articles.
Applies if no server is specified in the URL.  Defaults to the
environment variable NNTPSERVER or \"news\" if NNTPSERVER is
undefined."
  :type '(choice (const :tag "None" :value nil) string)
  :group 'url)

(defvar url-nonrelative-link
  "\\`\\([-a-zA-Z0-9+.]+:\\)"
  "A regular expression that will match an absolute URL.")

(defcustom url-confirmation-func 'y-or-n-p
  "*What function to use for asking yes or no functions.
Possible values are `yes-or-no-p' or `y-or-n-p', or any function that
takes a single argument (the prompt), and returns t only if a positive
answer is given."
  :type '(choice (const :tag "Short (y or n)" :value y-or-n-p)
		 (const :tag "Long (yes or no)" :value yes-or-no-p)
		 (function :tag "Other"))
  :group 'url-hairy)

(defcustom url-gateway-method 'native
  "*The type of gateway support to use.
Should be a symbol specifying how to get a connection from the local machine.

Currently supported methods:
`telnet': Run telnet in a subprocess to connect;
`rlogin': Rlogin to another machine to connect;
`socks': Connect through a socks server;
`ssl': Connect with SSL;
`native': Connect directy."
  :type '(radio (const :tag "Telnet to gateway host" :value telnet)
		(const :tag "Rlogin to gateway host" :value rlogin)
		(const :tag "Use SOCKS proxy" :value socks)
		(const :tag "Use SSL for all connections" :value ssl)
		(const :tag "Direct connection" :value native))
  :group 'url-hairy)

(defvar url-setup-done nil "Has setup configuration been done?")

(defconst weekday-alist
  '(("Sunday" . 0) ("Monday" . 1) ("Tuesday" . 2) ("Wednesday" . 3)
    ("Thursday" . 4) ("Friday" . 5) ("Saturday" . 6)
    ("Tues" . 2) ("Thurs" . 4)
    ("Sun" . 0) ("Mon" . 1) ("Tue" . 2) ("Wed" . 3)
    ("Thu" . 4) ("Fri" . 5) ("Sat" . 6)))

(defconst monthabbrev-alist
  '(("Jan" . 1) ("Feb" . 2) ("Mar" . 3) ("Apr" . 4) ("May" . 5) ("Jun" . 6)
    ("Jul" . 7) ("Aug" . 8) ("Sep" . 9) ("Oct" . 10) ("Nov" . 11)
    ("Dec" . 12)))

(defvar url-lazy-message-time 0)

;; Fixme: We may not be able to run SSL.
(defvar url-extensions-header "Security/Digest Security/SSL")

(defvar url-parse-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "*A syntax table for parsing URLs.")

(modify-syntax-entry ?' "\"" url-parse-syntax-table)
(modify-syntax-entry ?` "\"" url-parse-syntax-table)
(modify-syntax-entry ?< "(>" url-parse-syntax-table)
(modify-syntax-entry ?> ")<" url-parse-syntax-table)
(modify-syntax-entry ?/ " " url-parse-syntax-table)

(defvar url-load-hook nil
  "*Hooks to be run after initalizing the URL library.")

;;; Make OS/2 happy - yeeks
;; (defvar	tcp-binary-process-input-services nil
;;   "*Make OS/2 happy with our CRLF pairs...")

(defconst url-working-buffer " *url-work")

(defvar url-gateway-unplugged nil
  "Non-nil means don't open new network connexions.
This should be set, e.g. by mail user agents rendering HTML to avoid
`bugs' which call home.")

(defun url-vars-unload-hook ()
  (remove-hook 'set-language-environment-hook 'url-set-mime-charset-string))

(provide 'url-vars)

;;; url-vars.el ends here
