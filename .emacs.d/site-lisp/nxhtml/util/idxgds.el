;;; idxgds.el --- For Google Desktop Search
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2011-01-02 Sun
;; Version:
;; Last-Updated:
;; URL:
;; Keywords:
;; Compatibility:
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This is for use with idxsearch.el
;;
;; For the Google Desktop Search API see
;; http://code.google.com/apis/desktop/docs/queryapi.html
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'mm-url)

(defcustom idxgds-query-url ""
  "Stored query URL.
See URL `http://code.google.com/apis/desktop/docs/queryapi.html'
for how to get it."
  :type 'string
  :group 'idxgds)

;;;###autoload
(defun idxgds-query-url-p ()
  (< 0 (length idxgds-query-url)))

;; (idxgds-raw-query "cullberg" "" "c:/" 2 0)
(defun idxgds-raw-query (query file-patt root num start)
  "Return.
START is 0-based."
  (let* ((num-s   (number-to-string num))
         (start-s (number-to-string start))
         (url (concat idxgds-query-url query "&format=xml&num=" num-s "&start=" start-s))
         ;;(buffer (url-retrieve-synchronously url))
         (buffer (generate-new-buffer "idxgds-url"))
         num-hits hits
         (debug nil))
    (with-current-buffer buffer (url-insert-file-contents url))
    (when debug (display-buffer buffer))
    (message "url=%s" url)
    (with-current-buffer buffer
      (mm-enable-multibyte) ;; Fix-me: How should this be done, the data is utf8, xml.
      (goto-char (point-min))
      (message "buffer.size=%d" (buffer-size buffer))
      (re-search-forward "^<results count=\"\\([0-9]+\\)\">$")
      (message "buffer.point 1=%s" (point))
      (setq num-hits (string-to-number (match-string 1)))
      ;; (rx anything)
      (backward-char)
      (while (re-search-forward (concat "^<result>" (rx (submatch (*? anything))) "</result>$") nil t)
        (message "buffer.point 2=%s" (point))
        (let ((rec (match-string 1))
              orig-url orig-snippet hit (m t))
          (dolist (what '("category" "url" "snippet" "title" "icon"))
            (when (and m (string-match (concat "^<" what ">\\(.*?\\)</" what ">$") rec))
              (let ((str (match-string 1 rec)))
                (when str
                  (setq str (save-match-data (mm-url-decode-entities-string str))))
                (cond
                 ((string= "category" what)
                  (unless (string= "file" str) (setq m nil)))
                 ((string= "snippet" what)
                  (setq orig-snippet str))
                 ((string= "url" what)
                  (message "url str=%s" str)
                  (if (not (or (= 0 (length file-patt))
                               (string-match file-patt str)))
                      (setq m nil)
                    (setq orig-url str)
                    (and (< 0 (length root))
                         (let ((rel (file-relative-name str root)))
                           (setq str rel)
                           (or (string= ".."
                                        (substring rel 0 2))
                               (file-name-absolute-p rel))) ;; w32
                         (setq m nil))))
                 ((string= "title" what)
                  ;; Try to get rid of title if it is just repeating
                  ;; what is said in snippet.
                  (save-match-data
                    (let* ((str1 (replace-regexp-in-string "\\([0-9]+\.\\)\.\\([0-9]+\\)$" 
                                                           "\\1\\2" str))
                           (str1-len (length str1))
                           (len-snip (length orig-snippet)))
                      (when (and (> len-snip str1-len)
                                 (string= str1 (substring orig-snippet 0 str1-len)))
                        (setq str nil))
                      (when str
                        (setq str1 (substring str 0 -2))
                        (setq str1-len (length str1))
                        (when (and (> len-snip str1-len)
                                   (string= str1 (substring orig-snippet 0 str1-len)))
                          (setq str nil))
                        (when str
                          (let ((str2 (replace-regexp-in-string "</?b>" "" str)))
                            (setq str2 (substring str2 0 (max 0 (- (length str2) 4))))
                            (when (string= str2
                                           (substring orig-url 0 (min (length orig-url)
                                                                      (length str2))))
                              (setq str nil)))))))))
                ;; Transform to our marking format.
                (when (and str (member what '("title" "snippet")))
                  (setq str (replace-regexp-in-string "<b>" "{{{"  str))
                  (setq str (replace-regexp-in-string "</b>" "}}}" str)))
                (if (not m)
                    (setq hit nil)
                  (push str hit)))))
          (when hit
            (push (reverse hit) hits)))))
    ;;(unless debug (kill-buffer buffer))
    (list num-hits (reverse hits))))

(require 'browse-url)

;; (idxgds-search "cullberg" nil "c:/")
;;;###autoload
(defun idxgds-search (search-patt file-patt root)
  (let* ((words-or-phrases (idxsearch-ggl-split search-patt))
         (or+and (idxsearch-mk-and-grep words-or-phrases))
         (grep-or-patt   (nth 0 or+and))
         (grep-and-patts (nth 1 or+and))
         (index-patts (mapcar (lambda (w-or-p)
                                (if (or t (string-match "[^a-z0-9]" w-or-p))
                                    (concat "\"" w-or-p "\"")
                                  w-or-p))
                              words-or-phrases))
         (index-patt (mapconcat 'identity index-patts " ")))
    (idxgds-search-adv index-patt grep-or-patt grep-and-patts file-patt root)))

;;;###autoload
(defun idxgds-search-adv (index-patt grep-or-patt grep-and-patts file-patt root)
  ;; (when (eq system-type 'windows-nt) (setq root (downcase root)))
  (let* (
         (query (replace-regexp-in-string " " "+"
                                          (browse-url-url-encode-chars index-patt "[)$]")))
         (query (browse-url-encode-url index-patt))
         (more t)
         (num 50)
         (start 0)
         (buffer-name "*idxsearch gds*")
         (buffer (get-buffer buffer-name))
         (cnt-hits 0)
         win
         maxw)
    ;;(setq query (replace-regexp-in-string "\"" "" query))
    (when buffer (kill-buffer buffer))
    (setq buffer (get-buffer-create buffer-name))
    (setq win (display-buffer buffer))
    (setq maxw (window-width win))
    (with-current-buffer buffer
      (idxsearch-mode)
      (setq default-directory root)
      (visual-line-mode 1)
      (setq wrap-prefix "           ")
      (orgstruct-mode)
      (let ((inhibit-read-only t))
        (insert "-*- mode: idxsearch; default-directory: \"" root "\" -*-\n")
        (insert (format " idx:  %s\ngrep:  %s %S\nfile:  %s\n\n"
                        index-patt grep-or-patt grep-and-patts file-patt))
        (insert "Search started at " (format-time-string "%Y-%m-%d %T\n\n"))
        (while (and more
                    (setq more (idxgds-raw-query query file-patt root num start)))
          ;;(message "more=%S" more)
          (setq start (+ num start))
          (let ((num-hits (car more))
                (hits (cadr more)))
            (setq cnt-hits (+ cnt-hits (length hits)))
            (dolist (hit hits)
              (let ((category (nth 0 hit))
                    (url      (nth 1 hit))
                    (snippet  (nth 2 hit))
                    (title    (nth 3 hit))
                    (icon     (nth 4 hit)))
                ;;(setq url (file-relative-name url root))
                (unless url
                  (message "error hit=%S" hit)
                  (error "%S" hit))
                (insert "* File " url " matches\n")
                (when title   (insert "  Title:   " title "\n"))
                (when snippet (insert "  Snippet: " snippet "\n"))
                (when (idxsearch-text-p url)
                  (idxsearch-grep url grep-or-patt grep-and-patts maxw))
                (sit-for 0)
                ))
            (when (> start num-hits) (setq more nil))
            ))
        (insert (format "\nMatched %d files.\n" cnt-hits))
        (insert "Search finished at " (format-time-string "%Y-%m-%d %T"))
        (message "Search finished at %s" (format-time-string "%Y-%m-%d %T"))
        ))))

(provide 'idxgds)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; idxgds.el ends here
