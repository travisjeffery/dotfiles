;;; bibhlp.el --- Routines for article references handling
;;
;; Author: Lennart Borgman (lennart O borgman A gmail O com)
;; Created: 2010-12-04 Sat
;; Version: 0.2
;; Last-Updated: 2010-12-09 Thu
;; URL:
;; Keywords: bib
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
;; Help routines for handling of bibl ref, looking up doi, searching
;; library, simple parsing, some formatting, add to/lookup in your web
;; bibl ref manager.
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


(eval-when-compile (require 'mm-url))
(eval-when-compile (require 'web-vcs)) ;; autoloaded

(require 'browse-url)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Heuristic parsing of bibliographic entries

;; .bib = BibTex
;; .env = EndNote
;; .ris = Reference Manager

;;; RIS
;; TY  - JOUR
;; T1  - Training and plasticity of working memory
;; JO  - Trends in Cognitive Sciences
;; VL  - 14
;; IS  - 7
;; SP  - 317
;; EP  - 324
;; PY  - 2010/7//
;; T2  -
;; AU  - Klingberg, Torkel
;; SN  - 1364-6613
;; M3  - doi: DOI: 10.1016/j.tics.2010.05.002
;; UR  - http://www.sciencedirect.com/science/article/B6VH9-50B0K3D-1/2/962202c56e58dd3c877f2d485c1a0900
;; ER  -

(defun bibhlp-show-rec (rec)
  (if (not rec)
      (message "No data to show")
    (with-current-buffer (get-buffer-create "*BIBHLP*")
      (erase-buffer)
      (fundamental-mode)
      (let ((val (plist-get rec :authors)))
        (when val
          (dolist (v val)
            (let ((ln (nth 0 v))
                  (fn (nth 1 v))
                  (in (nth 2 v)))
              (insert "AU  - " ln ", ")
              (when fn (insert fn " "))
              (when in (insert in))
              (insert "\n")))))
      (let ((val (plist-get rec :mail)))
        (when val (insert "AD  - " val "\n")))
      (let ((val (plist-get rec :year)))
        (when val (insert "PY  - " val "\n")))
      (let ((val (plist-get rec :title)))
        (when val (insert "TI  - " val "\n")))
      (let ((val (plist-get rec :journal)))
        (when val (insert "JO  - " val "\n")))
      (let ((val (plist-get rec :volume)))
        (when val (insert "VL  - " val "\n")))
      (let ((val (plist-get rec :issue)))
        (when val (insert "IS  - " val "\n")))
      (let ((val (plist-get rec :firstpage)))
        (when val (insert "SP  - " val "\n")))
      (let ((val (plist-get rec :lastpage)))
        (when val (insert "EP  - " val "\n")))
      (let ((val (plist-get rec :doi)))
        (when val (insert "M3  - doi: DOI: " val "\n")))
      ;; :pmid
      (display-buffer (current-buffer))
      )))

(defun bibhlp-parse-entry (beg end)
  "Try to parse a bibiographic entry between BEG and END.
These defaults to (point-min) and (point-max).

Return as plist with keys

     :authors
     :mail
     :year
     :title
     :journal
     :volume
     :issue
     :firstpage
     :lastpage
     :doi
     :pmid

This is an adhoc routine to be used for convenience from
interactive functions.  Be aware that it may fail or give wrong
result.  Though I have had quite good success with it.

However what it does is first trying formats that reminds of RIS
etc.

If that fails it tries to parse it as a html code with meta tags
and other simple formats found for download on the web.

If that fails tro try to parse it like something similar to an
APA reference."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let ((here (point))
        type
        authors
        mail
        raw-authors
        year
        title
        journal
        volume
        issue
        firstpage
        lastpage
        doi
        url
        pmid
        section
        keywords
        abstract
        return-value)
    (save-restriction
      (narrow-to-region beg end)
      ;; Find out format
      (goto-char (point-min))
      (cond
       ((re-search-forward "^%\\(.\\) " nil t)
        (goto-char (point-min))
        (while (re-search-forward "^%\\(.\\) \\(.*\\)" nil t)
          (let ((mark (match-string 1))
                (val  (match-string 2)))
            (cond
             ((string= mark "0")
              (cond
               ((string= val "Journal Article") (setq type 'journal-article))
               (t (error "Unknown type: %S" val))))
             ((string= mark "T") (setq title val))
             ((string= mark "A") (setq raw-authors (cons val raw-authors)))
             ((string= mark "K") (setq keywords (cons val keywords)))
             ((string= mark "J") (setq journal val))
             ((string= mark "V") (setq volume val))
             ((string= mark "N") (setq issue val))
             ((string= mark "P")
              ;; Fix me: not sure of format
              (string-match "\\([0-9]+\\)\\(?:-\\([0-9]\\)\\)?" val)
              (setq firstpage (match-string 1 val))
              (setq lastpage (match-string 2 val))
              )
             ((string= mark "@")
              ;; Fix-me: what is it? Looks like page numbers, but much bigger.
              )
             ((string= mark "D") (setq year val))
             ((string= mark "K") (setq keywords (cons val keywords)))
             ((string= mark "X") (setq abstract val))
             ((string= mark "U") (setq url val))
             )))
        (goto-char (point-min))
        (when (re-search-forward "^ \\([^/].*/.*\\)" nil t)
          (setq doi (match-string 1))))
       ((re-search-forward "^AU +- " nil t)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Z]+\\) *- \\(.*\\)" nil t)
          (let ((mark (match-string 1))
                (val  (match-string 2)))
            (cond
             ((string= mark "TY")
              (cond
               ((string= val "JOUR") (setq type 'journal-article))
               (t (error "Unknown type: %S" val))))
             ((string= mark "T1") (setq title val))
             ((string= mark "TI") (setq title val))
             ((string= mark "JO") (setq journal val))
             ((string= mark "JT") (setq journal val))
             ((string= mark "VL") (setq volume val))
             ((string= mark "VI") (setq volume val))
             ((string= mark "IS") (setq issue val))
             ((string= mark "IP") (setq issue val))
             ((string= mark "SP") (setq firstpage val))
             ((string= mark "EP") (setq lastpage val))
             ((string= mark "PG")
              (string-match "\\([0-9]+\\)-\\([0-9]+\\)" val)
              (setq firstpage (match-string 1 val))
              (setq lastpage
                    (number-to-string (+
                                       (string-to-number firstpage)
                                       (string-to-number (match-string 2 val))))))
             ((string= mark "PY") (setq year val))
             ((string= mark "DP") (setq year (substring val 0 4)))
             ((string= mark "DEP") (setq year (substring val 0 4)))
             ((string= mark "AU") (setq raw-authors (cons val raw-authors)))
             ((string= mark "SN")
              ;; Fix-me: what is it? Looks like page numbers, but much bigger.
              )
             ((string= mark "M3")
              ;; M3  - doi: DOI: 10.1016/j.tics.2010.05.002
              (when (string-match "doi: +.*? \\(10\..*\\)" val)
                (setq doi (match-string 1 val))))
             ((string= mark "UR") (setq url val))
             ((string= mark "AB") (setq abstract val))

             ;; Used by pubmed at least:
             ((string= mark "AID")
              (when (string-match "^10\..* [doi]" val)
                (setq doi (match-string 1 val))))
             ((string= mark "PMID") (setq pmid val))
             ((string= mark "AD")
              (when (string-match thing-at-point-email-regexp val)
                (setq mail (match-string 1 val))))

             ;; There are a lot additions in for example pubmed so
             ;; just continue if we do not want it.
             (t nil)))))
       ((when (looking-at "[ \t\n]*<")
          (setq return-value (bibhlp-parse-from-html beg end)))
        nil)
       ((setq return-value (bibhlp-parse-apa-like beg end))
        nil)
       ((setq return-value (bibhlp-parse-unkown1 beg end))
        nil)
       (t (error "Unrecognized format in buffer")))
      (goto-char here)
      ;; thing-at-point pattern may catch <> around mail:
      (when (and mail (eq ?< (string-to-char mail)))
        (setq mail (substring mail 1 (- (length mail) 2))))
      ;; Lastname etc
      ;; fix-me: more to do here, initials
      (dolist (val raw-authors)
        (let (last first inits auth)
          (if (not (string-match "\\(.+\\), +\\(.+\\)" val))
              ;; Medline format perhaps:
              (if (not (string-match "\\(.+\\) +\\(.+\\)" val))
                  (setq auth (list val))
                (setq last  (match-string 1 val))
                (setq inits (match-string 2 val))
                (setq auth (list last nil inits)))
            (setq last (match-string 1 val))
            (setq first (match-string 2 val))
            (setq auth (list last first)))
          (setq authors (cons auth authors))))
      (or return-value
          (list
           :type type
           :authors authors
           :mail mail
           :keywords keywords
           :year year
           :title title
           :journal journal
           :volume volume
           :issue issue
           :firstpage firstpage
           :lastpage lastpage
           :doi doi
           :url url
           :pmid pmid)
          ))))

(defun bibhlp-parse-unkown1 (beg end)
  "Unknown.
Example:

Binswanger IA, Kral AH, Bluthenthal RN, Rybold
DJ, Edlin BR. High prevalence of abscesses and
cellulitis among community-recruited injection drug
users in San Francisco. Clin Infect Dis. 2000;
30:579–91"
  (let (auths beg-yy end-yy yy ti jo is pf pl vo doi pmid pmcid pos)
    (goto-char beg)
    (when (re-search-forward " \\([1-2][0-9]\\{3\\}\\);" end t)
      (setq yy (match-string-no-properties 1))
      (setq beg-yy (match-beginning 0))
      (setq end-yy (match-end 0))
      (goto-char beg)
      (when (search-forward "." beg-yy t)
        (let ((auth-str (buffer-substring-no-properties beg (1- (point))))
              )
          (dolist (au (split-string auth-str ",[ \n]+" t))
            (let* ((li (split-string au "[ \n]+" t))
                  (lastname (nth 0 li))
                  (initials (nth 1 li)))
              (setq auths (cons (list lastname nil initials) auths)))))
        (skip-chars-forward " \t\n")
        (setq pos (point))
        (when (search-forward "." beg-yy t)
          (setq ti (buffer-substring-no-properties pos (1- (point))))
          (setq ti (replace-regexp-in-string "[ \t\n]+" " " ti))
          (skip-chars-forward " \t\n")
          (setq pos (point))
          (when (search-forward "." beg-yy t)
            (setq jo (buffer-substring-no-properties pos (1- (point))))
            (setq jo (replace-regexp-in-string "[ \t\n]+" " " jo))
            (goto-char end-yy)
            (when (re-search-forward (concat "\\(?1:[0-9]+\\)"
                                             "\\(?:(\\(?2:[0-9]+\\))\\)?"
                                             ":\\(?3:[0-9]+\\)"
                                             "[-–]\\(?4:[0-9]+\\)"
                                             )
                                     end t)
              (setq vo (match-string 1))
              (setq is (match-string 2))
              (setq pf (match-string 3))
              (setq pl (match-string 4))
              (when (re-search-forward "\bdoi:\\(.*\\)\." end t)
                (setq doi (match-string 1)))
              (list
               :authors auths
               :year yy
               :title ti
               :journal jo
               :volume vo
               :issue is
               :firstpage pf
               :lastpage pl
               :doi doi)
              )))))))

(defun bibhlp-parse-apa-like (beg end)
  "Parse reference similar to APA style between BEG and END."
  (let (auths beg-yy end-yy yy ti doi pmid pmcid)
    ;; Find year in slightly different formats.
    (goto-char beg)
    (if (re-search-forward "(\\([0-9]\\{4\\}\\).*?)[.:]?" end t)
        (progn
          (setq beg-yy (match-beginning 0))
          (setq end-yy (match-end 0))
          (setq yy (match-string-no-properties 1)))
      (setq beg-yy end)
      (setq end-yy end))
    (when yy
      (goto-char beg)
      ;; Get authors. Formats we try to cover are:
      ;;   Saylam, C., Ucerler, H., Kitis, O., Ozand, E. and Gonul, A.S. (2006)
      ;;   Cooper P, Murray L, Wilson A, Romaniuk H (2003).
      ;;   Glezer I, Simard AR, Rivest S (2007):
      (let (;;(re-author "\\([^,]+\\),?[\w]+\\([^,\w]+\\),")
            ;;(re-author "[ \t\n]*\\([^,]*\\),[ \t\n]*\\([^,]*\\),")
            (re-author (rx (* whitespace)
                           (or (and (submatch (+ (not (any ","))))
                                    (+ whitespace)
                                    "et al."
                                    (+ whitespace)
                                    "(")
                               (and (submatch (+ (not (any ",&"))))
                                    ","
                                    (* whitespace)
                                    (? (and "&" (+ whitespace)))
                                    (submatch (+ (not (any ",&"))))
                                    (or ","
                                        (* whitespace)
                                        "(")))))
            )
        (while ( < (point) beg-yy)
          (let ((b1 (point))
                e1
                who
                lastname initials)
            (if (not (re-search-forward re-author (1+ beg-yy) t))
                (goto-char beg-yy)
              (if (match-string-no-properties 3)
                  (progn
                    (setq lastname (match-string-no-properties 2))
                    (setq initials (match-string-no-properties 3)))
                (setq lastname (match-string-no-properties 1))
                (setq initials nil))
              (setq initials (delete ?. (append initials nil)))
              (setq initials (concat initials))
              (push (list lastname nil initials) auths)))))
      ;; Get title, journal, volume, issue, pages.
      ;; Formast we try to cover are:
      ;;   Controlled trial. I. Impact on maternal mood. British Journal Psychiatry 182, 412–419
      ;;   Focal gray: A follow-up study. Neuropsychopharmacology 32:2057–2066.
      ;;   Reduced hippocampal. Surg. Radiolog. Anat., 28: 82–87.
      ;;   Microglia act: a (R)-[11C]PK11195 study. Biol Psych, 64(9), 820-822.
      ;;
      ;; All possibly followed by doi:, pmcid:, pmid: etc.
      (let ((re-ti-jo-vo-is-pg "[0-9][,:]\w*[0-9]+\\(?:-[0-9]+\\).?$")
            (re-inds "\b[^:]+:[^\w]+\w+"))
        (unless (eq beg-yy end)
          (goto-char beg-yy)
          (goto-char (search-forward ")." end t))
          (skip-syntax-forward " ")
          (let ((b1 (point))
                e1)
            (re-search-forward "[.?!:]" end t)
            (when (> (point) b1)
              (setq e1 (1- (point)))
              (setq ti (buffer-substring-no-properties b1 e1))
              (setq ti (replace-regexp-in-string "[ \t\n]+" " " ti))
              )))))
    ;; Sanity check
    (unless auths (setq yy nil))
    (when yy
      ;; doi etc
      (goto-char beg)
      (when (re-search-forward "\bdoi:\\([^ \t\n]*\\)" end t)
        (setq doi (match-string 1)))
      (goto-char beg)
      (when (re-search-forward "\bpmid:\\([^ \t\n]*\\)" end t)
        (setq pmid (match-string 1)))
      (goto-char beg)
      (when (re-search-forward "\bpmcid:\\([^ \t\n]*\\)" end t)
        (setq pmcid (match-string 1)))
      (list
       :year yy
       :authors auths
       :title ti
       :doi doi
       :pmid pmid
       :pmcid pmcid))))

(defun bibhlp-parse-from-html (beg end)
  "Parse html in buffer between BEG and END.
They default to current min and max.

Return a plist with found info, see `bibhlp-parse-entry'."
  (setq beg (or beg (point-min)))
  (setq end (or end (point-max)))
  (let (authors year title journal volume issue firstpage lastpage doi pmid section)
    (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
      (goto-char beg)
      ;; RDFa
      ;; Mediterranean Ceramics: RDFa Document Metadata: Authors in PLOS One
      ;; http://mediterraneanceramics.blogspot.com/2010/05/rdfa-document-metadata-authors-in-plos.html
      (let ((no-authors (unless authors t))
            ;; Fix-me: This pattern can't handle nested tags to get
            ;; the value. That case needs some kind of a parser. It
            ;; can probably be handled by splitting the regexp and
            ;; the search for start/end tags.
            (rdfa-re (concat "<[^<>]* property *= *\"\\(?1:[^\"]*\\)\"[^<>]*?"
                             "\\(?: content *= *\"\\(?2:[^\"]*\\)\""
                             "\\|"
                             ">\\(?3:[^<]*\\)<\\)"))
            )
        (while (re-search-forward rdfa-re end t)
          (let ((pro (match-string 1))
                (val (or (match-string 2)
                         (match-string 3)))
                (dummy (match-string 0)))
            (cond
             ;; fix-me: mostly guesses below:
             ((when no-authors (string= pro "foaf:name"))
              (let* ((mclist (split-string val " *" t))
                     (first  (nth 0 mclist))
                     (last   (nth 1 mclist)))
                (setq authors (cons (list first last) authors))))
             ((string= pro "dc:date") (setq year (substring-no-properties val 0 4)))
             ((string= pro "dc:title") (setq title val))
             ((string= pro "dc:source")
              ;; (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" "where 2010 5:87")
              (when (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" val)
                (setq journal   (match-string 1 val))
                (setq year      (match-string 2 val))
                (setq volume    (match-string 3 val))
                (setq firstpage (match-string 4 val))))
             ((string= pro "dc:identifier") (setq doi val))
             )))))
    (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
      (goto-char beg)
      (let ((no-authors (unless authors t)))
        (while (re-search-forward
                ;; "<meta +name *= *\"\\(dc\.[^\"]*\\)\" +content *= *\"\\([^\"]*\\)\""
                "<meta .*?>"
                end t)
          (let ((str (match-string-no-properties 0)))
            (save-match-data
              ;; "<meta +name *= *\"\\(dc\.[^\"]*\\)\" +content *= *\"\\([^\"]*\\)\""
              (let ((mn
                     (when (string-match " +name *= *\"\\(dc\.[^\"]*\\)\"" str)
                       (match-string-no-properties 1)))
                    (mc (when (string-match " +content *= *\"\\([^\"]*\\)\"" str)
                          (match-string-no-properties 1))))
                (when (and mn mc)
                  (cond
                   ((when no-authors (string= mn "dc.creator"))
                    (let* ((mclist (split-string mc " *" t))
                           (first  (nth 0 mclist))
                           (last   (nth 1 mclist)))
                      (setq authors (cons (list first last) authors))))
                   ((string= mn "dc.date") (setq year (substring-no-properties mc 0 4)))
                   ((string= mn "dc.title") (setq title mc))
                   ((string= mn "dc.source")
                    ;; (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" "where 2010 5:87")
                    (when (string-match "\\(.*\\) \\([0-9]\\{4\\}\\) \\([0-9]+\\):\\([0-9]+\\)$" mc)
                      (setq journal   (match-string 1 mc))
                      (setq year      (match-string 2 mc))
                      (setq volume    (match-string 3 mc))
                      (setq firstpage (match-string 4 mc))))
                   ((string= mn "dc.identifier") (setq doi mc))
                   ))))))
        (when no-authors (setq authors (reverse authors)))))
    (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
      (goto-char beg)
      (while (re-search-forward "<meta +name *= *\"\\([^\"]*\\)\" +content *= *\"\\([^\"]*\\)\"" end t)
        (let ((mn (match-string-no-properties 1))
              (mc (match-string-no-properties 2)))
          (cond
           ((string= mn "citation_authors")
            (let ((mclist (split-string mc ", *" t)))
              (setq authors (mapcar (lambda (a)
                                      (split-string a " +" t))
                                    mclist))))
           ((string= mn "citation_year") (setq year mc))
           ((string= mn "citation_title") (setq title mc))
           ((string= mn "citation_journal_title") (setq journal mc))
           ((string= mn "citation_volume") (setq volume mc))
           ((string= mn "citation_issue") (setq issue mc))
           ((string= mn "citation_firstpage") (setq firstpage mc))
           ((string= mn "citation_lastpage") (setq lastpage mc))
           ((string= mn "citation_doi") (setq doi mc))
           ))))
    (unless (and authors title journal volume issue firstpage lastpage doi pmid section)
      (goto-char beg)
      (setq authors nil) ;; fix-me
      (when (search-forward "<rdf:RDF " end t)
        (let ((beg-rdf (point))
              (end-rdf (search-forward "</rdf:RDF>"))
              (no-authors (unless authors t))
              )
          (goto-char beg-rdf)
          (while (re-search-forward "<\\([^/>]+\\)>\\([^<]+\\)<" end-rdf t)
            (let ((rf (match-string-no-properties 1))
                  (rv (match-string-no-properties 2)))
              ;;(message "rf=%S, rv=%S" rf rv)
              (cond
               ((string= rf "dc:title") (setq title rv))
               ((when no-authors (string= rf "dc:creator"))
                (let* ((names (split-string rv ", *"))
                       (firstname (nth 1 names))
                       (lastname  (nth 0 names)))
                  (setq authors (cons (list firstname lastname) authors))))
               ((string= rf "dc:identifier")
                (cond
                 ((string= "info:doi/" (substring-no-properties rv 0 9))
                  (setq doi (substring-no-properties rv 9)))
                 ((string= "info:pmid/" (substring-no-properties rv 0 10))
                  (setq pmid (substring-no-properties rv 10)))
                 (t (error "Unknown dc:identifier=%S" rv))))
               ((string= rf "dc:date") (setq year (substring-no-properties rv 0 4)))
               ((string= rf "prism:publicationName") (setq journal rv))
               ((string= rf "prism:publicationDate") (setq year (substring-no-properties rv 0 4)))
               ((string= rf "prism:volume") (setq volume rv))
               ((string= rf "prism:number") (setq issue rv)) ;; fix-me: Is this correct?
               ((string= rf "prism:startingPage") (setq firstpage rv))
               ((string= rf "prism:endingPage") (setq lastpage rv)) ;; Fix-me: Is this correct?
               )))
          (when no-authors (setq authors (reverse authors)))
          )))
    (when (or authors title doi pmid)
      (list
       :authors authors
       :year year
       :title title
       :journal journal
       :volume volume
       :issue issue
       :firstpage firstpage
       :lastpage lastpage
       :doi doi
       :pmid pmid)
      )))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Call the web for help


(defun bibhlp-doi-to-url (doi)
  (when (string-match "^doi:" doi)
    (setq doi (substring doi 4)))
  (concat "http://dx.doi.org/" doi))

;; (bibhlp-get-page "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (bibhlp-get-page "doi:10.1186/1744-859X-8-2")
(defun bibhlp-get-page (url)
  (when (string= (substring url 0 4)
                 "doi:")
    (setq url (concat "http://dx.doi.org/" url)))
  (let* ((buf-res (web-vcs-url-retrieve-synch url))
         (buf (car buf-res))
         (res (cdr buf-res)))
    (unless buf
      (error "status=%S" res))
    ;;(switch-to-buffer-other-window buf)
    buf))

;; (bibhlp-get-data-from-url "http://www.ncbi.nlm.nih.gov/pubmed/17501969")
;; (bibhlp-get-data-from-url "doi:10.1186/1744-859X-8-2")
(defun bibhlp-get-data-from-url (url)
  (when (string-match "^doi:" url)
    (setq url (bibhlp-doi-to-url url)))
  (setq url (bibhlp-resolve-url url)) ;; fix-me: should this be done or not?
  (let ((buf (bibhlp-get-page url)))
    (with-current-buffer buf
      ;;(message "buf=\n\n\n%s\n\n" (buffer-string))
      (bibhlp-parse-from-html nil nil))))

;;; CrossRef

(defvar bibhlp-crossref-post-url "http://www.crossref.org/SimpleTextQuery/")
(defvar bibhlp-crossref-form-url "http://www.crossref.org/SimpleTextQuery")
(defcustom bibhlp-crossref-user-email
  ;;nil
  "lennart.borgman@gmail.com"
  "Mail address to use for access of CrossRef.org simple query form.
This is used by `bibhlp-get-ids-from-crossref'.

See also `crossref-form-url'."
  :type 'string
  :group 'bibhlp)

(defcustom bibhlp-my-ip nil
  "Your pc:s ip as seen from outside.
Used by bibhlp-get-ids-from-crossref.

Get it from for example URL `http://www.whatismyipaddress.com/'."
  :type 'string
  :group 'bibhlp)
(setq bibhlp-my-ip "213.113.116.23")

(defvar bibhlp-ip2 "172.20.1.78"
  "Used by bibhlp-get-ids-from-crossref.
No idea what this is for address, DomainTools says it is private??")

;; (setq y (bibhlp-get-ids-from-crossref crossref-1))
;; (setq y (bibhlp-get-ids-from-crossref crossref-2))
(defun bibhlp-get-ids-from-crossref (ref)
  "Return an assoc list with DOI, PMID and PMCID for REF.
REF should be an item in a reference lists for something that
might have an DOI id.

This routine calls crossref.org \(URL `http://crossref.org') to
get those identifiers.  You must get an account there \(free for
non-commercial use) to be able to use it.  Enter the mail address
you use there in `cross-ref-user-email'.

Note: crossref.org is currently mainly for looking up DOI, but
will give you PMID and PMCID too if they are available."
  (declare (special url-http-response-status))
  (when (string-match "\\b[a-zA-Z]+:" ref)
    (setq ref (substring ref 0 (1- (match-beginning 0)))))
  ;;(message "ref=%S" ref)
  (unless bibhlp-my-ip
    (error "Please set `bibhlp-my-ip' first"))
  (let (status key1 key doi pmid pmcid)
    (message "Getting CrossRef.org fake input form...")
    (with-current-buffer
        (url-retrieve-synchronously bibhlp-crossref-post-url)
      (setq status url-http-response-status)
      (unless (= 200 status) (error "Error: status=%S" status))
      (goto-char (point-min))
      (unless (re-search-forward "\.key\.value = \"\\([^\"]*\\)\"" nil t)
        (message "\n\nCrossref error form was:\n%s" (buffer-substring-no-properties (point-min) (point-max)))
        (error "Can't find crossref first key, see *Messages*"))
      (setq key1 (match-string-no-properties 1)))
    (message "Getting CrossRef.org real input form...")
    (let* ((time (concat (format-time-string "%Y%m%d!%s") (format "%0d" (/ (nth 2 (current-time)) 1000))))
           (ip1 bibhlp-my-ip)
           (ip2 bibhlp-ip2)
           (cookie (catch 'session-id
                     (let ((cs (url-cookie-retrieve "www.crossref.org" "/SimpleTextQuery")))
                       (dolist (c cs)
                         (when (string= (aref c 1) "JSESSIONID")
                           (throw 'session-id (aref c 2)))))))
           (ret (http-post-simple bibhlp-crossref-post-url
                                  (list (cons 'key (concat
                                                    ;;cookie "_" ip2 "_" ip1 "_"
                                                    key1
                                                    time)))))
           (page (nth 0 ret))
           (res  (nth 2 ret))
           )
      (unless (= 200 res)
        (error "Crossref returned %S" res))
      (if (not (string-match "<input [^>]*id=\"key\" [^>]*value=\"\\([^\"]*\\)\"" page))
          (progn
            (message "\n\nCrossref second form was:\n%s" page)
            (error "Can't find crossref auth key, see *Messages*"))
        (setq key (match-string-no-properties 1 page)))
      ;; (message "\n\n\nCalling for translation, key=%S\n\n\n" key)
      ;; (message "Form page:\n%s\n\n\n" page)
      )
    (message "Getting CrossRef.org DOI output ...")
    (let* ((ret (http-post-simple bibhlp-crossref-post-url
                                  (list (cons 'command "Submit")
                                        (cons 'doiField "")
                                        (cons 'email bibhlp-crossref-user-email)
                                        (cons 'emailField "")
                                        (cons 'freetext ref)
                                        (cons 'key key)
                                        (cons 'password "")
                                        (cons 'username "")
                                        )
                                  nil
                                  ))
           (page (nth 0 ret))
           (res  (nth 2 ret))
           rec)
      ;;(message "\n \n\n\n\nPage=\n%s" page)
      (if (= 200 res)
          (progn
            ;;(when (string-match "href=\"http://dx.doi.org/\\([^\"]*\\)\"" page)
            (when (string-match ">doi:\\([^<]*\\)<" page)
              ;;(message "\n\n\nPage=\n%s\n\n\n" page)
              (setq doi (match-string-no-properties 1 page)))
            (when (string-match "\\bPMid:\\([0-9]+\\)" page)
              (setq pmid (match-string-no-properties 1 page)))
            (when (string-match "\\bPMCid:\\([0-9]+\\)" page)
              (setq pmcid (match-string-no-properties 1 page)))
            )
        (message "\n\nCrossref final output was:\n%s" page)
        (error "Status=%S when trying to get final result" status)))
    (let (ret)
      (when pmcid
        (push (cons 'pmcid pmcid) ret))
      (when pmid
        (push (cons 'pmid pmid) ret))
      (when doi
        (push (cons 'doi doi) ret))
      ret)))


;;; Resolving urls

;; &an=00019616-200903000-00013
;; (bibhlp-get-ids-from-crossref "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; err
;; (bibhlp-goto-citeulike "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok
;; (http-head-simple-internal "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok
;; (bibhlp-resolve-url "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; err
;; (browse-url "http://dx.doi.org/10.1097/TEN.0b013e318198b9b8") ;; ok - but not to actual page
;; (bibhlp-resolve-url "http://www.sunet.se/")
;; (bibhlp-resolve-url "http://www.google.com/")
;; (bibhlp-resolve-url "http://www.google.se/")
;; (bibhlp-resolve-url "http://dx.doi.org/10.1016/j.biopsych.2008.04.025")
;; (browse-url "http://dx.doi.org/10.1016/j.biopsych.2008.04.025")
(defun http-head-simple-internal (url)
  "Make a http head call for URL.
Return a list with status and returned url."
  (let ((url-request-method        "HEAD")
	(url-request-data          nil)
	(url-request-extra-headers nil)
        ;;(url-mime-charset-string   (http-post-charset-name charset))
        )
    (let (header
	  data
	  status
          current-url)
      (with-current-buffer (url-retrieve-synchronously url)
        ;; fix-me: I have no idea of what this `declare' does... -
        ;; there is no doc string for it. It is of course a cl
        ;; thing...
        (declare (special url-http-response-status))
        ;;(setq xbuf (current-buffer))
        ;;(setq xbuf-name (buffer-name))
        (setq current-url (url-recreate-url url-current-object))
	;; status
	(setq status url-http-response-status)
	;; ;; return the header and the data separately
	;; (goto-char (point-min))
	;; (if (search-forward-regexp "^$" nil t)
	;;     (setq header (buffer-substring (point-min) (point))
	;; 	  data   (buffer-substring (1+ (point)) (point-max)))
        ;;   ;; unexpected situation, return the whole buffer
        ;;   (setq data (buffer-string)))
        )
      (message "A:status=%S, current-url=%S" status current-url)
      (unless (memq status '(200))
        (setq url-request-method "GET")
        (with-current-buffer (url-retrieve-synchronously url)
          (declare (special url-http-response-status))
          (setq current-url (url-recreate-url url-current-object))
          (setq status url-http-response-status))
        (message "B:status=%S, current-url=%S" status current-url))
      (values status current-url))))

(defun bibhlp-resolve-url (url)
  "Return url after redirections from URL.
Does not take care of browser reload functions."
  (let ((new-url url)
        old-url
        res
        status
        done)
    (while (not (equal new-url old-url))
      (setq old-url new-url)
      (setq res (http-head-simple-internal old-url))
      (message "res=%S" res)
      (setq status (nth 0 res))
      (unless (memq status '(200 302))
        (error "Returned status=%S" status))
      (setq new-url (nth 1 res)))
    new-url
    ))


;;; ParsCit

(defvar bibhlp-parscit-post-url "http://aye.comp.nus.edu.sg/parsCit/parsCit.cgi")

(defvar bibhlp-parscit-bib-re (rx buffer-start
                                  (*? anything)
                                  (submatch
                                   "&lt;algorithm"
                                   (*? anything)
                                   "/algorithm&gt;"
                                   )
                                  (* anything)
                                  buffer-end)
  "Get the <algorithm> rec from the output.
This is html encoded, but looks like this after decoding:

<algorithm name=\"ParsCit\" version=\"10111111111111111111111101\">
  <citationList>
    <citation>
      <authors>
        <author>G P Amminger</author>
        <author>M R Schafer</author>
        <author>K Papageorgiou</author>
        <author>C M Klier</author>
        <author>S M Cotton</author>
        <author>S M Harrigan</author>
        <author>A Mackinnon</author>
      </authors>
      <volume>67</volume>
      <date>2010</date>
      <title>Long-Chain {omega}-3 Fatty Acids: A Randomized, Placebo-Controlled Trial</title>
      <journal>Arch Gen Psychiatry</journal>
      <pages>146--154</pages>
    </citation>
  </citationList>
</algorithm>")

;; (setq x (parscit-post-reference parscit-post-1))
(defun parscit-post-reference (ref)
  (message "\n\n\nparsit.ref=%S\n\n\n" ref)
  (let* ((ret (http-post-simple-multipart bibhlp-parscit-post-url
                                          (list (cons 'demo "3")
                                                (cons 'textlines ref)
                                                ;;(cons 'end3 "on")
                                                (cons 'ris3 "on")
                                                (cons 'SUBMIT "Parse these lines!")
                                                )
                                          nil
                                          ))
         (page (nth 0 ret))
         (res  (nth 2 ret))
         str-rec
         authors title date journal volume issue pages)
    (if (/= 200 res)
        (message "Could not get rec")
      (setq str-rec
            (mm-url-decode-entities-string
             (replace-regexp-in-string bibhlp-parscit-bib-re "\\1" page)))
      (message "\n\n\nstr-rec:\n%s\n\n\n" str-rec)
      (setq str-rec (split-string str-rec "[\n]" t))
      (dolist (str str-rec)
        (when (string-match "<author>\\([^<]*\\)<" str)  (setq authors (cons (match-string 1 str) authors)))
        (when (string-match "<title>\\([^<]*\\)<" str)   (setq title (match-string 1 str)))
        (when (string-match "<date>\\([^<]*\\)<" str)    (setq date (match-string 1 str)))
        (when (string-match "<journal>\\([^<]*\\)<" str) (setq journal (match-string 1 str)))
        (when (string-match "<volume>\\([^<]*\\)<" str)  (setq volume (match-string 1 str)))
        (when (string-match "<issue>\\([^<]*\\)<" str)   (setq issue (match-string 1 str)))
        (when (string-match "<number>\\([^<]*\\)<" str)   (setq issue (match-string 1 str)))
        (when (string-match "<pages>\\([^<]*\\)<" str)   (setq pages (match-string 1 str)))
        ))
    ;; Fix-me: try to normalize authors, making it three components: last, first, initials.
    (let ((norm-authors nil))
      (dolist (author authors)
        ;; (dolist (author
        ;;          '(
        ;;            "cullberg, johan"
        ;;            "cullberg, j. n."
        ;;            "cullberg, jon n."
        ;;            "cullberg allan"
        ;;            ))
        (let (last first inits temp)
          (cond
           ((string-match "^\\([^\w]+?\\),? +\\([^\w]+?\\)\\(?:[.]? +\\(.*\\)\\)?$" author)
            (setq last  (match-string 1 author))
            (setq first (match-string 2 author))
            (setq inits (match-string 3 author))
            (when inits
              (setq inits (replace-regexp-in-string "[.\w]" "" inits)))
            (setq last  (capitalize last))
            (setq first (capitalize first))
            (message "res=%S" (list last first inits))
            (setq norm-authors (cons (list last first inits) norm-authors))
            )
           (t (error "Could not match author=%S" author)))))
      (setq authors norm-authors))

    ;;(setq authors (reverse authors))
    (list
     :authors  authors
     :title  title
     :date  date
     :journal  journal
     :volume  volume
     :issue  issue
     :pages  pages
     )))

;;; PMID, PMC, Pubmed, PubmedCentral

;; (bibhlp-pmid2pmcid "19877500" "pubmed")
;; (bibhlp-pmid2pmcid "2804881" "pmc")
(defun bibhlp-pmid2pmcid (id from)
  "Convert ID format from FROM to alternate format.
Pubmed and Pubmed Central use different id:s.  This routine
converts between them by calling the conversion routines at NCBI,
see URL `http://www.ncbi.nlm.nih.gov/'.

FROM should be either \"pubmed\" or \"pmc\"."
  (let ((known-from '("pubmed" "pmc")))
    (unless (member from known-from) (error "Value from=%S must be in %S" from known-from)))
  (unless (stringp id) (error "Parameter id=%S should be a string" id))
  (let* ((post-res (http-post-simple "http://www.ncbi.nlm.nih.gov/sites/pmctopmid"
                                     (list (cons 'PMCRALayout.PMCIDS.PMCIDS_Portlet.Db from)
                                           (cons 'PMCRALayout.PMCIDS.PMCIDS_Portlet.Ids id)
                                           (cons 'p$a "PMCRALayout.PMCIDS.PMCIDS_Portlet.Convert")
                                           (cons 'p$l "PMCRALayout")
                                           (cons 'p$st "pmctopmid")
                                           )))
         (page    (nth 0 post-res))
         (status  (nth 2 post-res))
         decoded-page)
    (unless (= 200 status) "Error: status=%S" status)
    (setq decoded-page
          (mm-url-decode-entities-string
           (replace-regexp-in-string bibhlp-parscit-bib-re "\\1" page)))
    (message "\n\n\npage:\n%s\n\n\n" decoded-page)
    (when (cond
           ((string= from "pubmed") (string-match ">PMC\\([0-9]+\\)</td>" decoded-page))
           ((string= from "pmc")    (string-match ">\\([0-9]+\\)</td>" decoded-page))
           (t (error "Bad from=%S" from)))
      (match-string-no-properties 1 decoded-page))))


;;; ELIN at lu.se

(defun bibhlp-make-elin-search-string (rec)
  "Make a search string for ELIN from REC."
  (let ((txt nil))
    (dolist (auth (plist-get rec :authors))
      (let ((lastname (car auth)))
        (when txt (setq txt (concat txt " AND ")))
        (when (string-match-p " " lastname)
          (setq lastname (concat "\"" lastname "\"")))
        (setq txt (concat txt "au:" lastname))))
    (let ((ti (plist-get rec :title)))
      (when ti
        (dolist (tw (split-string ti "[][ \f\t\n\r\v!.:,()-]" t))
          (when (< 7 (length tw))
            (when txt (setq txt (concat txt " AND ")))
            (setq txt (concat txt "ti:" tw))))))
    (kill-new txt)
    txt))

(defun bibhlp-search-rec-in-elin (rec)
  "Go to ELIN@Lund and look for REC.
REC should be a bibliographic record in the format returned from
`bibhlp-parse-entry'."
  (let ((txt (bibhlp-make-elin-search-string rec)))
    (let ((url (concat "http://elin.lub.lu.se.ludwig.lub.lu.se/elin?func=advancedSearch&lang=se&query="
                       (browse-url-encode-url txt))))
      ;; Fix-me: Using Opera at the moment due to Chrome bug when displaying pdf:
      (if nil
          (browse-url url)
        (if (eq system-type 'windows-nt)
            (w32-shell-execute nil "C:/Program Files/Opera/opera.exe" url)
          (start-process "Opera" nil "C:/Program Files/Opera/opera.exe" url))
        ))))

(defun bibhlp-open-in-firefox (url)
  (if (eq system-type 'windows-nt)
      (w32-shell-execute nil "C:/Program Files/Mozilla Firefox/firefox.exe" url)
    (start-process "Opera" nil "C:/Program Files/Opera/opera.exe" url))
  )

(defun bibhlp-make-apa (rec no-empty)
  "Make an APA style ref from REC."
  (let ((str nil))
    (let ((authors (plist-get rec :authors)))
      (setq str (concat str
                        ;; Fix-me:
                        (let ((auth-strs (mapcar (lambda (a)
                                                   (let* ((l (nth 0 a))
                                                          (f (nth 1 a))
                                                          (i (nth 2 a))
                                                          (inits (split-string (concat
                                                                                (when f (substring f 0 1))
                                                                                i))))
                                                     (concat l ", "
                                                             (mapconcat 'identity inits ".")
                                                             (when inits (concat ".")))))
                                                 authors)))
                          (concat (mapconcat 'identity (butlast auth-strs) ", ")
                                  (when (cdr authors) (concat " & "
                                                              (car (last auth-strs)))))))))
    (setq str (concat str
                      " (" (or (plist-get rec :year) "n.d.") ")."))
    (let ((ti (plist-get rec :title)))
      (when (or ti (not no-empty))
        (setq str (concat str " " (or ti "NO-TI") "."))))
    (let ((jo (plist-get rec :journal)))
      (when (or jo (not no-empty))
        (setq str (concat str " /" (or jo "NO-JO")
                          "/,"))))
    (let ((vl (plist-get rec :volume)))
      (when (or vl (not no-empty))
        (setq str (concat str " " (or vl "NO-VL")))))
    (let ((is (plist-get rec :issue)))
      (when (or is (not no-empty))
        (setq str (concat str "(" (or is "NO-IS") "),"))))
    (let ((pf (plist-get rec :firstpage)))
      (when (or pf (not no-empty))
        (setq str (concat str " " (or pf "NO-PF")))
        (let ((pe (plist-get rec :lastpage)))
          (when pe
            (setq str (concat str "-" pe))))
        (setq str (concat str "."))))
    (let ((doi (plist-get rec :doi)))
      (when doi
        (setq str (concat str " doi:" doi))))
    (let ((pmid (plist-get rec :pmid)))
      (when pmid
        (setq str (concat str " pmid:" pmid))))
    (let ((pmcid (plist-get rec :pmcid)))
      (when pmcid
        (setq str (concat str " pmid:" pmcid))))
    str))

(defvar bibhlp-marking-ovl nil)
(make-variable-buffer-local 'bibhlp-marking-ovl)

(defun bibhlp-find-reftext-at (point include-doi-etc)
  "Found boundary for possible bibl ref at POINT.
If INCLUDE-DOI-ETC then include those \(they are supposed to be at the end).

Return a list \(BEG END)."
  (let* ((here (point))
         (end (progn
                (forward-paragraph)
                (skip-chars-backward " \t\n\f")
                (point)))
         (beg (progn
                (backward-paragraph)
                (skip-chars-forward " \t\n\f")
                (point))))
    (unless include-doi-etc
      (when (re-search-forward "\\(?:\\[\\[\\|https?://\\)" end t)
        (goto-char (match-beginning 0))
        (setq end (point))))
    (goto-char here)
    (list beg end)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Commands

(defun bibhlp-prompt ()
  (let ((cwcfg (current-window-configuration))
        (buf (get-buffer-create "*BIBHLP Promp*"))
        ev
        to-msg
        done)
    (unwind-protect
        (progn
          (display-buffer buf)
          (with-current-buffer buf
            (insert "hej")
            )
          (while (not done)
            (setq ev (read-event))
            (message "ev=%S" ev)
            (cond
             ((eq ev ?g)
              (message "rec g")
              (setq done t))
             (t
              (if (eq (lookup-key global-map (vector ev)) 'self-insert-command)
                  (message "There is no alternative '%c'" ev)
                (setq unread-command-events (list ev))
                (setq done t)
                )
              )
             )
            ))
      (set-window-configuration cwcfg)
      (kill-buffer buf))))

(defun bibhlp-alternatives-for-url (url)
  (let ((prompt (concat "
What do you want to do with the url at point?

  g - Just show in web browser
  e - Find in org mode buffers
  E - Find in org mode files
  c - Goto CiteULike, add or show
  p - Get page and try to parse it for bibl data
  f - Show in Firefox (so you can add it to Zotero)
"
                        ))
        done cc)
    (while (not done)
      (setq cc (read-char-exclusive prompt))
      (setq done t)
      (cond
       ((eq cc ?g)
        (browse-url url))
       ((eq cc ?e)
        (orgfl-find-links-in-org-buffers url))
       ((eq cc ?E)
        (orgfl-find-links-in-org-files url nil nil))
       ((eq cc ?c)
        (bibhlp-goto-citeulike url))
       ((eq cc ?p)
        (let ((rec (bibhlp-get-data-from-url url)))
          (bibhlp-show-rec rec)))
       ((eq cc ?f)
        (bibhlp-open-in-firefox url))
       (t (setq done nil))))
    ))

(defun bibhlp-alternatives-for-entry ()
  (let (beg end)
    (if mark-active
        (progn
          (setq beg (region-beginning))
          (setq end (region-end)))
      (let ((be (bibhlp-find-reftext-at (point) nil)))
        (setq beg (nth 0 be))
        (setq end (nth 1 be)))
      (if bibhlp-marking-ovl
          (move-overlay bibhlp-marking-ovl beg end)
        (setq bibhlp-marking-ovl (make-overlay beg end))
        (overlay-put bibhlp-marking-ovl 'face 'secondary-selection)))
    (let ((prompt (concat "
What do you want to do with the marked entry?

  a - APA style
  p - Parse
  c - ParsCit
  x - CrossRef
  e - ELIN@Lund
"
                          ))
          done cc
          (str (buffer-substring-no-properties beg end)))
      (unwind-protect
          (while (not done)
            (setq cc (read-char-exclusive prompt))
            (setq done t)
            (cond
             ((eq cc ?p)
              (bibhlp-show-rec (bibhlp-parse-entry beg end)))
             ((eq cc ?c)
              (bibhlp-show-rec (parscit-post-reference str)))
             ((eq cc ?x)
              (let ((ret (bibhlp-get-ids-from-crossref str))
                    )
                (with-current-buffer (get-buffer-create "*BIBHLP*")
                  (erase-buffer)
                  (org-mode)
                  (dolist (r ret)
                    (let ((k (car r))
                          (v (cdr r)))
                      (insert (format "%s:%s\n" k v))))
                  (display-buffer (current-buffer)))))
             ((eq cc ?e)
              ;;(bibhlp-search-ref-at-point-in-elin)
              (let ((rec (bibhlp-parse-entry beg end)))
                (bibhlp-search-rec-in-elin rec)))
             ;;(bibhlp-search-rec-in-elin rec)
             ((eq cc ?a)
              (let* ((rec (bibhlp-parse-entry beg end))
                     (str (bibhlp-make-apa rec nil)))
                (with-current-buffer (get-buffer-create "*BIBHLP*")
                  (erase-buffer)
                  (org-mode)
                  (insert str)
                  (display-buffer (current-buffer)))))
             (t (setq done nil))))
        (when (and bibhlp-marking-ovl
                   (overlay-buffer bibhlp-marking-ovl))
          (delete-overlay bibhlp-marking-ovl))))))

;;;###autoload
(defun bibhlp ()
  "Big Question aka simple entry point.
Will give you a list of what you can do with the bibl ref or url
at point.

"
  (interactive)
  (catch 'top-level
    (let ((url (or (when (derived-mode-p 'org-mode)
                     (org-url-at-point))
                   (url-get-url-at-point))))
      (if url
          (bibhlp-alternatives-for-url url)
        (bibhlp-alternatives-for-entry)))))

;; (defun bibhlp-goto-article ()
;;   (interactive)
;;   )

;; (bibhlp-goto-citeulike "http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2804881/")
;; (bibhlp-goto-citeulike (concat "http://dx.doi.org/" "doi:10.1001/archgenpsychiatry.2009.192"))

;;;###autoload
(defun bibhlp-goto-citeulike (article-url)
  "Open CiteULike in a web browser and open the article from ARTICLE-URL.
If this article have not been added to CiteULike then you can add
it when the browser opens CitULike.  Othwise the already added
article will be shown.

See URL `http://www.citeulike.org/' for info about CiteULike.

Note: CiteULike is a privately owned site sponsored by Springer
and with close source.  However you can have your data private
and it looks like data can be shared/exported to Zotero later."
  (interactive (list (or (org-url-at-point)
                         (read-string "Article URL: "))))
  (let* ((citeurl (concat "http://www.citeulike.org/posturl?username=beogl&bml=nopopup&url="
                          article-url)))
    (browse-url citeurl)))

;;;###autoload
(defun bibhlp-search-ref-at-point-in-elin ()
  "Try to find bibliographic at point in ELIN@Lund.
ELIN@Lund is Lunds University Library, URL `http://www.lub.lu.se/en.html'."
  (interactive)
  (let ((rec (bibhlp-parse-entry nil nil)))
    (bibhlp-search-rec-in-elin rec)))


(provide 'bibhlp)
;; Local variables:
;; coding: utf-8
;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; bibhlp.el ends here
