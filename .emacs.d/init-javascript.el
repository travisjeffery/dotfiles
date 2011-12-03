;; On-the-fly syntax checking
(eval-after-load "js"
  '(progn
     (add-hook 'js-mode-hook 'flymake-jslint-load)))

;; ;; MMM submode regions in html
;; (eval-after-load "mmm-vars"
;;   `(progn
;;      (mmm-add-group
;;       'html-js
;;       '((js-script-cdata
;;          :submode ,preferred-mmm-javascript-mode
;;          :face mmm-code-submode-face
;;          :front "<script[^>]*>[ \t\n]*\\(//\\)?<!\\[CDATA\\[[ \t]*\n?"
;;          :back "[ \t]*\\(//\\)?]]>[ \t\n]*</script>"
;;          :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
;;                       @ "\n" _ "\n" @ "</script>" @)))
;;         (js-script
;;          :submode ,preferred-mmm-javascript-mode
;;          :face mmm-code-submode-face
;;          :front "<script[^>]*>[ \t]*\n?"
;;          :back "[ \t]*</script>"
;;          :insert ((?j js-tag nil @ "<script language=\"JavaScript\">"
;;                       @ "\n" _ "\n" @ "</script>" @)))
;;         (js-inline
;;          :submode ,preferred-mmm-javascript-mode
;;          :face mmm-code-submode-face
;;          :front "on\w+=\""
;;          :back "\"")))
;;      (dolist (mode (list 'html-mode 'nxml-mode))
;;        (mmm-add-mode-ext-class mode "\\.r?html\\(\\.erb\\)?$" 'html-js))))

(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(eval-after-load "coffee-mode"
  `(setq coffee-js-mode 'js3-mode
         coffee-tab-width 2))

(add-hook 'coffee-mode-hook 'flymake-coffee-load)

(setq inferior-js-program-command "js")
(defun add-inferior-js-keys ()
  (local-set-key "\C-x\C-e" 'js-send-last-sexp)
  (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
  (local-set-key "\C-cb" 'js-send-buffer)
  (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
  (local-set-key "\C-cl" 'js-load-file-and-go))
(add-hook 'js2-mode-hook 'add-inferior-js-keys)
(add-hook 'js-mode-hook 'add-inferior-js-keys)


(provide 'init-javascript)
