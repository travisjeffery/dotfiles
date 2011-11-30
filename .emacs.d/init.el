(require 'cl)


(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-js
                      starter-kit-eshell
                      scpaste
                      clojure-mode
                      clojure-test-mode
                      markdown-mode
                      yaml-mode
                      marmalade
                      browse-kill-ring
                      textmate
                      full-ack
                      wrap-region
                      magit
                      gist
                      coffee-mode
                      rinari
                      browse-url
                      anything
                      anything-match-plugin
                      auto-complete
                      js2-mode
                      ace-jump-mode
                      autopair
                      color-theme-sanityinc-solarized
                      )
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq-default save-place 1)

(setenv "PATH"
        (concat
         (expand-file-name "~/bin") ":"
         "/usr/local/bin" ":"
         "/usr/bin" ":"
         "/bin" ":"))

(setq exec-path
      '((expand-file-name "~/bin") 
        "/usr/local/bin" 
        "/usr/bin" 
        "/bin"))

(let ((plist (expand-file-name "~/.MacOSX/environment.plist")))
  (when (file-readable-p plist)
    (let ((dict (cdr (assq #'dict (cdar (xml-parse-file plist))))))
      (while dict
        (if (and (listp (car dict))
                 (eq 'key (caar dict)))
            (setenv (car (cddr (car dict)))
                    (car (cddr (car (cddr dict))))))
        (setq dict (cdr dict))))

    ;; Configure exec-path based on the new PATH
    (setq exec-path nil)
    (mapc #'(lambda (path)
              (add-to-list 'exec-path path))
          (nreverse (split-string (getenv "PATH") ":")))))

(set-default-font "Inconsolata-14")

(setq browse-url-browser-function 'browse-url-default-macosx-browser
      custom-file (expand-file-name "~/.emacs.d/custom.el")
      ido-handle-duplicate-virtual-buffers 2
      rinari-tags-file-name "TAGS"
      auto-mode-alist (cons '("Rakefile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("Capfile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("Gemfile" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.rake" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.god" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ru" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.gemspec" . ruby-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.markdown" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.md" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.ronn?" . markdown-mode) auto-mode-alist)
      auto-mode-alist (cons '("\\.js$" . js-mode) auto-mode-alist)
      coffee-tab-width 2
      ack-prompt-for-directory 1
      erc-nick "travisjeffery"
      erc-autojoin-channels-alist '(("freenode.net" "#github" "#logicalawesome"
                                     "#rip" "#resque" "#{" "#sinatra" "#redis"
                                     "#coffeescript" "#math" "#vim" "#emacs"
                                     "##javascript" "#thechangelog" "#clojure"))
      scroll-margin 30
      ;; from the docs: If the value is greater than 100, redisplay will never recenter point,
      ;; but will always scroll just enough text to bring point into view, even
      ;; if you move far away.
      scroll-conservatively 101
      scroll-preserve-screen-position 1

      backup-by-copying 1
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory 1))
      delete-old-versions 1
      kept-new-versions 6
      kept-old-versions 2
      version-control 1
      fill-column 79
      autopair-autowrap 1
      cua-enable-cua-keys 0
      )

(add-to-list 'load-path "~/.emacs.d/site-lisp/git-emacs")
(require 'autopair)
(add-to-list 'load-path "~/.emacs.d/site-lisp/twittering-mode")
(require 'twittering-mode)
(add-to-list 'load-path "~/.emacs.d/site-lisp/jade-mode")
(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))

(defvar anything-c-source-occur
  '((name . "Occur")
    (init . (lambda ()
              (setq anything-occur-current-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (let ((anything-occur-buffer (get-buffer-create "*Anything Occur*")))
                      (with-current-buffer anything-occur-buffer
                        (occur-mode)
                        (erase-buffer)
                        (let ((count (occur-engine anything-pattern
                                                   (list anything-occur-current-buffer) anything-occur-buffer
                                                   list-matching-lines-default-context-lines case-fold-search
                                                   list-matching-lines-buffer-name-face
                                                   nil list-matching-lines-face
                                                   (not (eq occur-excluded-properties t)))))
                          (when (> count 0)
                            (setq next-error-last-buffer anything-occur-buffer)
                            (cdr (split-string (buffer-string) "\n" t))))))))
    (action . (("Goto line" . (lambda (candidate)
                                (with-current-buffer "*Anything Occur*"
                                  (search-forward candidate))
                                (goto-line (string-to-number candidate) anything-occur-current-buffer)))))
    (requires-pattern . 3)
    (volatile)
    (delayed)))

(eval-after-load "anything"
  '(require 'anything-match-plugin))

(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map [?l] 'dired-up-directory)
     (define-key dired-mode-map [tab] 'other-window)
     (define-key dired-mode-map [(meta ?s) ?f] 'find-grep)))

(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c f") 'find-file-in-project)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c R") 'replace-string)
(global-set-key (kbd "M-z") 'zap-upto-char)
(global-set-key (kbd "M-i") 'textmate-goto-symbol)
(global-set-key (kbd "A-F") 'ack)
(global-set-key (kbd "C-TAB") 'other-window)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)
(global-set-key (kbd "C-*") 'isearch-forward-at-point)
(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-unset-key (kbd "C-x m"))

(set-default 'autopair-dont-activate #'(lambda () (eq major-mode 'sldb-mode)))
(add-hook 'js-mode '(lambda ()
                      (paredit-mode -1)
                      (autopair-mode 1)))
(autopair-global-mode)
(cua-mode 1)

(defvar autopair-modes '(r-mode ruby-mode js-mode python-mode html-mode))
(defun turn-on-autopair-mode () (autopair-mode 1))
(dolist (mode autopair-modes) (add-hook (intern (concat (symbol-name mode) "-hook")) 'turn-on-autopair-mode))
(defadvice paredit-mode (around disable-autopairs-around (arg))
  "Disable autopairs mode if paredit-mode is turned on"
  ad-do-it
  (if (null ad-return-value)
      (autopair-mode 1)
    (autopair-mode 0)))
(ad-activate 'paredit-mode)
(color-theme-sanityinc-solarized-light)
(textmate-mode)
(add-hook 'python-mode-hook '(lambda ()
                               (textmate-mode)
                               (esk-paredit-nonlisp)
                               (flyspell-prog-mode)))
(add-hook 'ruby-mode-hook '(lambda ()
                             (textmate-mode)
                             (esk-paredit-nonlisp)
                             (flyspell-prog-mode)))
(add-hook 'clojure-mode-hook '(lambda ()
                                (enable-paredit-mode)
                                (flyspell-prog-mode)))


(global-linum-mode)
(wrap-region-global-mode)

(add-hook 'coffee-mode-hook '(lambda ()
                               (define-key coffee-mode-map (kbd "C-c C-c") 'coffee-compile-file)
                               (define-key coffee-mode-map (kbd "C-c C-r") 'coffee-repl)
                               (esk-paredit-nonlisp)))

(defun zap-upto-char (arg char)
  (interactive "p\ncZap to char: ")
  (if (char-table-p translation-table-for-input)
      (setq char (or (aref translation-table-for-input) char)))
  (kill-region (point)
               (progn
                 (search-forward (char-to-string char) nil nil arg)
                 (- (point) 1)))
  (backward-char 1))

(defun clean-slate ()
  (interactive)
  (let ((buffers (buffer-list)) (safe '("*scratch*")))
    (while buffers
      (when (not (member (car buffers) safe))
        (kill-buffer (car buffers))
        (setq buffers (cdr buffers))))))

(defvar isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-ste-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
  "Interactive search forward for the symbol at point."
  (interactive "P\np")
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let * ((end (progn (skip-syntax-forward "w_") (point)))
            (begin (progn (skip-syntax-backward "w_") (point))))
         (if (eq begin end)
             (isearch-forward regexp-p no-recursive-edit)
           (setq isearch-initial-string (buffer-substring begin end))
           (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
           (isearch-forward regexp-p no-recursive-edit)))))

(defun isearch-occur ()
  "Invoike `occur' from within isearch."
  (interactive)
  (let ((case-fold-search isearch-case-fold-search))
    (occur (if isearch-regexp isearch-string (regexp-quote iserach-string)))))

(define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

(defun insert-emdash ()
  (interactive)
  (insert "—"))

(global-set-key (kbd "M-_") 'insert-emdash)
(global-set-key (kbd "C-M-=") 'esk-indent-buffer)

(add-hook 'js-mode-hook (lambda ()  
                          (defun js--proper-indentation (parse-status)
                            "Return the proper indentation for the current line."
                            (save-excursion
                              (back-to-indentation)
                              (cond
                               ((looking-at ",")
                                (let ((spos
                                       (save-excursion
                                         (while (looking-back "}[\t\n ]*")
                                           (backward-sexp)
                                           (if (looking-back ",[\t\n ]*")
                                               (re-search-backward ",[\t\n ]*")))

                                         (cond
                                          ((looking-back "\\(,\\|(\\)[ \t]*\\<\\([^:=\n\t ]+\\)[ \t\n]*")
                                           (re-search-backward "\\(,\\|(\\)[ \t]*\\<\\([^:=\n\t ]+\\)[ \t\n]*" (point-min) t)
                                           (+ (current-column) 2))

                                          ((re-search-backward "\\<\\([^:=\n\t ]+\\)[ \t]*\\(:\\|=\\)" (point-min) t)
                                           (current-column))
                                          (t 
                                           nil)))))
                                  (if spos
                                      (- spos 2)
                                    (+ js-indent-level js-expr-indent-offset))))
                               ((nth 4 parse-status)
                                (js--get-c-offset 'c (nth 8 parse-status)))
                               ((nth 8 parse-status) 0) ; inside string
                               ((js--ctrl-statement-indentation))
                               ((eq (char-after) ?#) 0)
                               ((save-excursion (js--beginning-of-macro)) 4)
                               ((nth 1 parse-status)
                                ;; A single closing paren/bracket should be indented at the
                                ;; same level as the opening statement. Same goes for
                                ;; "case" and "default".
                                (let ((same-indent-p (looking-at
                                                      "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                                      (continued-expr-p (js--continued-expression-p)))
                                  (goto-char (nth 1 parse-status)) ; go to the opening char
                                  (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                                      (progn ; nothing following the opening paren/bracket
                                        (skip-syntax-backward " ")
                                        (when (eq (char-before) ?\)) (backward-list))
                                        (back-to-indentation)
                                        (cond (same-indent-p
                                               (current-column))
                                              (continued-expr-p
                                               (+ (current-column) (* 2 js-indent-level)
                                                  js-expr-indent-offset))
                                              (t
                                               (+ (current-column) js-indent-level
                                                  (case (char-after (nth 1 parse-status))
                                                    (?\( js-paren-indent-offset)
                                                    (?\[ js-square-indent-offset)
                                                    (?\{ js-curly-indent-offset))))))
                                    ;; If there is something following the opening
                                    ;; paren/bracket, everything else should be indented at
                                    ;; the same level.
                                    (unless same-indent-p
                                      (forward-char)
                                      (skip-chars-forward " \t"))
                                    (current-column))))

                               ((js--continued-expression-p)
                                (+ js-indent-level js-expr-indent-offset))
                               (t 0))))))

(defun goto-config ()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(setq textmate-find-files-command "git ls-tree --full-tree --name-only -r HEAD")
(load custom-file 'noerror)

