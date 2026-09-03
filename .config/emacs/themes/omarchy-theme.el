;;; omarchy-theme.el --- Omarchy theme synced from system colors -*- lexical-binding: t -*-

(deftheme omarchy "Omarchy theme synced from system colors.")

(require 'omarchy-colors)

(let* ((light (if (fboundp 'omarchy-light-theme-p)
                  (omarchy-light-theme-p)
                ;; Standalone fallback (theme loaded without omarchy.el):
                ;; honor the light.mode marker at either Omarchy location.
                (or (file-exists-p "~/.local/state/omarchy/current/theme/light.mode")
                    (file-exists-p "~/.config/omarchy/current/theme/light.mode"))))
       (highlight-bg          (if light omarchy-color-bright-white omarchy-color-bright-black))
       (mode-line-inactive-bg (if light omarchy-color-bright-white omarchy-color-black))
       (line-number-fg        (if light omarchy-color-bright-black omarchy-color-blue))
       (variable-name-fg      (if light omarchy-color-black         omarchy-color-bright-white))
       (completions-first-fg  (if light omarchy-color-black         omarchy-color-bright-white))
       (comment-attrs (if light
                          `(:foreground ,omarchy-color-bright-black)
                        `(:foreground ,omarchy-color-bright-blue :slant italic))))
  (custom-theme-set-faces
   'omarchy

   ;; Core faces
   `(default ((t (:foreground ,omarchy-color-fg :background ,omarchy-color-bg))))
   `(cursor ((t (:background ,omarchy-color-cursor))))
   `(region ((t (:foreground ,omarchy-color-sel-fg :background ,omarchy-color-sel-bg))))
   `(highlight ((t (:background ,highlight-bg))))
   `(fringe ((t (:background ,omarchy-color-bg))))
   `(vertical-border ((t (:foreground ,omarchy-color-bright-black))))

   ;; Mode line
   `(mode-line ((t (:foreground ,omarchy-color-bg :background ,omarchy-color-accent :box nil))))
   `(mode-line-inactive ((t (:foreground ,omarchy-color-bright-black :background ,mode-line-inactive-bg :box nil))))
   `(mode-line-buffer-id ((t (:weight bold))))

   ;; Line numbers
   `(line-number ((t (:foreground ,line-number-fg :background ,omarchy-color-bg))))
   `(line-number-current-line ((t (:foreground ,omarchy-color-accent :background ,omarchy-color-bg :weight bold))))

   ;; Minibuffer / prompts
   `(minibuffer-prompt ((t (:foreground ,omarchy-color-accent :weight bold))))

   ;; Search
   `(isearch ((t (:foreground ,omarchy-color-bg :background ,omarchy-color-yellow))))
   `(lazy-highlight ((t (:foreground ,omarchy-color-bg :background ,omarchy-color-bright-yellow))))

   ;; Font lock (syntax highlighting)
   `(font-lock-keyword-face ((t (:foreground ,omarchy-color-magenta))))
   `(font-lock-function-name-face ((t (:foreground ,omarchy-color-blue))))
   `(font-lock-function-call-face ((t (:foreground ,omarchy-color-blue))))
   `(font-lock-variable-name-face ((t (:foreground ,variable-name-fg))))
   `(font-lock-string-face ((t (:foreground ,omarchy-color-green))))
   `(font-lock-comment-face ((t ,comment-attrs)))
   `(font-lock-comment-delimiter-face ((t ,comment-attrs)))
   `(font-lock-type-face ((t (:foreground ,omarchy-color-yellow))))
   `(font-lock-constant-face ((t (:foreground ,omarchy-color-white))))
   `(font-lock-builtin-face ((t (:foreground ,omarchy-color-cyan))))
   `(font-lock-preprocessor-face ((t (:foreground ,omarchy-color-red))))
   `(font-lock-warning-face ((t (:foreground ,omarchy-color-red :weight bold))))
   `(font-lock-doc-face ((t (:foreground ,omarchy-color-bright-green))))
   `(font-lock-number-face ((t (:foreground ,omarchy-color-white))))
   `(font-lock-negation-char-face ((t (:foreground ,omarchy-color-red))))
   `(font-lock-operator-face ((t (:foreground ,omarchy-color-cyan))))

   ;; Parentheses
   `(show-paren-match ((t (:foreground ,omarchy-color-bg :background ,omarchy-color-accent :weight bold))))
   `(show-paren-mismatch ((t (:foreground ,omarchy-color-bg :background ,omarchy-color-red :weight bold))))

   ;; Completion
   `(completions-common-part ((t (:foreground ,omarchy-color-accent))))
   `(completions-first-difference ((t (:foreground ,completions-first-fg :weight bold))))

   ;; Links
   `(link ((t (:foreground ,omarchy-color-cyan :underline t))))
   `(link-visited ((t (:foreground ,omarchy-color-magenta :underline t))))

   ;; Errors / warnings
   `(error ((t (:foreground ,omarchy-color-red :weight bold))))
   `(warning ((t (:foreground ,omarchy-color-yellow :weight bold))))
   `(success ((t (:foreground ,omarchy-color-green :weight bold))))

   ;; Org mode
   `(org-level-1 ((t (:foreground ,omarchy-color-blue :weight bold :height 1.1))))
   `(org-level-2 ((t (:foreground ,omarchy-color-magenta :weight bold))))
   `(org-level-3 ((t (:foreground ,omarchy-color-cyan :weight bold))))
   `(org-level-4 ((t (:foreground ,omarchy-color-yellow))))
   `(org-code ((t (:foreground ,omarchy-color-green))))
   `(org-verbatim ((t (:foreground ,omarchy-color-white))))
   `(org-link ((t (:foreground ,omarchy-color-cyan :underline t))))
   `(org-done ((t (:foreground ,omarchy-color-green :weight bold))))
   `(org-todo ((t (:foreground ,omarchy-color-red :weight bold))))

   ;; Diff / ediff
   `(diff-added ((t (:foreground ,omarchy-color-green :background unspecified))))
   `(diff-removed ((t (:foreground ,omarchy-color-red :background unspecified))))
   `(diff-header ((t (:foreground ,omarchy-color-cyan :weight bold))))
   `(diff-file-header ((t (:foreground ,omarchy-color-blue :weight bold))))))

(provide-theme 'omarchy)
;;; omarchy-theme.el ends here
