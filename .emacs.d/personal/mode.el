                                        ; (add-to-list 'load-path "~/.emacs.d/vendor/evil")
                                        ; (require 'evil)
                                        ; (evil-mode 1)

                                        ; wrap region with tags and
                                        ; puncation, similar to what I
                                        ; had with surround.vim
(vendor 'wrap-region)
(wrap-region-global-mode t)

(textmate-mode t)

(vendor 'fuzzy-find-in-project)
(vendor 'eproject)
(require 'eproject-extras)

(define-project-type ruby (generic)
  (look-for "Gemfile"))

(define-project-type emacs (generic)
  (look-for "init.el"))

(browse-kill-ring-default-keybindings)

(setq ack-prompt-for-directory t)

(setenv "PATH" "/Users/travis/.rvm/gems/ruby-head/bin:/Users/travis/.rvm/gems/ruby-head@global/bin:/Users/travis/.rvm/rubies/ruby-head/bin:/Users/travis/.rvm/bin:/Users/travis/bin:/usr/local/bin:/Users/travis/.rbenv/shims:/Users/travis/.rbenv/bin:/Users/travis/.lein/bin/:/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/X11/bin:/usr/X11/bin")

(whitespace-mode 0)
