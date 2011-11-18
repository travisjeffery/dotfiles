(setq
   backup-by-copying t
   backup-directory-alist
    '(("." . "~/.tmp"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t)


(vendor 'color-theme-solarized)
(color-theme-solarized-light)
