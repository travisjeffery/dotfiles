 completion source for Emacs hippie-expand
==========================================

This plugin provides a completion source for Emacs'
built-in `hippie-expand` auto-completion framework. 


Installation
=============

First, ensure `slime` is installed: I recommend
using packages [MELPA][melpa].

You'll need `slime` to be enabled and working, so please consult the
corresponding documentation is you have any trouble with this.

Next, install `hippie-expand-slime`. If you choose not to use the convenient
package in [MELPA][melpa], you'll need to
add the directory containing `hippie-expand-slime.el` to your `load-path`, and
then `(require 'hippie-expand-slime)`.

`hippie-expand-slime` provides a couple of `slime`-specific completion sources,
so `auto-complete` needs to be told to use them when `slime-mode` is
active. To do this, put the following code in your emacs init file to 

    (add-hook 'slime-mode-hook 'set-up-slime-hippie-expand)
    (add-hook 'slime-repl-mode-hook 'set-up-slime-hippie-expand))

Usage
=====

`hippie-expand-slime` should now automatically be enabled when you visit a buffer
in which `slime-mode` is active.

Simply trigger `hippie-expand`, and completion candidates supplied by
slime should be inserted.

More
====

See also [ac-slime](https://github.com/purcell/ac-slime), which does
the same thing but with the `auto-complete` library.


[melpa]: http://melpa.org

<hr>

[![](http://api.coderwall.com/purcell/endorsecount.png)](http://coderwall.com/purcell)

[![](http://www.linkedin.com/img/webpromo/btn_liprofile_blue_80x15.png)](http://uk.linkedin.com/in/stevepurcell)

[Steve Purcell's blog](http://www.sanityinc.com/) // [@sanityinc on Twitter](https://twitter.com/sanityinc)

