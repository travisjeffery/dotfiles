
Emacs AutoFmt
=============

This is a utility to auto-format Emacs lisp.

This is a command line tool which requires Python3.8 as well as
an Emacs utility to run this tool on saving.

Projects using this:

- `diff-at-point <https://gitlab.com/ideasman42/emacs-diff-at-point>`__
- `hl-block-mode <https://gitlab.com/ideasman42/emacs-hl-block-mode>`__
- `run-stuff <https://gitlab.com/ideasman42/emacs-run-stuff>`__
- `scoll-on-drag <https://gitlab.com/ideasman42/emacs-scroll-on-drag>`__
- `spell-fu <https://gitlab.com/ideasman42/emacs-spell-fu>`__
- `undo-fu <https://gitlab.com/ideasman42/emacs-undo-fu>`__
- `undo-fu-session <https://gitlab.com/ideasman42/emacs-undo-fu-session>`__

*Currently my projects, just to give examples of how it works.*


Motivation
----------

This tool removes the need to manually format and indent code,
it can be useful to re-arrange code without the need to manually reformat it.


Features
--------

- Enforces maximum line width (using the fill column).
- Consistent 2 space indentation.
- Keeps blank lines.
- Keeps trailing comments at the end of lines.
- Extracts function arguments from locally defined functions.
- Experimental support for exporting function argument lengths.

  *Currently writes all function information making it slow.*


Usage
-----

The save hook can be enabled in the mode hook.

Since it's likely you will work on code-bases that *don't* have auto-formatting enabled,
this checks for the existence of an ``.elisp-autofmt`` file in the buffers directory (including parent paths).

.. code-block:: elisp

   (add-hook 'emacs-lisp-mode-hook
     (lambda ()
       (require 'elisp-autofmt)
       (elisp-autofmt-save-hook-for-this-buffer)))

.. note::

   ``.elisp-autofmt`` will eventually be used for configuration, for now it should be left empty.


Functions
^^^^^^^^^

``(elisp-autofmt-buffer &optional buf)``
   Auto formats the current buffer (or ``buf``).
``(elisp-autofmt-save-hook-for-this-buffer &optional force)``
   Setup auto-formatting for this buffer, optionally you can pass in ``force`` = ``t``
   to enable auto-formatting even when ``.elisp-autofmt`` isn't found.


Customization
-------------

``elisp-autofmt-empty-line-max``
   The maximum number of empty lines to keep (default ``2``).
``elisp-autofmt-use-function-defs``
   When non-nil, use function argument lengths generated from Emacs (default ``nil``).

TODO
----

- Use: ``.elisp-autofmt`` as a configuration file.

- Support conventional ``let`` formatting:

  .. code-block:: elisp

     ;; Support this.
     (let ((foo 1)
           (bar 2))
       *body*)

     ;; As an alternative to this.
     (let
       (
         (foo 1)
         (bar 2))
       *body*)

  *Moving away from the current rule of fixed 2 space indentation.*

- Scan the buffer for functions, only writing function data for functions in use.
- Use the indentation width from Emacs (currently fixed to 2).
- Support un-escaped character literals.
