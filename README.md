# indent-control
> Generic control the indentation level for each mode.

Interface that combine all the indentation variables from each major mode
to one giant list.


## Features

* Decouple from user knowing each major mode's indentation level variable. (if have)
* Keep the indentation level across buffers. If you change the indentation level
in `buffer A` and switch to `buffer B`; they will have the same indentation level.


## Usage

You can tweak variable `indent-control-records ` to set the initial
indentation level for each major mode.

```el
(setq indent-control-records
  '((actionscript-mode     . 4)
    (c-mode                . 4)
    (c++-mode              . 4)
    (csharp-mode           . 4)
    (css-mode              . 2)
    (elisp-mode            . 2)
    (emacs-lisp-mode       . 2)
    (java-mode             . 4)
    (js-mode               . 4)
    (js2-mode              . 4)
    (json-mode             . 4)
    (lisp-mode             . 2)
    (lisp-interaction-mode . 2)
    (lua-mode              . 4)
    (nasm-mode             . 4)
    (nxml-mode             . 2)
    (objc-mode             . 4)
    (python-mode           . 4)
    (ruby-mode             . 4)
    (rust-mode             . 4)
    (shader-mode           . 4)
    (sql-mode              . 1)
    (typescript-mode       . 4)
    (web-mode              . 2)
    (yaml-mode             . 2)))
```

### Change indentation level

These functions will change the current indentation level for the major mode
that you currently on.

* `indent-control-inc-tab-width`
* `indent-control-dec-tab-width`


## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
