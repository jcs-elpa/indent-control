[![Build Status](https://travis-ci.com/jcs-elpa/indent-control.svg?branch=master)](https://travis-ci.com/jcs-elpa/indent-control)
[![MELPA](https://melpa.org/packages/indent-control-badge.svg)](https://melpa.org/#/indent-control)
[![MELPA Stable](https://stable.melpa.org/packages/indent-control-badge.svg)](https://stable.melpa.org/#/indent-control)
[![Release Tag](https://img.shields.io/github/v/release/jcs-elpa/indent-control.svg)](https://github.com/jcs-elpa/indent-control/releases/latest)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

# indent-control
> Management for indentation level.

Interface that combine all the indentation variables from each major mode
to one giant list.

## Features

* Decouple from user knowing each major mode's indentation level variable. (if have)
* Keep the indentation level across buffers. If you changed the indentation level
in `buffer A` and switch to `buffer B` with the same major mode; they will have
the same indentation level.

## Usage

You can tweak variable `indent-control-records ` to set the initial
indentation level for each major mode.

```el
(setq indent-control-records
  '((actionscript-mode     . 4)
    (c-mode                . 4)
    (c++-mode              . 4)
    (csharp-mode           . 4)
    ...
    (sql-mode              . 1)
    (typescript-mode       . 4)
    (web-mode              . 2)
    (yaml-mode             . 2)))
```

### Change indentation level

These functions will change the current indentation level for the major mode
that you currently on.

* `indent-control-inc-indent-level`
* `indent-control-dec-indent-level`

You can tweak variable `indent-control-delta` to change the size of the one
indentation level. The default value is `2`.

### Make indentation work across all modes

Is easy to make indentation level inherit last time modified in Emacs.
Just enable the global minor mode `indent-control-mode` then it will
automatically records all changes from the indentation level.

This minor mode is already get called in `prog-mode-hook`, but with some
major modes that they do not use inherit `prog-mode` will not work!
You would have to manually called it in each mode's startup hook.
For instance `actionscript-mode` doesn't inherit `prog-mode` so you would
have to do the following.

```el
(defun my-actionscript-mode-hook ()
  "My actionscript mode hook."
  (indent-control-continue-with-record))
(add-hook 'actionscript-mode-hook #'my-actionscript-mode-hook)
```

## Contribution

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
