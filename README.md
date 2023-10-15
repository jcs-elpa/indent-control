[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![JCS-ELPA](https://raw.githubusercontent.com/jcs-emacs/badges/master/elpa/v/indent-control.svg)](https://jcs-emacs.github.io/jcs-elpa/#/indent-control)
[![MELPA](https://melpa.org/packages/indent-control-badge.svg)](https://melpa.org/#/indent-control)
[![MELPA Stable](https://stable.melpa.org/packages/indent-control-badge.svg)](https://stable.melpa.org/#/indent-control)

# indent-control
> Management for indentation level.

[![CI](https://github.com/jcs-elpa/indent-control/actions/workflows/test.yml/badge.svg)](https://github.com/jcs-elpa/indent-control/actions/workflows/test.yml)

Interface that combine all the indentation variables from each major mode
to one giant list.

## 🏆 Features

* Decouple from user knowing each major mode's indentation level variable. (if have)
* Keep the indentation level across buffers. If you changed the indentation level
in `buffer A` and switch to `buffer B` with the same major mode; they will have
the same indentation level.

## 🔨 Usage

You can tweak variable `indent-control-records ` to set the initial
indentation level for each major mode.

```elisp
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

* `indent-control-inc`
* `indent-control-dec`

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

```elisp
(defun my-actionscript-mode-hook ()
  "My actionscript mode hook."
  (indent-control-continue-with-record))
(add-hook 'actionscript-mode-hook #'my-actionscript-mode-hook)
```

## Contribute

[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](http://makeapullrequest.com)
[![Elisp styleguide](https://img.shields.io/badge/elisp-style%20guide-purple)](https://github.com/bbatsov/emacs-lisp-style-guide)
[![Donate on paypal](https://img.shields.io/badge/paypal-donate-1?logo=paypal&color=blue)](https://www.paypal.me/jcs090218)
[![Become a patron](https://img.shields.io/badge/patreon-become%20a%20patron-orange.svg?logo=patreon)](https://www.patreon.com/jcs090218)

If you would like to contribute to this project, you may either
clone and make pull requests to this repository. Or you can
clone the project and establish your own branch of this tool.
Any methods are welcome!
