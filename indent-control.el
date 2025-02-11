;;; indent-control.el --- Management for indentation level  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2025  Shen, Jen-Chieh
;; Created date 2019-07-30 09:37:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-elpa/indent-control
;; Version: 0.3.4
;; Package-Requires: ((emacs "26.1"))
;; Keywords: convenience control indent tab generic level

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Interface that combine all the indentation variables from each major
;; mode to one giant list.
;;
;; You can set up the initial indentation level by changing the variable
;; `indent-control-records'.  This variable is a list on cons cell form
;; by (mode . level).  For example,
;;
;;   `(actionscript-mode . 4)`
;;
;; If you want to make the indentation level works consistently across
;; all buffer.  You would need to call function `indent-control-continue-with-record'
;; at the time you want the new buffer is loaded.
;;

;;; Code:

(defgroup indent-control nil
  "Visual Studio like line annotation in Emacs."
  :prefix "indent-control-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/indent-control"))

(defcustom indent-control-records
  '((actionscript-mode     . 4)
    (apache-mode           . 4)
    (awk-mode              . 4)
    (bpftrace-mode         . 4)
    (c-mode                . 4)
    (c++-mode              . 4)
    (cmake-mode            . 2)
    (coffee-mode           . 4)
    (coq-mode              . 2)
    (cperl-mode            . 2)
    (crystal-mode          . 2)
    (csharp-mode           . 4)
    (css-mode              . 2)
    (less-css-mode         . 2)
    (scss-mode             . 2)
    (dart-mode             . 2)
    (dockerfile-mode       . 2)
    (d-mode                . 4)
    (elixir-mode           . 2)
    (elm-mode              . 4)
    (emacs-lisp-mode       . 2)
    (enh-ruby-mode         . 2)
    (erlang-mode           . 2)
    (ess-mode              . 2)
    (f90-mode              . 2)
    (feature-mode          . 2)
    (fsharp-mode           . 4)
    (gdscript-mode         . 4)
    (go-mode               . 4)
    (groovy-mode           . 4)
    (haskell-mode          . 4)
    (haxe-mode             . 4)
    (haxor-mode            . 4)
    (idl-mode              . 4)
    (jade-mode             . 2)
    (java-mode             . 4)
    (javascript-mode       . 2)
    (jayces-mode           . 4)
    (jenkinsfile-mode      . 4)
    (js-mode               . 2)
    (js2-mode              . 2)
    (js2-jsx-mode          . 2)
    (js3-mode              . 2)
    (json-mode             . 2)
    (julia-mode            . 4)
    (kotlin-mode           . 4)
    (lisp-mode             . 2)
    (lisp-interaction-mode . 2)
    (livescript-mode       . 2)
    (lua-mode              . 4)
    (matlab-mode           . 4)
    (meson-mode            . 2)
    (mips-mode             . 4)
    (mustache-mode         . 2)
    (nasm-mode             . 4)
    (nginx-mode            . 4)
    (nix-mode              . 2)
    (nxml-mode             . 2)
    (objc-mode             . 4)
    (octave-mode           . 2)
    (perl-mode             . 2)
    (php-mode              . 4)
    (pike-mode             . 4)
    (ps-mode               . 4)
    (pug-mode              . 2)
    (puppet-mode           . 2)
    (python-mode           . 4)
    (rjsx-mode             . 2)
    (ruby-mode             . 2)
    (rust-mode             . 4)
    (rustic-mode           . 4)
    (scala-mode            . 2)
    (scss-mode             . 2)
    (sgml-mode             . 2)
    (sh-mode               . 2)
    (shader-mode           . 4)
    (slim-mode             . 2)
    (snippet-mode          . 1)
    (sml-mode              . 4)
    (sql-mode              . 1)
    (ssass-mode            . 2)
    (svelte-mode           . 2)
    (tcl-mode              . 4)
    (terra-mode            . 2)
    (typescript-mode       . 4)
    (verilog-mode          . 3)
    (web-mode              . 2)
    (yaml-mode             . 2)
    (zig-mode              . 4))
  "Indent level records for all `major-mode's."
  :type 'list
  :group 'indent-control)

(defcustom indent-control-alist
  '((actionscript-mode     . actionscript-indent-level)
    (apache-mode           . apache-indent-level)
    (awk-mode              . c-basic-offset)
    (bpftrace-mode         . c-basic-offset)
    (c-mode                . c-basic-offset)
    (c++-mode              . c-basic-offset)
    (cmake-mode            . cmake-tab-width)
    (coffee-mode           . coffee-tab-width)
    (coq-mode              . coq-indent-basic)
    (cperl-mode            . cperl-indent-level)
    (crystal-mode          . crystal-indent-level)
    (csharp-mode           . (c-basic-offset csharp-mode-indent-offset))
    (css-mode              . css-indent-offset)
    (less-css-mode         . css-indent-offset)
    (scss-mode             . css-indent-offset)
    (ssass-mode            . ssass-tab-width)
    (dockerfile-mode       . dockerfile-indent-offset)
    (d-mode                . c-basic-offset)
    (elixir-mode           . elixir-smie-indent-basic)
    (elm-mode              . elm-indent-offset)
    (emacs-lisp-mode       . lisp-body-indent)
    (enh-ruby-mode         . enh-ruby-indent-level)
    (erlang-mode           . erlang-indent-level)
    (ess-mode              . ess-indent-offset)
    (f90-mode              . (f90-associate-indent
                              f90-continuation-indent
                              f90-critical-indent
                              f90-do-indent
                              f90-if-indent
                              f90-program-indent
                              f90-type-indent))
    (feature-mode          . (feature-indent-offset
                              feature-indent-level))
    (fsharp-mode           . (fsharp-continuation-offset
                              fsharp-indent-level
                              fsharp-indent-offset))
    (gdscript-mode         . gdscript-indent-offset)
    (groovy-mode           . groovy-indent-offset)
    (jenkinsfile-mode      . groovy-indent-offset)
    (haskell-mode          . (haskell-indent-spaces
                              haskell-indent-offset
                              haskell-indentation-layout-offset
                              haskell-indentation-left-offset
                              haskell-indentation-starter-offset
                              haskell-indentation-where-post-offset
                              haskell-indentation-where-pre-offset
                              shm-indent-spaces))
    (haxe-mode             . c-basic-offset)
    (haxor-mode            . haxor-tab-width)
    (idl-mode              . c-basic-offset)
    (jade-mode             . jade-tab-width)
    (java-mode             . c-basic-offset)
    (javascript-mode       . js-indent-level)
    (jayces-mode           . c-basic-offset)
    (js-mode               . js-indent-level)
    (js2-mode              . js2-basic-offset)
    (js2-jsx-mode          . (js2-basic-offset sgml-basic-offset))
    (js3-mode              . js3-indent-level)
    (json-mode             . js-indent-level)
    (julia-mode            . julia-indent-offset)
    (kotlin-mode           . kotlin-tab-width)
    (lisp-mode             . lisp-body-indent)
    (lisp-interaction-mode . lisp-body-indent)
    (livescript-mode       . livescript-tab-width)
    (lua-mode              . lua-indent-level)
    (matlab-mode           . matlab-indent-level)
    (meson-mode            . meson-indent-basic)
    (mips-mode             . mips-tab-width)
    (mustache-mode         . mustache-basic-offset)
    (nasm-mode             . nasm-basic-offset)
    (nginx-mode            . nginx-indent-level)
    (nxml-mode             . (nxml-child-indent nxml-attribute-indent))
    (objc-mode             . c-basic-offset)
    (octave-mode           . octave-block-offset)
    (perl-mode             . perl-indent-level)
    (php-mode              . c-basic-offset)
    (pike-mode             . c-basic-offset)
    (ps-mode               . ps-mode-tab)
    (pug-mode              . pug-tab-width)
    (puppet-mode           . puppet-indent-level)
    (python-mode           . py-indent-offset)
    (rjsx-mode             . (js-indent-level sgml-basic-offset))
    (ruby-mode             . ruby-indent-level)
    (rust-mode             . rust-indent-offset)
    (rustic-mode           . rustic-indent-offset)
    (scala-mode            . scala-indent:step)
    (scss-mode             . css-indent-offset)
    (sgml-mode             . sgml-basic-offset)
    (sh-mode               . (sh-basic-offset sh-indentation))
    (shader-mode           . shader-indent-offset)
    (slim-mode             . slim-indent-offset)
    (sml-mode              . sml-indent-level)
    (sql-mode              . sql-indent-offset)
    (svelte-mode           . svelte-basic-offset)
    (tcl-mode              . (tcl-indent-level tcl-continued-indent-level))
    (terra-mode            . terra-indent-level)
    (typescript-mode       . typescript-indent-level)
    (verilog-mode          . (verilog-indent-level
                              verilog-indent-level-behavioral
                              verilog-indent-level-declaration
                              verilog-indent-level-module
                              verilog-cexp-indent
                              verilog-case-indent))
    (web-mode              . (web-mode-attr-indent-offset
                              web-mode-attr-value-indent-offset
                              web-mode-code-indent-offset
                              web-mode-css-indent-offset
                              web-mode-markup-indent-offset
                              web-mode-sql-indent-offset
                              web-mode-block-padding
                              web-mode-script-padding
                              web-mode-style-padding))
    (yaml-mode             . yaml-indent-offset)
    (zig-mode              . zig-indent-offset))
  "AList that maps `major-mode' to each major-mode's indent level variable name."
  :type 'list
  :group 'indent-control)

(defcustom indent-control-delta 2
  "Delta value for increment/decrement indentation level."
  :type 'integer
  :group 'indent-control)

(defcustom indent-control-min-indentation-level 0
  "Minimum indentation level can be set to."
  :type 'integer
  :group 'indent-control)

(defcustom indent-control-max-indentation-level 12
  "Maximum indentation level can be set to."
  :type 'integer
  :group 'indent-control)

(defcustom indent-control-prefer-indent-size 4
  "Prefer indent size."
  :type 'integer
  :group 'indent-control)

;;
;; (@* "Util" )
;;

(defmacro indent-control--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let (message-log-max)
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) ,@body))))

(defmacro indent-control--no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let (message-log-max) ,@body))

(defun indent-control--clamp-integer (val min max)
  "Make sure the VAL is between MIN and MAX."
  (max (min val max) min))

(defun indent-control--major-mode-p (name)
  "Return non-nil if NAME is current variable `major-mode'."
  (cond ((stringp name) (string= (symbol-name major-mode) name))
        ((listp name)
         (let ((index 0) (len (length name)) current-mode-name found)
           (while (and (not found) (< index len))
             (setq current-mode-name (nth index name)
                   found (indent-control--major-mode-p current-mode-name)
                   index (1+ index)))
           found))
        ((symbolp name) (equal major-mode name))))

;;
;; (@* "Core" )
;;

;;;###autoload
(defun indent-control-ensure-indentable ()
  "Ensure the current major mode are indentable."
  (when (null tab-width)
    (setq-local tab-width indent-control-prefer-indent-size))
  (when (null standard-indent)
    (setq-local standard-indent indent-control-prefer-indent-size)))

(define-obsolete-function-alias
  'indent-control-ensure-tab-width 'indent-control-ensure-indentable
  "0.3.5")

(defun indent-control--indent-level-name ()
  "Return symbol defined as indent level."
  (or (cdr (assoc major-mode indent-control-alist))
      (quote standard-indent)
      (quote tab-width)))

(defun indent-control--indent-level-record (&optional record-name)
  "Return record of current indent level by RECORD-NAME."
  (cdr (assoc (or record-name major-mode) indent-control-records)))

(defun indent-control--set-indent-level-record (new-level &optional record-name)
  "Set NEW-LEVEL to RECORD-NAME indent record."
  (setq record-name (or record-name major-mode))
  (if (assoc record-name indent-control-records)
      (setf (cdr (assoc record-name indent-control-records)) new-level)
    (indent-control-ensure-indentable)
    (user-error "[WARNING] Indentation level record not found: %s" record-name)))

(defun indent-control-set-indent-level-by-mode (new-level)
  "Set the NEW-LEVEL for current major mode."
  (let ((var-symbol (indent-control--indent-level-name)))
    (cond ((listp var-symbol)
           (dolist (indent-var var-symbol) (set indent-var new-level)))
          (t (set var-symbol new-level)))
    (setq-local standard-indent new-level))
  (when (and (integerp new-level)
             (indent-control--set-indent-level-record new-level))
    (indent-control--no-log-apply
      (message "[INFO] Current indent level: %s" new-level))))

(defun indent-control-get-indent-level-by-mode ()
  "Get indentation level by mode."
  (let ((var-symbol (indent-control--indent-level-name)))
    (when (listp var-symbol) (setq var-symbol (nth 0 var-symbol)))
    (unless (symbol-value var-symbol) (indent-control-ensure-indentable))
    (or (symbol-value var-symbol)
        standard-indent
        tab-width)))

(defun indent-control--delta-indent-level (delta-value)
  "Increase/Decrease tab width by DELTA-VALUE."
  (let ((indent-level (indent-control-get-indent-level-by-mode)))
    (indent-control-set-indent-level-by-mode
     (indent-control--clamp-integer  ; Ensure this is the valid value.
      (+ indent-level delta-value)
      indent-control-min-indentation-level indent-control-max-indentation-level))))

;;;###autoload
(defun indent-control-inc ()
  "Increase indent level by one level."
  (interactive)
  (indent-control--delta-indent-level indent-control-delta)
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-dec ()
  "Decrease indent level by one level."
  (interactive)
  (indent-control--delta-indent-level (- indent-control-delta))
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-continue-with-record ()
  "Keep the tab width the same as last time modified."
  (indent-control-set-indent-level-by-mode (indent-control--indent-level-record)))

;;
;; (@* "Setup" )
;;

(defun indent-control--prog-mode-hook ()
  "Programming language mode hook."
  (indent-control--mute-apply (indent-control-continue-with-record)))

;;;###autoload
(define-minor-mode indent-control-mode
  "Minor mode `indent-control-mode'."
  :global t
  :require 'indent-control
  :group 'indent-control
  (if indent-control-mode
      (progn
        (add-hook 'prog-mode-hook #'indent-control--prog-mode-hook)
        (indent-control--prog-mode-hook))  ; Activate it immediately
    (remove-hook 'prog-mode-hook #'indent-control--prog-mode-hook)))

;;
;; (@* "Obsolete" )
;;

(define-obsolete-function-alias
  'indent-control-inc-indent-level 'indent-control-inc "0.3.5")
(define-obsolete-function-alias
  'indent-control-dec-indent-level 'indent-control-dec "0.3.5")

(provide 'indent-control)
;;; indent-control.el ends here
