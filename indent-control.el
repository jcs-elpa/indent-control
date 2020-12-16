;;; indent-control.el --- Management for indentation level  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-07-30 09:37:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Management for indentation level.
;; Keyword: control indent tab generic level
;; Version: 0.2.4
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/indent-control

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
;; If you want to make the indentation level works consistently across all buffer.
;; You would need to call function `indent-control-continue-with-tab-width-record'
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
    (c-mode                . 4)
    (c++-mode              . 4)
    (csharp-mode           . 4)
    (css-mode              . 2)
    (dockerfile-mode       . 2)
    (elisp-mode            . 2)
    (emacs-lisp-mode       . 2)
    (go-mode               . 4)
    (groovy-mode           . 4)
    (java-mode             . 4)
    (jayces-mode           . 4)
    (js-mode               . 2)
    (js2-mode              . 2)
    (json-mode             . 2)
    (kotlin-mode           . 4)
    (less-css-mode         . 2)
    (lisp-mode             . 2)
    (lisp-interaction-mode . 2)
    (lua-mode              . 4)
    (nasm-mode             . 4)
    (nix-mode              . 2)
    (nxml-mode             . 2)
    (objc-mode             . 4)
    (python-mode           . 4)
    (rjsx-mode             . 2)
    (ruby-mode             . 4)
    (rust-mode             . 4)
    (scss-mode             . 2)
    (shader-mode           . 4)
    (ssass-mode            . 2)
    (sql-mode              . 1)
    (typescript-mode       . 4)
    (web-mode              . 2)
    (yaml-mode             . 2))
  "Indent level recrods for all `major-mode's."
  :type 'list
  :group 'indent-control)

(defcustom indent-control-alist
  '((actionscript-mode     . actionscript-indent-level)
    (c-mode                . c-basic-offset)
    (c++-mode              . c-basic-offset)
    (csharp-mode           . c-basic-offset)
    (java-mode             . c-basic-offset)
    (jayces-mode           . c-basic-offset)
    (objc-mode             . c-basic-offset)
    (css-mode              . css-indent-offset)
    (less-css-mode         . css-indent-offset)
    (scss-mode             . css-indent-offset)
    (ssass-mode            . ssass-tab-width)
    (groovy-mode           . groovy-indent-offset)
    (js-mode               . js-indent-level)
    (js2-mode              . js2-basic-offset)
    (lisp-mode             . lisp-body-indent)
    (lisp-interaction-mode . lisp-body-indent)
    (emacs-lisp-mode       . lisp-body-indent)
    (lua-mode              . lua-indent-level)
    (nasm-mode             . nasm-basic-offset)
    (nxml-mode             . nxml-child-indent)
    (python-mode           . py-indent-offset)
    (rjsx-mode             . js-indent-level)
    (ruby-mode             . ruby-indent-level)
    (rust-mode             . rust-indent-offset)
    (shader-mode           . shader-indent-offset)
    (sql-mode              . sql-indent-offset)
    (typescript-mode       . typescript-indent-level)
    (web-mode              . (web-mode-markup-indent-offset
                              web-mode-css-indent-offset
                              web-mode-code-indent-offset))
    (yaml-mode             . yaml-indent-offset))
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

(defcustom indent-control-max-indentation-level 8
  "Maximum indentation level can be set to."
  :type 'integer
  :group 'indent-control)

;;
;; (@* "Util" )
;;

(defmacro indent-control--mute-apply (&rest body)
  "Execute BODY without message."
  (declare (indent 0) (debug t))
  `(let ((message-log-max nil))
     (with-temp-message (or (current-message) nil)
       (let ((inhibit-message t)) (progn ,@body)))))

(defmacro indent-control--no-log-apply (&rest body)
  "Execute BODY without write it to message buffer."
  (declare (indent 0) (debug t))
  `(let ((message-log-max nil)) (progn ,@body)))

(defun indent-control--clamp-integer (in-val in-min in-max)
  "Make sure the IN-VAL is between IN-MIN and IN-MAX."
  (cond ((<= in-val in-min) (setq in-val in-min))
        ((>= in-val in-max) (setq in-val in-max)))
  in-val)

(defun indent-control--major-mode-p (mns)
  "Check if this major mode MNS."
  (cond ((stringp mns) (string= (symbol-name major-mode) mns))
        ((listp mns)
         (let ((index 0) (len (length mns)) current-mode-name found)
           (while (and (not found) (< index len))
             (setq current-mode-name (nth index mns)
                   found (indent-control--major-mode-p current-mode-name)
                   index (1+ index)))
           found))
        ((symbolp mns) (equal major-mode mns))
        (t nil)))

;;
;; (@* "Core" )
;;

(defun indent-control--indent-level-by-mode ()
  "Return indentation level variable as symbol depends on current major mode."
  (or (cdr (assoc major-mode indent-control-alist))
      (quote tab-width)))

(defun indent-control-set-indent-level-by-mode (tw)
  "Set the tab width (TW) for current major mode."
  (let ((var-symbol (indent-control--indent-level-by-mode)))
    (cond ((listp var-symbol) (dolist (indent-var var-symbol) (set indent-var tw)))
          (t (set var-symbol tw))))
  (when (integerp tw)
    (indent-control--set-indent-level-record-by-mode tw)
    (indent-control--no-log-apply
      (message "[INFO] Current indent level: %s" tw))))

(defun indent-control-get-indent-level-by-mode ()
  "Get indentation level by mode."
  (let ((var-symbol (indent-control--indent-level-by-mode)))
    (when (listp var-symbol) (setq var-symbol (nth 0 var-symbol)))
    (symbol-value var-symbol)))

(defun indent-control--delta-ensure-valid-tab-width (cv dv)
  "Change tab width by current value CV and delta value (DV)."
  (indent-control--clamp-integer
   (+ cv dv) indent-control-min-indentation-level indent-control-max-indentation-level))

(defun indent-control--delta-tab-width (dv)
  "Increase/Decrease tab width by delta value (DV)."
  (let ((indent-level (indent-control-get-indent-level-by-mode)))
    (indent-control-set-indent-level-by-mode
     (indent-control--delta-ensure-valid-tab-width indent-level dv))))

(defun indent-control--set-indent-level-record-by-mode (tw &optional mn)
  "Set the tab width record by mode name MN with tab width TW."
  (unless mn (setq mn major-mode))
  (let ((index 0) (len (length indent-control-records)) break-it)
    (while (and (not break-it) (< index len))
      (let* ((record (nth index indent-control-records))
             (record-mode-name (car record)))
        (when (equal mn record-mode-name)
          (setf (cdr (nth index indent-control-records)) tw)
          (setq break-it t)))
      (setq index (1+ index)))
    (unless break-it
      (indent-control--no-log-apply
        (message "[WARNING] Indentation level record not found: %s" mn)))))

(defun indent-control--get-indent-level-record-by-mode (&optional mn)
  "Get the tab width record by mode name, MN."
  (unless mn (setq mn major-mode))
  (let ((index 0) (len (length indent-control-records)) break-it
        ;; Have default to `tab-width'.
        (target-tab-width tab-width))
    (while (and (not break-it) (< index len))
      (let* ((record (nth index indent-control-records))
             (record-mode-name (car record)) (record-tab-width (cdr record)))
        (when (equal mn record-mode-name)
          (setq target-tab-width record-tab-width
                break-it t)))
      (setq index (1+ index)))
    target-tab-width))

(defun indent-control--prog-mode-hook ()
  "Programming language mode hook."
  (indent-control--mute-apply
    (indent-control-continue-with-tab-width-record)))
(add-hook 'prog-mode-hook #'indent-control--prog-mode-hook)

;;;###autoload
(defun indent-control-inc-indent-level ()
  "Increase indent level by one level, default is 2."
  (interactive)
  (indent-control--delta-tab-width indent-control-delta)
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-dec-indent-level ()
  "Decrease indent level by one level, default is 2."
  (interactive)
  (indent-control--delta-tab-width (- 0 indent-control-delta))
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-continue-with-tab-width-record ()
  "Keep the tab width the same as last time modified."
  (indent-control-set-indent-level-by-mode (indent-control--get-indent-level-record-by-mode)))

;; NOTE: Initial the indent level once after module is loaded.
;;;###autoload
(indent-control-continue-with-tab-width-record)

(provide 'indent-control)
;;; indent-control.el ends here
