;;; indent-control.el --- Management for indentation level  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-07-30 09:37:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Management for indentation level.
;; Keyword: control indent tab generic level
;; Version: 0.2.0
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
;; Management for indentation level.
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
  (cond
   ((indent-control--major-mode-p '("actionscript-mode")) (quote actionscript-indent-level))
   ((indent-control--major-mode-p '("cc-mode"
                                    "c-mode"
                                    "c++-mode"
                                    "csharp-mode"
                                    "java-mode"
                                    "jayces-mode"
                                    "objc-mode")) (quote c-basic-offset))
   ((indent-control--major-mode-p '("css-mode"
                                    "less-css-mode"
                                    "scss-mode")) (quote css-indent-offset))
   ((indent-control--major-mode-p '("ssass-mode")) (quote ssass-tab-width))
   ((indent-control--major-mode-p '("groovy-mode")) (quote groovy-indent-offset))
   ((indent-control--major-mode-p '("js-mode")) (quote js-indent-level))
   ((indent-control--major-mode-p '("js2-mode")) (quote js2-basic-offset))
   ((indent-control--major-mode-p '("lisp-mode"
                                    "lisp-interaction-mode"
                                    "emacs-lisp-mode")) (quote lisp-body-indent))
   ((indent-control--major-mode-p '("lua-mode")) (quote lua-indent-level))
   ((indent-control--major-mode-p '("nasm-mode")) (quote nasm-basic-offset))
   ((indent-control--major-mode-p '("nxml-mode")) (quote nxml-child-indent))
   ((indent-control--major-mode-p '("python-mode")) (quote py-indent-offset))
   ((indent-control--major-mode-p '("rjsx-mode")) (quote js-indent-level))
   ((indent-control--major-mode-p '("ruby-mode")) (quote ruby-indent-level))
   ((indent-control--major-mode-p '("rust-mode")) (quote rust-indent-offset))
   ((indent-control--major-mode-p '("shader-mode")) (quote shader-indent-offset))
   ((indent-control--major-mode-p '("sql-mode")) (quote sql-indent-offset))
   ((indent-control--major-mode-p '("typescript-mode")) (quote typescript-indent-level))
   ((indent-control--major-mode-p '("web-mode"))
    (quote (web-mode-markup-indent-offset
            web-mode-css-indent-offset
            web-mode-code-indent-offset)))
   ((indent-control--major-mode-p '("yaml-mode")) (quote yaml-indent-offset))
   (t (quote tab-width))))

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
      (message "[WARNING] Indentation level record not found: %s" mn))))

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

(indent-control-continue-with-tab-width-record)

(provide 'indent-control)
;;; indent-control.el ends here
