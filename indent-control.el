;;; indent-control.el --- Generic control the indentation level for each mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Shen, Jen-Chieh
;; Created date 2019-07-30 09:37:44

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Generic control the indentation level for each mode.
;; Keyword: control indent tab generic level
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.3"))
;; URL: https://github.com/jcs090218/indent-control

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
;; Generic control the indentation level for each mode.
;;

;;; Code:

(defgroup indent-control nil
  "Visual Studio like line annotation in Emacs."
  :prefix "indent-control-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs090218/indent-control"))

(defcustom indent-control-records
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
    (yaml-mode             . 2))
  "Tab with recrods for all `major-mode's."
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

(defun indent-control--is-current-major-mode-p (mns)
  "Check if this major modes MNS."
  (cond ((stringp mns)
         (string= (symbol-name major-mode) mns))
        ((listp mns)
         (let ((index 0)
               (current-mode-name nil)
               (found nil))
           (while (and (< index (length mns))
                       (not found))
             (setq current-mode-name (nth index mns))
             (setq found (indent-control--is-current-major-mode-p current-mode-name))
             (setq index (1+ index)))
           found))
        ((symbolp mns)
         (equal major-mode mns))
        (t nil)))

(defun indent-control--set-tab-width-by-mode (tw)
  "Set the tab width TW for current major mode."
  (cond
   ((indent-control--is-current-major-mode-p '("actionscript-mode"))
    (setq actionscript-indent-level tw))
   ((indent-control--is-current-major-mode-p '("cc-mode"
                                               "c-mode"
                                               "c++-mode"
                                               "csharp-mode"
                                               "java-mode"
                                               "jayces-mode"
                                               "objc-mode"))
    (setq c-basic-offset tw))
   ((indent-control--is-current-major-mode-p '("css-mode"
                                               "scss-mode"))
    (setq css-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("js-mode"
                                               "json-mode"))
    (setq js-indent-level tw))
   ((indent-control--is-current-major-mode-p '("js2-mode"))
    (setq js2-basic-offset tw))
   ((indent-control--is-current-major-mode-p '("lisp-mode"
                                               "lisp-interaction-mode"
                                               "emacs-lisp-mode"))
    (setq lisp-body-indent tw))
   ((indent-control--is-current-major-mode-p '("lua-mode"))
    (setq lua-indent-level tw))
   ((indent-control--is-current-major-mode-p '("lua-mode"))
    (setq lua-indent-level tw))
   ((indent-control--is-current-major-mode-p '("nasm-mode"))
    (setq nasm-basic-offset tw))
   ((indent-control--is-current-major-mode-p '("nxml-mode"))
    (setq nxml-child-indent tw))
   ((indent-control--is-current-major-mode-p '("python-mode"))
    (setq py-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("ruby-mode"))
    (setq ruby-indent-level tw))
   ((indent-control--is-current-major-mode-p '("rust-mode"))
    (setq rust-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("shader-mode"))
    (setq shader-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("sql-mode"))
    (setq sql-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("typescript-mode"))
    (setq typescript-indent-level tw))
   ((indent-control--is-current-major-mode-p '("web-mode"))
    (setq web-mode-markup-indent-offset tw))
   ((indent-control--is-current-major-mode-p '("yaml-mode"))
    (setq yaml-indent-offset tw))
   (t
    (setq tab-width tw)))
  (if tw
      (progn
        (indent-control--set-tab-width-record-by-mode tw)
        (message "Current indent level: %s" tw))
    (message "No indent offset defined in major mode: %s" major-mode)))

(defun indent-control--get-tab-width-by-mode ()
  "Get indentation level by mode."
  (cond
   ((indent-control--is-current-major-mode-p '("actionscript-mode"))
    actionscript-indent-level)
   ((indent-control--is-current-major-mode-p '("cc-mode"
                                               "c-mode"
                                               "c++-mode"
                                               "csharp-mode"
                                               "java-mode"
                                               "jayces-mode"
                                               "objc-mode"))
    c-basic-offset)
   ((indent-control--is-current-major-mode-p '("css-mode"))
    css-indent-offset)
   ((indent-control--is-current-major-mode-p '("js-mode"
                                               "json-mode"))
    js-indent-level)
   ((indent-control--is-current-major-mode-p '("js2-mode"))
    js2-basic-offset)
   ((indent-control--is-current-major-mode-p '("lisp-mode"
                                               "lisp-interaction-mode"
                                               "emacs-lisp-mode"))
    lisp-body-indent)
   ((indent-control--is-current-major-mode-p '("lua-mode"))
    lua-indent-level)
   ((indent-control--is-current-major-mode-p '("nasm-mode"))
    nasm-basic-offset)
   ((indent-control--is-current-major-mode-p '("nxml-mode"))
    nxml-child-indent)
   ((indent-control--is-current-major-mode-p '("python-mode"))
    py-indent-offset)
   ((indent-control--is-current-major-mode-p '("ruby-mode"))
    ruby-indent-level)
   ((indent-control--is-current-major-mode-p '("rust-mode"))
    rust-indent-offset)
   ((indent-control--is-current-major-mode-p '("shader-mode"))
    shader-indent-offset)
   ((indent-control--is-current-major-mode-p '("sql-mode"))
    sql-indent-offset)
   ((indent-control--is-current-major-mode-p '("typescript-mode"))
    typescript-indent-level)
   ((indent-control--is-current-major-mode-p '("web-mode"))
    web-mode-markup-indent-offset)
   ((indent-control--is-current-major-mode-p '("yaml-mode"))
    yaml-indent-offset)
   (t tab-width)))

(defun indent-control--set-tab-width-record-by-mode (tw &optional mn)
  "Set the tab width record by MN with tab width TW."
  (unless mn (setq mn major-mode))
  (let ((index 0)
        (break-it nil))
    (while (and (< index (length indent-control-records))
                (not break-it))
      (let ((record-mode-name (car (nth index indent-control-records))))
        (when (equal mn record-mode-name)
          (setf (cdr (nth index indent-control-records)) tw)
          (setq break-it t)))
      (setq index (1+ index)))
    (unless break-it
      (message "Tab width record not found: %s" mn))))

(defun indent-control--get-tab-width-record-by-mode (&optional mn)
  "Get the tab width record by MN."
  (unless mn (setq mn major-mode))
  (let ((index 0)
        (break-it nil)
        (target-tab-width nil))
    (while (and (< index (length indent-control-records))
                (not break-it))
      (let ((record-mode-name (car (nth index indent-control-records)))
            (record-tab-width (cdr (nth index indent-control-records))))
        (when (equal mn record-mode-name)
          (setq target-tab-width record-tab-width)
          (setq break-it t)))
      (setq index (1+ index)))
    target-tab-width))


(defun indent-control--clamp-integer (in-val in-min in-max)
  "Make sure the IN-VAL is between IN-MIN and IN-MAX."
  (let ((out-result in-val))
    (cond ((<= in-val in-min) (progn (setq out-result in-min)))
          ((>= in-val in-max) (progn (setq out-result in-max))))
    out-result))

(defun indent-control--ensure-valid-tab-width (cv dv)
  "Change tab width by current value CV and delta value DV."
  (indent-control--clamp-integer (+ cv dv)
                                 indent-control-min-indentation-level
                                 indent-control-max-indentation-level))

(defun indent-control--delta-tab-width (dv)
  "Increase/Decrease tab width by delta value DV."
  (indent-control--set-tab-width-by-mode (indent-control--ensure-valid-tab-width (indent-control--get-tab-width-by-mode) dv)))

(defun indent-control--prog-mode-hook ()
  "Programming language mode hook."
  (let ((inhibit-message t)
        (message-log-max nil))
    (indent-control-continue-with-tab-width-record)))
(add-hook 'prog-mode-hook #'indent-control--prog-mode-hook)

;;;###autoload
(defun indent-control-inc-indent-level ()
  "Increase tab width by 2."
  (interactive)
  (indent-control--delta-tab-width indent-control-delta)
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-dec-indent-level ()
  "Decrease tab width by 2."
  (interactive)
  (indent-control--delta-tab-width (- 0 indent-control-delta))
  (indent-for-tab-command))

;;;###autoload
(defun indent-control-continue-with-tab-width-record ()
  "Keep the tab width the same as last time modified."
  (indent-control--set-tab-width-by-mode (indent-control--get-tab-width-record-by-mode)))

(provide 'indent-control)
;;; indent-control.el ends here
