;;; helpers.el --- My custom helper macros           -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ian Kollipara

;; Author: Ian Kollipara <ian.kollipara@gmail.com>
;; Keywords: 

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
;; This is a collection of helper macros I use throughout my configuration.
;; Some are simple wrappers that just provide a nicer name for its purpose.
;; I'm a big fan of DSLs.

;;; Code:

(defmacro disable-mode-hook (mode)
  "Create a lambda that disables the given `MODE'."
  `(lambda () (,mode -1)))

(defmacro disable-mode (mode)
  "Disable the given `MODE'."
  `(,mode -1))

(defmacro enable-mode (mode)
  "Enable the given `MODE'."
  `(,mode 1))

(defmacro enable-mode-hook (mode)
  "Create a lambda that enables the given `MODE'."
  `(lambda () (,mode 1)))

(defmacro config-for (mod &rest body)
  "Declare a configuration for `MOD'."
  (declare (indent defun))
  `(with-eval-after-load ',mod
     ,@body))

(defmacro require-all (&rest mods)
  "Require all the given `MODS'."
  `(progn
     ,@(mapcar (lambda (mod) `(require ',mod)) mods)))

(defmacro keybinds-for (mode &rest mapping-alist)
  "Create a series of keybinds for the given `MODE'."
  (declare (indent defun))
  `(progn
     ,@(mapcar (lambda (pair)
		 `(define-key ,(intern (concat (symbol-name mode) "-mode-map")) (kbd ,(car pair)) ',(cdr pair)))
	       mapping-alist)))

(defmacro ilambda (&rest body)
  "Create an interactive lambda."
  `(lambda () (interactive) ,@body))

(defmacro current-project-root (&optional prompt)
  "Get the current project's root. `PROMPT' is passed to `project-current'."
  `(project-root (project-current ,prompt)))

(provide 'helpers)
;;; helpers.el ends here
