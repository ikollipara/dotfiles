;;; init.el --- My Emacs Configuration               -*- lexical-binding: t; -*-

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

;; This is my personal Emacs Configuration.
;; It's styled the way I like, with keybinds that I like.

;;; Code:


;;; Straight Initialization
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; User Settings
(setq user-full-name "Ian Kollipara"
      user-mail-address "ian.kollipara@gmail.com"
      user-emacs-directory (expand-file-name "~/.config/emacs/"))

;;; Helper Macro
(defmacro emacs-dir (path)
  `(concat user-emacs-directory ,path))

(add-to-list 'load-path (emacs-dir "lisp"))

;;; This is used later for Eglot
(setq straight-built-in-pseudo-packages '(emacs nadvice python image-mode project flymake xref))


(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; No Littering
;; This configuration does not make use of `use-package'.
;; I've used it in the past, but I found that I prefer manually configuring
;; everything.

(straight-use-package 'no-littering)
(require 'no-littering)
(setq version-control t
      kept-old-versions 6
      kept-new-versions 2
      delete-old-versions t
      backup-by-copying t)
(setq backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
(setq auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(require 'opam-user-setup)
(require 'completions)
(require 'navigation)
(require 'ui)
(require 'writing)
(require 'prog)
(require 'keybinds)

(put 'narrow-to-region 'disabled nil)
