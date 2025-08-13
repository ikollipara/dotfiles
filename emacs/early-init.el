;;; early-init.el --- Emacs Early Initialization     -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Ian Kollipara

;; Author: Ian Kollipara <ian@Fedora>
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

;; This file contains all the code needed to initialize emacs early.
;; It's mostly empty, besides the stopping of packages since I use `straight.el'.

;;; Code:

(setq package-enable-at-startup nil)

(provide 'early-init)
;;; early-init.el ends here
