;;; productivity.el --- Productivity Tooling         -*- lexical-binding: t; -*-

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

;;; Code:

(require 'org)

(defun pomadoro--reset-org-timer-format ()
  (setq org-timer-format "%s "))

(defun pomadoro-start (title)
  "Start a pomadoro timer with TITLE."
  (interactive "sTitle: ")
  (setq org-timer-format (concat "[" title "] %s "))
  (org-timer-set-timer 1)
  (run-at-time "1 min" nil #'pomadoro--reset-org-timer-format))


(provide 'productivity)
;;; productivity.el ends here
