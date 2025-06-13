;;; keybinds.el --- Keybinds                         -*- lexical-binding: t; -*-

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

;; This file contains all the keybinds for the system.
;; I prefer this since it provides a nice central place to look and modify keybinds.
;; It is always loaded last so all possible bits are available.

;;; Code:

(require 'helpers)

(defun swiper-dwim ()
  "Call `swiper' if no region is active, otherwise call `swiper-thing-at-point'."
  (interactive)
  (call-interactively (if (region-active-p) #'swiper-thing-at-point #'swiper)))

(defun swiper-all-dwim ()
  "Call `swiper-all' if no region is active, otherwise call `swiper-all-thing-at-point'."
  (interactive)
  (call-interactively (if (region-active-p) #'swiper-all-thing-at-point #'swiper-all)))

(defun avy-dwim ()
  "Call `avy-goto-char-2' if not prefixed, otherwise call `avy-goto-char-in-line'."
  (interactive)
  (call-interactively (if current-prefix-arg #'avy-goto-char-in-line #'avy-goto-char-2)))

(defun counsel-fzf-dwim (&optional initial-input initial-directory prompt custom-fzf-cmd)
  "Wrapper for `counsel-fzf' that enables custom commands.

In addition, if we are in a project, the root will be set to the project root, unless specified."
  (interactive)
  (cond
   (custom-fzf-cmd
    (with-environment-variables
	(("FZF_DEFAULT_COMMAND" custom-fzf-cmd))
      (counsel-fzf initial-input initial-directory prompt)))
   ((and (not initial-directory) (project-current nil))
    (counsel-fzf "" (current-project-root) "Find Project File: "))
   (current-prefix-arg
    (call-interactively #'counsel-fzf))
   (t
    (counsel-fzf initial-input initial-directory prompt))))

(global-set-key (kbd "C-s") 'swiper-dwim)
(global-set-key (kbd "C-r") 'swiper-all-dwim)
(global-set-key (kbd "M-s M-r") 'counsel-recentf)
(global-set-key (kbd "C-'") 'avy-dwim)
(global-set-key (kbd "M-'") 'avy-goto-line)
(global-set-key (kbd "C-c j") 'counsel-org-capture)
(global-set-key (kbd "C-c f") 'counsel-fzf-dwim)
(global-set-key (kbd "C-M-g") 'org-clock-goto)
(global-set-key (kbd "C-c a") 'org-agenda-list)
(global-set-key (kbd "C-c n d") 'my/denote)
(global-set-key (kbd "C-c n j") 'denote-silo-dired)
(global-set-key (kbd "C-c n r") 'citar-create-note)
(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C-c n m") 'create-or-open-journal-entry)
(global-set-key (kbd "C-x g") 'magit)
(setopt persp-mode-prefix-key (kbd "C-x x"))
(define-key persp-mode-map (kbd "C-x x C-s") 'swiper-persp)
(global-set-key (kbd "C-x b") 'persp-counsel-switch-buffer)
(global-set-key (kbd "C-x k") 'persp-kill-buffer*)
(global-set-key (kbd "C-c n f")
		(ilambda
		 (counsel-fzf-dwim
		  ""
		  (if current-prefix-arg (ivy-completing-read "Silo: " denote-silo-directories) denote-directory)
		  "Find Note: "
		  "fd --type f -e org")))
(global-set-key (kbd "M-$") 'jinx-correct)
(global-unset-key (kbd "M-v"))
(global-set-key (kbd "M-v p") 'ivy-push-view)
(global-set-key (kbd "M-v P") 'ivy-pop-view)
(global-set-key (kbd "M-v M-v") 'ivy-switch-view)
(global-set-key (kbd "C-c s") 'vterm)
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "M-SPC") 'er/expand-region)
(global-set-key (kbd "M-j") 'mc/mark-more-like-this-extended)
(global-set-key (kbd "C-c J") (ilambda (dired (ivy-completing-read "Directory: " '("~/Code/" "~/.config/emacs/")))))
(global-set-key (kbd "C-x p C-s") 'swiper-project)

(define-key swiper-map (kbd "C-r") 'swiper-C-r)

(define-key mc/mark-more-like-this-extended-keymap (kbd "n") 'mc/mmlte--down)
(define-key mc/mark-more-like-this-extended-keymap (kbd "p") 'mc/mmlte--up)

(keybinds-for org
  ("C-c C-x C-d" . denote-link)
  ("C-c C-x C-b" . denote-backlinks)
  ("C-c C-x d" . denote-create-empty-file-and-link)
  ("C-'" . avy-dwim)
  ("M-g i" . counsel-org-goto)
  ("C-c C-x C-a" . org-cycle-agenda-files))

(keybinds-for dired
  ("r" . denote-rename-file))

(keybinds-for reftex
  ("C-c ]" . citar-insert-citation))

(keybinds-for pdf-view
  ("M-n" . pdf-view-next-page-command)
  ("M-p" . pdf-view-previous-page-command)
  ("n" . pdf-view-next-line-or-next-page)
  ("p" . pdf-view-previous-line-or-previous-page))

(keybinds-for tuareg
  ("C-c C-a" . tuareg-find-alternate-file))

(define-key tempel-map (kbd "TAB") 'tempel-next)
(define-key tempel-map (kbd "M-TAB") 'tempel-previous)
(provide 'keybinds)
;;; keybinds.el ends here
