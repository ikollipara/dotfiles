;;; navigation.el --- Navigation Helpers             -*- lexical-binding: t; -*-

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
;; As stated in `completions.el' I use Ivy + Counsel + Swiper.
;; The tight integration these tools have with `avy' is what I love.
;; In addition, there's a lot of goodies with swiper that I extend.
;; I try my best to "search" my way through a document, not by C-n and C-p.

;;; Code:

(require 'helpers)

(straight-use-package 'counsel)		; Counsel provides a ton of navigational commands
(straight-use-package 'swiper)		; The best searching tool ever
(straight-use-package 'amx)		; Provides nice M-x help
(straight-use-package 'avy)		; The best navigation tool ever
(straight-use-package 'ivy-rich)	; Makes ivy *pretty*
(straight-use-package 'ace-window)	; Quick window switching

;;; Counsel
(enable-mode counsel-mode)
;; By default counsel will setup "^" as the prefix in searches
;; This breaks orderless, so I disable these here.
(ivy-configure 'counsel-M-x :initial-input "")
(ivy-configure 'counsel-org-capture :initial-input "")
(ivy-configure 'counsel-describe-symbol :initial-input "")
(ivy-configure 'counsel-minor :initial-input "")
(ivy-configure 'counsel-package :initial-input "")

;; This custom FZF default makes sure that `counsel-fzf' respects .gitignore
(setenv "FZF_DEFAULT_COMMAND" "fd --type f --strip-cwd-prefix --hidden --follow --exclude .git")
(with-suppressed-warnings 'emacs
  (setopt counsel-describe-symbol-function #'helpful-symbol
	  counsel-describe-variable-function #'helpful-function))


(enable-mode ivy-rich-mode)


(config-for swiper

  ;; I use multiple cursors when coding, so I enable its swiper integration here.
  (with-eval-after-load 'multiple-cursors-core
    (add-to-list 'mc/cmds-to-run-once 'swiper-mc))

  ;; There's `swiper-C-s' which does what isearch's C-s does, but
  ;; there's no `swiper-C-r'. I added that here.
  (defun swiper-C-r (&optional arg)
    (interactive "p")
    (if (string= ivy-text "")
	(ivy-previous-history-element 1)
      (ivy-previous-line arg)))

  (defun swiper-project ()
    "Swiper support for just project buffers. Wraps `swiper-multi'."
    (interactive)
    (setq swiper-multi-buffers (seq-filter (lambda (b) (if (buffer-file-name b) t nil)) (project-buffers (project-current t))))
    (setq swiper-multi-candidates (swiper--multi-candidates (mapcar #'get-buffer swiper-multi-buffers)))
    (let ((swiper-window-width (- (- (frame-width) (if (display-graphic-p) 0 1)) 4)))
      (ivy-read "Swiper: " swiper-multi-candidates
		:action #'swiper-multi-action-2
		:caller 'swiper-multi)))

  (with-eval-after-load 'perspective
    (defun swiper-persp ()
      "Swiper support for persp buffers. Wraps `swiper-multi'."
      (interactive)
      (setq swiper-multi-buffers (persp-buffers (persp-curr nil)))
      (setq swiper-multi-candidates (swiper--multi-candidates (mapcar #'get-buffer swiper-multi-buffers)))
      (let ((swiper-window-width (- (- (frame-width) (if (display-graphic-p) 0 1)) 4)))
	(ivy-read "Swiper: " swiper-multi-candidates
		  :action #'swiper-multi-action-2
		  :caller 'swiper-multi)))))



(config-for avy

  ;; Avy doesn't recenter when navigating, so I wrap its invoking command here.
  (advice-add #'avy-goto-char-2 :after (lambda (&rest _) (recenter)))
  (advice-add #'avy-goto-line :after (lambda (&rest _) (recenter)))
  
  (with-eval-after-load 'multiple-cursors
    (defun avy-action-mark-cursor (pt)
      "Add an additional cursor at the point using avy."
      (save-excursion
	(goto-char pt)
	(mc/mark-pop))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    (setf (alist-get ?c avy-dispatch-alist) 'avy-action-mark-cursor))
  (with-eval-after-load 'jinx
    (defun avy-action-jinx (pt)
      "Correct the word at point using jinx."
      (save-excursion
	(goto-char pt)
	(jinx-correct))
      (select-window (cdr (ring-ref avy-ring 0))))
    (setf (alist-get ?i avy-dispatch-alist) 'avy-action-jinx))
  (defun avy-mark-to-char (pt)
    "Mark all characters between original point and new point."
    (activate-mark)
    (goto-char pt))
  (setf (alist-get ?  avy-dispatch-alist) 'avy-mark-to-char))

(config-for ace-window
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-dispatch-alist
	'((?x aw-delete-window "Delete Window")
	  (?m aw-swap-window "Swap Windows")
	  (?M aw-move-window "Move Window")
	  (?c aw-copy-window "Copy Window")
	  (?j aw-switch-buffer-in-window "Select Buffer")
	  (?n aw-flip-window)
	  (?u aw-switch-buffer-other-window "Switch Buffer Other Window")
	  (?c aw-split-window-fair "Split Fair Window")
	  (?v aw-split-window-vert "Split Vert Window")
	  (?b aw-split-window-horz "Split Horz Window")
	  (?o delete-other-windows "Delete Other Windows")
	  (?? aw-show-dispatch-help))))

(require-all amx counsel swiper avy ace-window)

(provide 'navigation)
;;; navigation.el ends here
