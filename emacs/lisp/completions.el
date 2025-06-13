;;; completions.el --- Completion Setup              -*- lexical-binding: t; -*-

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
;; The completion framework I use is Ivy + Counsel + Swiper.
;; I find abo-abo's tools to fit the way I prefer to work with
;; Emacs. In particular, the tight integration of avy means
;; that I can easily select things in a few key presses.

;;; Code:

(require 'helpers)

;;; Packages to Install
(straight-use-package 'ivy)
(straight-use-package 'corfu)		; I use Corfu for my capf framework.
(straight-use-package 'cape)		; Cape enhances Corfu
(straight-use-package 'orderless)	; I really like the orderless completion style

(config-for ivy
  (enable-mode ivy-mode)
  (add-to-list 'load-path (emacs-dir "straight/repos/swiper"))
  (with-eval-after-load 'avy
    (require 'ivy-avy))
  (setopt ivy-use-virtual-buffers t
	  ivy-count-format "(%d/%d) "))

(config-for corfu
  (setopt corfu-auto t
	  corfu-auto-prefix 2)
  (keymap-set corfu-map "C-'" #'corfu-quick-complete)
  (enable-mode corfu-popupinfo-mode)
  (enable-mode corfu-history-mode))


(add-hook 'prog-mode-hook #'corfu-mode)

(config-for cape
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-elisp-symbol))

(config-for orderless
  (setopt completion-styles '(orderless basic)
	  completion-category-overrides '((file (styles basic partial-completion))))
  
  ;; This configures Ivy to use Orderless.
  ;; Check the Orderless docs for setting this up.
  (with-eval-after-load 'ivy
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
    (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))))

(require-all ivy corfu cape orderless)

(provide 'completions)
;;; completions.el ends here
