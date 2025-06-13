
;;; ui.el --- UI configuration                       -*- lexical-binding: t; -*-

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
;; My UI is arguably the most custom of the setup. I pull from `lambda-themes' and `nano'
;; to create a pleasing and aesthetic Emacs.
;; In addition, I use nerd icons across the layout to make commands and other things feel richer.
;; Lastly, I use `perspective.el' to create workspaces, since I like to segment my many things in Emacs.

;;; Code:

(straight-use-package 'olivetti)
(straight-use-package 'nerd-icons)
(straight-use-package 'nerd-icons-dired)
(straight-use-package 'nerd-icons-ibuffer)
(straight-use-package 'nerd-icons-corfu)
(straight-use-package 'nerd-icons-ivy-rich)
(straight-use-package 'helpful)
(straight-use-package '(lambda-themes :type git :host github :repo "lambda-emacs/lambda-themes"))
(straight-use-package '(nano :type git :host github :repo "rougier/nano-emacs"))
(straight-use-package 'perspective)

(require 'nerd-icons)

(add-hook 'prog-mode-hook (enable-mode-hook display-line-numbers-mode))
(setopt lambda-themes-set-italic-comments t
	lambda-themes-set-italic-keywords t
	lambda-themes-set-vibrant t)
(load-theme 'lambda-dark t)

(modify-frame-parameters
 nil
 '((internal-border-width . 24)
   (left-fringe . 0)
   (right-fringe . 0)))

;; Needed for Lambda Themes
;; It expects an older version of nano-modeline, which used this setting.
;; I have it at the top, so I just put it here for compatibilty.
(setq nano-modeline-position 'top)

(setq window-divider-default-places 'right-only)
(enable-mode window-divider-mode)

(config-for perspective
  (setopt persp-interactive-completion-function 'ivy-completing-read
	  persp-modestring-short t
	  persp-suppress-no-prefix-key-warning t)

  ;;; Perspective-Project.el integration
  ;; There's no nice integrations, so I wrote my own.
  ;; It's just a simple advice helper that wraps all project commands that I use
  ;; to also create a perspective.
  (defun ad--persp-project-find (orig &rest args)
    "Wrap `ORIG' to switch perspectives based on the `project-current'."
    (let ((d (apply orig args)))
      (persp-switch (project-name (project-current)))
      (persp-set-buffer d)
      (persp-switch-to-buffer d)))

  (advice-add 'project-find-dir :around #'ad--persp-project-find)
  (advice-add 'project-find-file :around #'ad--persp-project-find)
  (advice-add 'project-dired :around #'ad--persp-project-find)
  (advice-add 'project-vterm :around #'ad--persp-project-find)
  (advice-add 'project-eshell :around #'ad--persp-project-find)
  
  (defun project-perspective ()
    "Open the perspective associated with `project-current'."
    (interactive)
    (persp-switch (project-name (project-current))))
  (add-to-list 'project-switch-commands '(project-perspective "Open Perspective" "P")))

(enable-mode persp-mode)

;;; Nano

(setq nano-font-family-monospaced "Space Mono"
      nano-font-size 14)

;; I don't use all of nano, rather I pick and choose to create my preferred environment.

(require 'nano-base-colors)
(require 'nano-faces)
(nano-faces)
(require 'nano-theme)
;; I don't use all of nano-theme because it breaks the pretty theming of `lambda-themes'.
(nano-theme--diff)
(nano-theme--mode-line)
(require 'nano-layout)
(setq window-divider-default-right-width 12)
;; Disable the modes that are set by `nano-layout'.
(disable-mode tool-bar-mode)
(disable-mode menu-bar-mode)
(disable-mode scroll-bar-mode)
(enable-mode display-time-mode)
(enable-mode display-battery-mode)
(require 'nano-modeline)
;; I override the `nano-modeline-compose' to put extra things into the modeline.
;; Notably, I add: `display-time-string', `org-mode-line-string', `persp-mode-line', and `nerd-icons'.
(defun nano-modeline-compose (status name primary secondary)
  "Compose a string with provided information"
  (let* ((char-width    (window-font-width nil 'header-line))
         (window        (get-buffer-window (current-buffer)))
         (space-up       +0.15)
         (space-down     -0.20)
	 (prefix (cond ((string= status "RO")
			(propertize (if (window-dedicated-p)" -- " " RO ")
                                    'face 'nano-face-header-popout))
		       ((string= status "**")
			(propertize (if (window-dedicated-p)" -- " " ** ")
                                    'face 'nano-face-header-critical))
		       ((string= status "RW")
			(propertize (if (window-dedicated-p)" -- " " RW ")
                                    'face 'nano-face-header-faded))
		       (t (propertize status 'face 'nano-face-header-popout))))
         (left (concat
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-up))
		;; This is a hack that makes the background consistent with the modeline.
		(propertize (nerd-icons-icon-for-buffer) 'face `((:background "#1A1919") ,(or (get-text-property 0 'face (nerd-icons-icon-for-buffer)) 'default)))
		(propertize " " 'face 'nano-face-header-default)
                (propertize name 'face 'nano-face-header-strong)
                (propertize " "  'face 'nano-face-header-default
			    'display `(raise ,space-down))
		(propertize primary 'face 'nano-face-header-default)))
         (right (concat org-mode-line-string " " (s-join "" (persp-mode-line)) " " display-time-string " " secondary "  "))
         (available-width (- (window-total-width) 
			     (length prefix) (length left) (length right)
			     (/ (window-right-divider-width) char-width)))
	 (available-width (max 1 available-width)))
    (concat prefix
	    left
	    (propertize (make-string available-width ?\ )
                        'face 'nano-face-header-default)
	    (propertize right 'face `(:inherit nano-face-header-default
					       :foreground ,nano-color-faded)))))
;; I am handling org clock with the default, so I just replace its call with the default.
(advice-add 'nano-modeline-org-clock-mode :override  #'nano-modeline-default-mode)
(nano-modeline)

(config-for olivetti
  (setopt olivetti-body-width 120
	  olivetti-style nil))

(add-hook 'dired-mode-hook #'nerd-icons-dired-mode)

;; To get ivy rich to work with icons I need to toggle it.
(disable-mode ivy-rich-mode)
(enable-mode nerd-icons-ivy-rich-mode)
(enable-mode ivy-rich-mode)

(add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)

;; Lambda Themes has presets for Org, but I don't like them.
;; This is an override for those to make sure Org looks the way I like.
(with-eval-after-load 'org
  (defun ik/setup-org-theming ()
    (set-face-attribute 'org-level-1 nil :inherit nil :height 1.6 :foreground (face-attribute 'lambda-green :foreground))
    (set-face-attribute 'org-level-2 nil :inherit nil :height 1.4 :foreground (face-attribute 'lambda-red :foreground))
    (set-face-attribute 'org-level-3 nil :inherit nil :height 1.2 :foreground (face-attribute 'lambda-aqua :foreground))
    (set-face-attribute 'org-level-4 nil :inherit nil :height 1.0)
    (set-face-attribute 'org-level-5 nil :inherit nil :height 1.0)
    (set-face-attribute 'org-level-6 nil :inherit nil :height 1.0)
    (set-face-attribute 'org-level-7 nil :inherit nil :height 1.0)
    (set-face-attribute 'org-level-8 nil :inherit nil :height 1.0)
    (set-face-attribute 'org-document-title nil :height 2.0 :foreground (face-attribute 'lambda-blue :foreground)))
  (ik/setup-org-theming))

;; In some views I prefer nothing but text.
;; This function allows me to toggle the nano-modeline.
(defvar nano-modeline-format header-line-format)
(defun toggle-nano-modeline ()
  (interactive)
  (setq-local header-line-format (if header-line-format
				     '()
				   nano-modeline-format)))

(defalias 'yes-or-no-p 'y-or-n-p)

(require-all olivetti perspective)

(provide 'ui)
;;; ui.el ends here
