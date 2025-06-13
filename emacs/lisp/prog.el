;;; prog.el --- Programming Languages Setup          -*- lexical-binding: t; -*-

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
;; This is my coding setup. I primarily work in Python-based environments,
;; a lot on web code. The setup here reflects that.

;;; Code:

(require 'helpers)

(straight-use-package 'tempel)
(straight-use-package 'exec-path-from-shell)
(straight-use-package 'eglot)
(straight-use-package 'js2-mode)
(straight-use-package 'rjsx-mode)
(straight-use-package 'emmet-mode)
(straight-use-package 'web-mode)
(straight-use-package 'pyvenv)
(straight-use-package 'tuareg)
(straight-use-package 'ocaml-eglot)
(straight-use-package 'vterm)
(straight-use-package 'magit)
(straight-use-package 'apheleia)
(straight-use-package 'multiple-cursors)
(straight-use-package 'expand-region)
(straight-use-package 'request)
(straight-use-package 'yaml-mode)

(exec-path-from-shell-initialize)

(setopt tempel-path (emacs-dir "templates")
	tempel-trigger-prefix "<"
	tab-always-indent 'complete)

(defun tempel-setup-capf ()
  ;; Add the Tempel Capf to `completion-at-point-functions'.
  ;; `tempel-expand' only triggers on exact matches. Alternatively use
  ;; `tempel-complete' if you want to see all matches, but then you
  ;; should also configure `tempel-trigger-prefix', such that Tempel
  ;; does not trigger too often when you don't expect it. NOTE: We add
  ;; `tempel-expand' *before* the main programming mode Capf, such
  ;; that it will be tried first.
  (setq-local completion-at-point-functions
              (cons #'tempel-expand
                    completion-at-point-functions)))

(add-hook 'conf-mode-hook 'tempel-setup-capf)
(add-hook 'prog-mode-hook 'tempel-setup-capf)
(add-hook 'text-mode-hook 'tempel-setup-capf)

;;; Formatting

(config-for apheleia
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff ruff-isort))
  (setf (alist-get 'js-mode apheleia-mode-alist)
	'(biome)))

(enable-mode apheleia-global-mode)

;;; Vterm

(defun project-vterm ()
  "Start an inferior shell in the current project's root directory.
If a buffer already exists for running a shell in the project's root,
switch to it.  Otherwise, create a new shell buffer.
With \\[universal-argument] prefix arg, create a new inferior shell buffer even
if one already exists."
  (interactive)
  (require 'comint)
  (let* ((default-directory (project-root (project-current t)))
         (default-project-shell-name (project-prefixed-buffer-name "shell"))
         (shell-buffer (get-buffer default-project-shell-name)))
    (if (and shell-buffer (not current-prefix-arg))
        (if (comint-check-proc shell-buffer)
            (pop-to-buffer shell-buffer (bound-and-true-p display-comint-buffer-action))
          (vterm shell-buffer))
      (vterm (generate-new-buffer-name default-project-shell-name)))))

(advice-add 'project-shell :override #'project-vterm)

;;; Editting

(enable-mode delete-selection-mode)

;;; Elisp

(add-hook 'emacs-lisp-mode-hook #'electric-pair-local-mode)


;;; Python

(config-for pyvenv
  (defun activate-venv-if-present ()
    "If there is a uv-based \".venv\" available, make that the current python."
    (interactive)
    (let* ((project-dir (current-project-root))
	   (venv-dir-name (concat project-dir ".venv")))
      (when (file-exists-p venv-dir-name)
	(pyvenv-activate venv-dir-name)))))

(add-hook 'python-base-mode #'activate-venv-if-present)
(add-hook 'python-base-mode-hook #'electric-pair-local-mode)

;;; JS

(add-hook 'js-mode #'js2-minor-mode)
(add-hook 'js-mode #'electric-pair-local-mode)
(add-hook 'rjsx-mode #'electric-pair-local-mode)
(add-to-list 'auto-mode-alist '("\\.mjs\\'" . js-mode))
(add-to-list 'auto-mode-alist '("\\.cjs\\'" . js-mode))

;;; Web

(add-hook 'web-mode #'emmet-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;; OCaml

(add-hook 'tuareg-mode-hook #'merlin-mode)
;; By default tuareg doesn't make mode-name a string. This breaks the modeline.
;; This hook fixes that.
(add-hook 'tuareg-mode-hook (lambda () (setq-local mode-name "OCaml")))
(require 'dune)

;;; Eglot

(config-for eglot
  (setq eglot-server-programs
	'(((python-ts-mode python-mode) "pylsp")
	  ((js-mode js-ts-mode rjsx-mode) ("typescript-language-server" "--stdio"))
	  (scss-mode ("some-sass-language-server" "--stdio"))
	  (js-json-mode "vscode-json-language-server")))
  (add-hook 'eglot-managed-mode-hook #'tempel-setup-capf))


;;; Initial Mode

(defun get-random-quote ()
  "Fetch a random quote and make that the scratch buffer's text."
  (request "https://thequoteshub.com/api/"
    :sync t
    :parser 'json-read
    :error (cl-function
	    (lambda (&key data &allow-other-keys)
	      (message "Error fetching. No quote shown")))
    :success (cl-function
	      (lambda (&key data &allow-other-keys)
		(setq initial-scratch-message
		      (concat "/" (alist-get 'text data) "/\n"
			      (alist-get 'author data)))))))



(setq initial-major-mode 'org-mode)
(get-random-quote)

(require-all tempel pyvenv vterm tuareg magit multiple-cursors-core)

(provide 'prog)
;;; prog.el ends here
