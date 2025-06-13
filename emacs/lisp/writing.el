;;; writing.el --- Writing Configuration             -*- lexical-binding: t; -*-

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
;; I use Emacs as a researcher quite a bit. I find the plain text workflow to best
;; suit my brain. I use `org' extensively, with `denote' as my preferred note-taking tool.

;;; Code:

(require 'helpers)

(straight-use-package 's)		; This is used later, so I just add it here.
(require 's)
(straight-use-package 'org)
(straight-use-package 'org-modern)	; To make org *pretty*
(straight-use-package 'denote)
(straight-use-package 'denote-silo)	; To separate my work notes and personal notes
(straight-use-package 'citar)		; To provide a rich interface for citations
(straight-use-package 'citar-denote)
(straight-use-package 'pdf-tools)	; To have `AUCTeX' previews
(straight-use-package 'markdown-mode)	; For GitHub READMEs
(straight-use-package 'auctex)		; For writing latex
(straight-use-package 'reftex)		; For navigating latex
(straight-use-package 'jinx)		; For spell-checking

(config-for org
  (require 'org-tempo)
  (require 'org-habit)
  (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'variable-pitch-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook #'jinx-mode)
  (setq org-directory "~/Dropbox/ZK"
	org-agenda-files '("~/Dropbox/Personal.org"
			   "~/Dropbox/Research.org"
			   "~/Dropbox/CUNE.org"
			   "~/Dropbox/School.org"
			   "~/Dropbox/CUNE-Cal.org"
			   "~/Dropbox/GCal.org"
			   "~/Dropbox/UNL-Cal.org"
			   "~/Dropbox/ZK/")
	org-agenda-file-regexp "\\`[^.].*agenda.*\\.org\\'"
	org-todo-keywords '((sequence "TODO(t)" "|" "DONE(d!)" "CANCELLED(l!)")))

  (setq org-capture-templates
	'(("p" "Personal Task" entry (file+headline "~/Dropbox/Personal.org" "Personal Tasks")
	   "** TODO %? %^g\n:PROPERTIES:\n:CAPTURE: %T\n:FILE: [[%(buffer-name (get-file-buffer \"%f\"))][%F]]\n:TASK: %K\n:END:\n" :empty-lines 1)
	  ("r" "Research Task" entry (file+headline "~/Dropbox/Research.org" "Research Tasks")
	   "** TODO %? %^g\n:PROPERTIES:\n:CAPTURE: %T\n:FILE: [[%(buffer-name (get-file-buffer \"%f\"))][%F]]\n:TASK: %K\n:END:\n" :empty-lines 1)
	  ("c" "CUNE Task" entry (file+headline "~/Dropbox/CUNE.org" "Concordia Tasks")
	   "** TODO %? %^g\n:PROPERTIES:\n:CAPTURE: %T\n:FILE: [[%(buffer-name (get-file-buffer \"%f\"))][%F]]\n:TASK: %K\n:END:\n" :empty-lines 1)
	  ("s" "School Task" entry (file+headline "~/Dropbox/School.org" "School/Homework Tasks")
	   "** TODO %? %^g\n:PROPERTIES:\n:CAPTURE: %T\n:FILE: [[%(buffer-name (get-file-buffer \"%f\"))][%F]]\n:TASK: %K\n:END:\n" :empty-lines 1)
	  ("f" "File Note" entry (clock)
	   "** TODO %? %^g\n:PROPERTIES:\n:CAPTURE: %T\n:FILE: [[%f][%F]]\n:TASK:%K\n:END:\n" :empty-lines 1)))
  (defun extract-html-title-libxml (url)
    "Fetch the URL and extract the <title> tag using libxml parsing."
    (with-current-buffer (url-retrieve-synchronously url t t)
      (goto-char (point-min))
      (re-search-forward "^$" nil 'move) ; Skip HTTP headers
      (let* ((dom (libxml-parse-html-region (point) (point-max)))
             (title-node (car (dom-by-tag dom 'title))))
	(kill-buffer)
	(when title-node
          (string-trim (dom-text title-node))))))
  (defun org-extract-title-from-link (link)
    "Extract the title of a webpage, then format it as an org link."
    (interactive "sURL: ")
    (let ((title (extract-html-title-libxml link)))
      (insert (format "[[%s][%s]]" link title) "\n"))))

(config-for denote
  (add-hook 'dired-mode-hook #'denote-dired-mode)
  (add-hook 'org-mode-hook #'denote-rename-buffer-mode)
  (setq denote-directory org-directory
	xref-search-program 'ripgrep)

  ;; This function is inspired by a video I saw from "Linking Your Thinking" about the value of links, even if the note is empty.
  (defun denote-create-empty-note-and-link (title)
    "Create an empty note with the title of region. Then link it back to the original document."
    (interactive (list (cond
			((use-region-p)
			 (buffer-substring-no-properties (region-beginning) (region-end)))
			(t
			 (ivy-read "Title: ")))))
    (let ((current-buf (current-buffer))
	  (note (denote title)))
      (with-current-buffer (get-file-buffer note)
	(save-buffer)
	(if (not current-prefix-arg)
	    (kill-this-buffer)))
      (with-current-buffer current-buf
	(denote-link note 'org (org-get-title note)))
      (if current-prefix-arg
	  (switch-to-buffer (get-file-buffer note))
	(switch-to-buffer current-buf)))))

(config-for denote-silo
  (setq denote-silo-directories (list denote-directory
				      "~/Dropbox/Work/UNL/"
				      "~/Dropbox/Work/CUNE/"))
  (defun my/denote (&optional choose-silo)
    "Wrapper for denote that can switch silos based on `current-prefix-arg'."
    (interactive "P")
    (call-interactively (if choose-silo #'denote-silo-create-note #'denote))))

(config-for citar
  (setopt org-cite-global-bibliography '("~/Dropbox/ZK/References.bib")
	  org-cite-insert-processor 'citar
	  org-cite-follow-processor 'citar
	  org-cite-activate-processor 'citar)
  (setopt citar-bibliography org-cite-global-bibliography))

(config-for citar-denote
  (setopt citar-denote-file-type 'org
	  citar-denote-keyword "bib"
	  citar-denote-signature nil
	  citar-denote-subdir nil
	  citar-denote-template nil
	  citar-denote-title-format "title"
	  citar-denote-title-format-andstr "and"
	  citar-denote-use-bib-keywords nil)
  (enable-mode citar-denote-mode)
  ;; Enables Zotero links to work correctly.
  (org-link-set-parameters
   "zotero"
   :follow
   (lambda (path _)
     (call-process "xdg-open" nil nil nil (concat "zotero:" path)))))

(config-for org-modern
  (add-hook 'org-mode-hook 'org-modern-mode)
  (setopt org-modern-replace-stars "12345678"
	  org-modern-star 'replace
	  org-modern-keyword '(("title" . "Τ")
			       ("author" . "Α")
			       ("date" . "Δ")
			       ("theme" . "τ")
			       ("identifier" . "Ι")
			       ("references" . "Ρ")
			       ("filetags" . "ϕ")
			       (t . t))))

(config-for auctex
  (add-hook 'LaTeX-mode-hook #'TeX-fold-mode)
  (add-hook 'LaTeX-mode-hook #'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook #'olivetti-mode)
  (setq TeX-auto-save t
	TeX-parse-self t
	TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-source-correlate-start-server t
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (require 'f)
  ;; This is a hooked function to always open the pdf to the right side-view.
  (defun setup-pdf-view ()
    (setq-local display-buffer-alist
		(append
		 display-buffer-alist
		 (list `(,(concat (f-base (buffer-file-name)) ".pdf")
			 (display-buffer-in-side-window)
			 (side . right)
			 (window-width . 0.5))))))
  (defun narrow-to-section ()
    "Narrow to the current `LaTeX' section."
    (interactive)
    (LaTeX-mark-section)
    (call-interactively #'narrow-to-region)
    (deactivate-mark t))
  (add-hook 'LaTeX-mode-hook #'setup-pdf-view))

(config-for reftex
  (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
  (add-hook 'LaTeX-mode-hook #'activate-local-bib-if-tex)

  (setq reftex-plug-into-AUCTeX t)
  (defun activate-local-bib-if-tex ()
    "Set `citar' to use the locally specified bib file in the tex document."
    (when (string-equal "tex" (file-name-extension (buffer-file-name)))
      (save-excursion
	(goto-char (point-min))
	(when (re-search-forward (rx "\\addbibresource{" (* letter) "\." (one-or-more letter)) nil t)
	  (let* ((file (thing-at-point 'filename))
		 (start (+ 1 (s-index-of "{" file)))
		 (end (s-index-of "}" file)))
	    (if (length> (substring file start end) 0)
		(setq-local citar-bibliography (list (format "./%s" (substring file start end))))
	      (message "No Bib File Found."))))))))

(config-for pdf-tools
  (pdf-tools-install :no-query))

(add-to-list 'magic-mode-alist '("%PDF" . pdf-view-mode))

(defun create-or-open-journal-entry ()
  "Open current day's journal entry."
  (interactive "P")
  (let* ((day (format-time-string "%Y-%m-%d"))
	 (filename (concat (expand-file-name "~/Dropbox/Journal/") day ".org"))
	 (exists? (file-exists-p filename))
	 (file-buffer (find-file-noselect filename)))
    (unless exists?
      (with-current-buffer file-buffer
	(insert (concat "#+TITLE: " (format-time-string "%A %B %d, %Y (W%W)") "\n"))
	(insert (concat "#+AUTHOR: " user-full-name "\n"))
	(insert "\n")
	(insert "\n")
	(insert "* Verse of the Day \n")
	(save-buffer)))
    (display-buffer file-buffer)
    (switch-to-buffer file-buffer)
    (delete-other-windows)))



(require-all org org-modern denote denote-silo citar citar-denote auctex reftex markdown-mode pdf-tools)

(provide 'writing)
;;; writing.el ends here
