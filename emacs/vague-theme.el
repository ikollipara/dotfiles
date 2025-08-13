;;; vague-theme.el --- A port of vague.nvim          -*- lexical-binding: t; -*-

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

(use-package rainbow-mode)

(deftheme vague
  "A low-contrast, pastel Emacs theme inspired by vague.nvim.")

(let ((class '((class color) (min-colors 89)))
      ;; Vague.nvim colors
      (bg           "#141415")
      (fg           "#cdcdcd")
      (floatBoarder "#878787")
      (line         "#252530")
      (comment      "#606079")
      (builtin      "#b4d4cf")
      (func         "#c48282")
      (string       "#e8b589")
      (number       "#e0a363")
      (property     "#c3c3d5")
      (constant     "#aeaed1")
      (parameter    "#bb9dbd")
      (visual       "#333738")
      (error_       "#d8647e")
      (warning      "#f3be7c")
      (hint         "#7e98e8")
      (operator     "#90a0b5")
      (keyword      "#6e94b2")
      (type         "#9bb4bc")
      (search       "#405065")
      (plus         "#7fa563")
      (delta        "#f4be7c"))
  (set-face-attribute 'error nil :foreground error_)
  (custom-theme-set-faces
   'vague

   ;; --- Basic UI ---
   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,fg))))
   `(fringe ((,class (:background ,floatBoarder))))
   `(region ((,class (:background ,visual))))
   `(highlight ((,class (:background ,visual))))
   `(minibuffer-prompt ((,class (:foreground ,fg :weight bold))))
   `(vertical-border ((,class (:foreground ,floatBoarder))))
   `(link ((,class (:foreground ,keyword :underline t))))
   `(border ((,class (:backgrund ,floatBoarder))))
   `(shadow ((,class (:foreground ,comment))))
   `(window-divider ((,class (:foreground ,bg))))
   `(window-divider-first-pixel ((,class (:foreground ,bg))))
   `(window-divider-last-pixel ((,class (:foreground ,bg))))
   `(vertical-border ((,class (:foreground ,bg))))
   `(isearch ((,class (:foreground ,fg :background ,search))))

   ;; --- Ansi Term ---
   `(ansi-color-blue ((,class (:foreground ,keyword :background ,keyword))))
   `(ansi-color-black ((,class (:foreground ,line :background ,line))))
   `(ansi-color-red ((,class (:foreground ,error_ :background ,error_))))
   `(ansi-color-green ((,class (:foreground ,plus :background ,plus))))
   `(ansi-color-yellow ((,class (:foreground ,warning :background ,warning))))
   `(ansi-color-magenta ((,class (:foreground ,parameter :background ,parameter))))
   `(ansi-color-cyan ((,class (:foreground ,constant :background ,constant))))
   `(ansi-color-white ((,class (:foreground ,fg :background ,fg))))

   `(ansi-color-bright-blue ((,class (:foreground ,keyword :background ,keyword))))
   `(ansi-color-bright-black ((,class (:foreground ,line :background ,line))))
   `(ansi-color-bright-red ((,class (:foreground ,error_ :background ,error_))))
   `(ansi-color-bright-green ((,class (:foreground ,plus :background ,plus))))
   `(ansi-color-bright-yellow ((,class (:foreground ,warning :background ,warning))))
   `(ansi-color-bright-magenta ((,class (:foreground ,parameter :background ,parameter))))
   `(ansi-color-bright-cyan ((,class (:foreground ,constant :background ,constant))))
   `(ansi-color-bright-white ((,class (:foreground ,fg :background ,fg))))


   ;; --- Mode Line ---
   `(mode-line ((,class (:background ,bg :foreground ,bg))))
   `(mode-line-inactive ((,class (:background ,bg :foreground ,fg))))

   ;; --- Syntax Highlighting ---
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-constant-face ((,class (:foreground ,constant))))
   `(font-lock-function-name-face ((,class (:foreground ,func))))
   `(font-lock-keyword-face ((,class (:foreground ,keyword :weight bold))))
   `(font-lock-string-face ((,class (:foreground ,string))))
   `(font-lock-type-face ((,class (:foreground ,type))))
   `(font-lock-variable-name-face ((,class (:foreground ,constant))))
   `(font-lock-warning-face ((,class (:foreground ,warning :weight bold))))
   `(font-lock-number-face ((,class (:foreground ,number))))
   `(font-lock-operator-face ((,class (:foreground ,operator))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))

   ;; --- Org Mode ---
   `(org-level-1 ((,class (:foreground ,hint :weight bold :height 1.2))))
   `(org-level-2 ((,class (:foreground ,builtin :weight bold))))
   `(org-level-3 ((,class (:foreground ,string))))
   `(org-level-4 ((,class (:foreground ,error_))))
   `(org-link ((,class (:foreground ,plus :underline t))))
   `(org-block ((,class (:background ,bg :foreground ,fg))))
   `(org-code ((,class (:background ,bg :foreground ,string))))
   `(org-verbatim ((,class (:foreground ,keyword))))
   `(org-table ((,class (:foreground ,delta))))
   `(org-document-title ((,class (:foreground ,number :weight bold :height 1.3))))

   ;; --- LSP Mode ---
   `(lsp-face-highlight-textual ((,class (:background ,fg :foreground ,bg))))
   `(lsp-face-semhl-variable-read ((,class (:foreground ,constant))))
   `(lsp-face-semhl-variable-write ((,class (:foreground ,constant :underline t))))
   `(lsp-face-semhl-function ((,class (:foreground ,func))))
   `(lsp-face-semhl-keyword ((,class (:foreground ,keyword :weight bold))))
   `(lsp-face-semhl-type ((,class (:foreground ,type))))
   `(lsp-face-semhl-number ((,class (:foreground ,number))))
   `(lsp-face-semhl-struct ((,class (:foreground ,constant))))
   `(lsp-face-semhl-comment ((,class (:foreground ,comment))))
   `(lsp-face-semhl-parameter ((,class (:foreground ,parameter))))

   ;; --- Emacs Lisp ---
   `(elisp-shorthand-font-lock-face ((,class (:foreground ,string))))
   `(eval-sexp-fu-flash ((,class (:background ,keyword :foreground ,bg))))

   ;; --- F# ---
   `(fsharp-ui-error-face ((,class (:foreground ,error_))))
   `(fsharp-ui-operator-face ((,class (:foreground ,operator))))
   `(fsharp-ui-warning-face ((,class (:foreground ,warning))))
   `(fsharp-ui-generic-face ((,class (:foreground ,type))))

   ;; --- Python Mode ---
   `(python-builtin-face ((,class (:foreground ,builtin))))
   `(python-function-call-face ((,class (:foreground ,func))))
   `(python-variable-name-face ((,class (:foreground ,constant))))
   `(python-string-face ((,class (:foreground ,string))))
   `(python-comment-face ((,class (:foreground ,comment :slant italic))))

   ;; --- Avy ---
   `(avy-lead-face ((,class (:background ,error_ :foreground ,fg))))
   `(avy-lead-face-0 ((,class (:background ,hint :foreground ,fg))))

   ;; --- Ivy ---
   `(ivy-current-match ((,class (:background ,search))))
   `(completions-annotations ((,class (:foreground ,comment :inherit italic))))
   ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'vague)

(provide 'vague-theme)
;;; vague-theme.el ends here
