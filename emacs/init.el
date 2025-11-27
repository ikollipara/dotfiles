;;; init.el --- Emacs Configuration                  -*- lexical-binding: t; -*-

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

;; This is my entire Emacs configuration. I've spent a lot of time
;; tweaking this configuration to get it exactly where I want it.
;; It uses some older packages, but I find the flow I get with them
;; to far exceed the flow I get with other tools.
;;
;; In this configuration, you'll see headings demarked by `;;;;'.
;; Subheadings will have `;;;'.
;;
;; In addition, you'll see  to demark pages.

;;; Code:

;;;; Straight.el 
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

;;;; Base Emacs Configuration 
;;; Preface

;;; Helper Macros
;; I use a few macros to make the code a bit clearer.

(defmacro enable-mode (mode)
  "Enable the given MODE."
  `(,mode t))

(defmacro disable-mode (mode)
  "Disable the given MODE."
  `(,mode -1))

(defmacro emacs-dir (path)
  "Create a filepath that is based in the `user-emacs-directory'."
  `(concat user-emacs-directory ,path))

(defmacro ilambda (&rest body)
  "Create an interactive lambda with BODY."
  (declare (ident defun))
  `(lambda () (interactive) ,@body))

(defalias 'install 'straight-use-package "Install a package")
(defmacro install-and-require (pkg &optional melpa-recipe)
  "Install and Require the given PKG."
  `(progn
     (install ',(if melpa-recipe melpa-recipe pkg))
     (require ',pkg)))

;;; User Settings

(setopt user-full-name "Ian Kollipara"
	user-mail-address "ian.kollipara@gmail.com"
	user-emacs-directory (expand-file-name "~/.config/emacs/"))

;;; Built-in modes

(dolist (pair '((tool-bar-mode . -1)
		(menu-bar-mode . -1)
		(scroll-bar-mode . -1)
		(display-time-mode . t)
		(recentf-mode . t)
		(delete-selection-mode . t)))
  (funcall (car pair) (cdr pair)))


(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;;; Custom File

(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;; Use Package

(straight-use-package 'use-package)
(setq straight-use-package-by-default t
      use-package-enable-imenu-support t)

;;; Buffer Keybinds
(global-set-key (kbd "C-x x b") 'previous-buffer)
(global-set-key (kbd "C-x x f") 'next-buffer)
(global-set-key (kbd "C-x x F") 'font-lock-update)

;;; No Littering
;; No Littering is a tool I use often to help with the sheer amount of files
;; created by Emacs. I'm sure there are other ways, but this is the simplest for me.

(use-package no-littering
  :custom
  (version-control t)
  (kept-old-versions 6)
  (kept-new-versions 2)
  (delete-old-versions t)
  (backup-by-copying t)
  :config
  (setq backup-directory-alist `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
	lock-file-name-transforms `(("\\`/.*/\\([^/]+\\)\\'" "/var/tmp/\\1" t))))


;;;; Completion Framework 
;;; Ivy
;; I choose to use the `ivy' completion framework. It's a bit older, but its tight
;; integration with `avy' means that I can use a similar set of keybinds for selecting
;; nearly everything.


(use-package ivy
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-use-selectable-prompt t)
  :init
  (enable-mode ivy-mode)
  :config
  ;; We add the `swiper' repo to the path so we can use `ivy-avy'.
  (add-to-list 'load-path (emacs-dir "straight/repos/swiper")))

(use-package counsel
  :after ivy
  :bind (("C-r" . counsel-search-dwim)
	 ("M-s r" . counsel-recentf)
	 ("C-c J" . (lambda () (interactive) (ivy-read "File: " '("~/Code/" "~/.config/emacs/") :action (lambda (x) (dired x))))))
  :custom
  (counsel-find-file-at-point t)
  :init
  (enable-mode counsel-mode)
  :config
  (ivy-configure 'counsel-M-x :initial-input "")
  (ivy-configure 'counsel-org-capture :initial-input "")
  (ivy-configure 'counsel-describe-symbol :initial-input "")
  (ivy-configure 'counsel-minor :initial-input "")
  (ivy-configure 'counsel-package :initial-input "")
  (setenv "FZF_DEFAULT_COMMAND" "fd --type f --strip-cwd-prefix --hidden --follow --exclude .git")

  (defun counsel-search-dwim ()
    "Search using `counsel-fzf' when prefixed, otherwise use `counsel-rg'."
    (interactive)
    (call-interactively (if current-prefix-arg #'counsel-fzf #'counsel-rg))))

(use-package swiper
  :after counsel
  :bind (("C-s" . swiper-dwim)
	 (:map swiper-map
	       ("C-r" . swiper-C-r)))
  :config
  (defun swiper-C-r (&optional arg)
    "The counterpart to `swiper-C-s' that doesn't exist."
    (interactive "p")
    (if (string= ivy-text "")
	(ivy-previous-history-element 1)
      (ivy-previous-line arg)))

  (defun swiper-dwim ()
    "Call `swiper-thing-at-point' if region is active, call `swiper-all' when prefixed, call `swiper-isearch' when double prefixed, otherwise call `swiper'."
    (interactive)
    (call-interactively
     (cond
      ((and (region-active-p) (equal current-prefix-arg '(16)))
       #'swiper-isearch-thing-at-point)
      ((and (region-active-p) (equal current-prefix-arg '(4)))
       #'swiper-all-thing-at-point)
      ((region-active-p)
       #'swiper-thing-at-point)
      ((equal current-prefix-arg '(16))
       #'swiper-isearch)
      ((equal current-prefix-arg '(4))
       #'swiper-all)
      (t
       #'swiper)))))

(use-package ivy-rich
  :init (enable-mode ivy-rich-mode))

(use-package nerd-icons)
(use-package nerd-icons-ivy-rich
  :after (:all nerd-icons ivy-rich)
  :init
  (enable-mode nerd-icons-ivy-rich-mode)
  (ivy-rich-reload))

(use-package amx)

;;; Autocomplete
;; I use `corfu' for my autocomplete. It's a strange pairing, but I prefer the
;; corfu setup.

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  :bind (:map corfu-map
	      ("C-'" . corfu-quick-complete))
  :init
  (enable-mode global-corfu-mode)
  :config
  (enable-mode corfu-popupinfo-mode)
  (enable-mode corfu-history-mode))

(use-package nerd-icons-corfu
  :after (:all nerd-icons corfu)
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))


;;; Fuzzy Matching
;; For fuzzy matching I use orderless.

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  :hook
  (ivy-mode . orderless--setup-ivy)
  :config
  (defun orderless--setup-ivy ()
    "Setup `ivy' to play with `orderless'."
    (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
    (add-to-list 'ivy-highlight-functions-alist '(orderless-ivy-re-builder . orderless-ivy-highlight))))



;;;; UX and UI 
;;; Editor Theme
;; In the past I've used the "lambda-emacs" theme from "https://github.com/lambda-emacs/lambda-themes".
;; However, I've moved away to try something with a bit more color.

(add-to-list 'custom-theme-load-path (emacs-dir ""))
;; (load-theme 'vague t)

(defvar nano-modeline-position 'top)
(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes")
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-vibrant t)
  :config
  (load-theme 'lambda-dark t))

(use-package catppuccin-theme
  :custom
  (catppuccin-flavor 'mocha)
  :config
  (defun catppuccin-pick-flavor (flavor)
    "Pick and load a particular `catppuccin-flavor' to use as a theme."
    (interactive (list (ivy-read "Catppuccin Flavor: " '(latte frappe macchiato mocha))))
    (setq catppuccin-flavor (intern flavor))
    (catppuccin-reload)))

;;; Modeline
;; I use a highly customized `nano-modeline' from `rougier/nano-emacs'.
;; I find rougier's style to be quite creative and I enjoy his work a lot.

(setopt battery-mode-line-format "{%t (%b%p)}")
(enable-mode display-battery-mode)

(use-package nano
  :straight '(nano :type git :host github :repo "rougier/nano-emacs")
  :no-require t
  :init
  (setopt nano-font-family-monospaced "Space Mono"
	  nano-font-size 14)
  (require 'nano-base-colors)
  (require 'nano-faces)
  (nano-faces)
  (require 'nano-theme)
  (nano-theme--mode-line)
  (require 'nano-layout)
  (setq window-divider-default-right-width 1)
  (setf (alist-get 'left-fringe default-frame-alist) 0)
  (setf (alist-get 'right-fringe default-frame-alist) 0)
  (disable-mode tool-bar-mode)
  (disable-mode scroll-bar-mode)
  (require 'nano-modeline)
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
		  (propertize
		   (nerd-icons-icon-for-buffer)
		   'face `((:background
			    ,(face-attribute 'nano-face-header-default :background))
			   ,(or (get-text-property 0 'face (nerd-icons-icon-for-buffer)) 'nano-face-header-default)))
		  (propertize " " 'face 'nano-face-header-default)
                  (propertize name 'face 'nano-face-header-strong)
                  (propertize " "  'face 'nano-face-header-default
			      'display `(raise ,space-down))
		  (propertize primary 'face 'nano-face-header-default)))
           (right (concat
		   (when (featurep 'org-timer)
		     (if org-timer-mode-line-timer
			 (concat org-timer-mode-line-string " ")
		       ""))
		   (when (featurep 'org)
		     org-mode-line-string)
		   " " (when (featurep 'perspective)
			 (string-join (persp-mode-line) ""))
		   " " battery-mode-line-string
		   " " display-time-string
		   " " secondary
		   "  "))
           (available-width (- (window-total-width) 
			       (length prefix) (length left) (length right)
			       (/ (window-right-divider-width) char-width)))
	   (available-width (max 1 available-width)))
      (concat prefix
	      left
	      (propertize (make-string available-width ?\ )
                          'face 'nano-face-header-default)
	      (propertize right 'face `(:inherit nano-face-header-default
						 :foreground "#EBE9E7")))))

  (advice-add 'nano-modelie-org-clock-mode :override #'nano-modeline-default-mode)
  (nano-modeline)

  (defvar nano-modeline-format header-line-format)
  (defun toggle-nano-modeline ()
    "Toggle the display of `nano-modeline'."
    (interactive)
    (setq-local header-line-format (if header-line-format nil nano-modeline-format)))

  (advice-add 'catppuccin-pick-flavor
	      :after (lambda (&rest args)
		       (setq nano-base-colors--defaults
			     `((foreground . ,(face-foreground 'default nil t))
			       (background . ,(face-background 'default nil t))
			       (highlight . ,(face-background 'fringe nil t))
			       (critical . ,(face-foreground 'error nil t))
			       (salient . ,(face-foreground 'font-lock-keyword-face nil t))
			       (strong . ,(face-foreground 'default nil t))
			       (popout . ,(face-foreground 'font-lock-string-face nil t))
			       (subtle . ,(face-background 'mode-line-inactive nil t))
			       (faded . ,(face-foreground 'shadow nil t))))
		       (setopt nano-color-foreground (nano-base-colors--get 'foreground)
			       nano-color-background (nano-base-colors--get 'background)
			       nano-color-highlight (nano-base-colors--get 'highlight)
			       nano-color-critical (nano-base-colors--get 'critical)
			       nano-color-salient (nano-base-colors--get 'salient)
			       nano-color-strong (nano-base-colors--get 'strong)
			       nano-color-popout (nano-base-colors--get 'popout)
			       nano-color-subtle (nano-base-colors--get 'subtle)
			       nano-color-faded (nano-base-colors--get 'faded))
		       (nano-faces)
		       (set-face-attribute 'internal-border nil :background (face-attribute 'default :background)))))

(defun transparent-p ()
  "Check if transparency is enabled."
  (not (eq (frame-parameter nil 'alpha-background) 100)))

(defun toggle-transparency (value)
  "Toggle the transparency. When prefixed, ask for a value."
  (interactive (list
		(if current-prefix-arg
		    (read-number "Transparency Percent: " 80)
		  (if (transparent-p) 100 80))))
  (set-frame-parameter (selected-frame) 'alpha-background value))

;;; Persp
;; I use perspective.el to manage much of my workflow. I have a few custom integrations to make working with it as easy as possible.

(use-package perspective
  :custom
  (persp-interactive-completion-function 'ivy-completing-read)
  (persp-modestring-short t)
  (persp-suppress-no-prefix-key-warning t)
  (persp-mode-prefix-key (kbd "C-]"))
  :bind (("C-x b" . persp-counsel-switch-buffer)
	 ("C-x k" . kill-buffer-dwim))
  :config
  (defun kill-buffer-dwim ()
    "Kill the buffer using `kill-current-buffer' unless prefixed, call `persp-kill-buffer*'."
    (interactive)
    (call-interactively (if current-prefix-arg #'persp-kill-buffer* #'kill-current-buffer)))
  (defun advice--create-new-persp-after (orig &rest args)
    "Create a new perspective, or switch to one, after running original function.
Used in the integration of project.el with perspective.el"
    (let ((d (apply orig args)))
      (persp-switch (project-name (project-current nil)))
      (persp-set-buffer d)
      (persp-switch-to-buffer d)))

  (advice-add 'project-find-dir :around #'advice--create-new-persp-after)
  (advice-add 'project-find-file :around #'advice--create-new-persp-after)
  (advice-add 'project-dired :around #'advice--create-new-persp-after)
  (advice-add 'project-shell :around #'advice--create-new-persp-after)
  (advice-add 'project-eshell :around #'advice--create-new-persp-after)
  :init
  (enable-mode persp-mode))

;;; Fonts
;; I use 2 fonts for my work. One is for coding, one is for writing.
;; This distinction is based on the thinking from https://notes.alexkehayias.com/turn-emacs-into-a-focused-writing-tool/.


(set-face-attribute 'default nil :family "Cascadia Code" :height 110)
(set-face-attribute 'variable-pitch nil :family "Space Mono" :height 110)

;;; Ligatures
;; No coding is complete without strong ligature support.

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
					 (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  ;; Enables ligature checks globally in all buffers. You can also do it
  ;; per mode with `ligature-mode'.
  :init
  (global-ligature-mode t))

;;; Helpful
;; I use helpful a ton since Emacs provides such rich documentation features.

(use-package helpful
  :bind (("C-h k" . helpful-key)
	 ("C-h x" . helpful-command))
  :init
  (with-suppressed-warnings 'emacs
    (setopt counsel-describe-symbol-function #'helpful-symbol
	    counsel-describe-function-function #'helpful-callable
	    counsel-describe-variable-function #'helpful-variable)))


;;; Movement
;; To move around, I use `avy' and `other-window' + `ace-window'.

(use-package avy
  :after ivy
  :bind (("C-'" . avy-dwim)
	 ("M-'" . avy-goto-line))
  :init
  (defun avy-dwim ()
    "When prefixed, call `avy-goto-char-in-line', otherwise call `avy-goto-char'"
    (interactive)
    (call-interactively (if current-prefix-arg #'avy-goto-char-in-line #'avy-goto-char)))
  (require 'ivy-avy))

(use-package ace-window
  :bind (("M-o" . other-window-dwim))
  :custom
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (aw-dispatch-always t)
  :init
  (unbind-key "C-x o")
  (defun other-window-dwim ()
    "When prefixed, call `ace-window', otherwise call `other-window'."
    (interactive)
    (if current-prefix-arg
	(let ((current-prefix-arg nil))
	  (call-interactively #'ace-window))
      (call-interactively #'other-window))))

;;;; Programming 

;;; Mise

(use-package mise
  :hook (after-init . global-mise-mode))

;;; Yasnippet

(use-package yasnippet
  :bind (("M-/" . yas-insert-snippet))
  :custom
  (yas-snippet-dirs (list (emacs-dir "snippets")))
  :init
  (enable-mode yas-global-mode))

;;; Vterm

(use-package vterm
  :bind (("C-c s" . vterm)
	 ("C-x p s" . project-vterm))
  :init
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
	(vterm (generate-new-buffer-name default-project-shell-name))))))

;;; Paths

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

;;; Coloration

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode-enable))

;;; Utilities

(use-package transient :defer nil)

;;; Magit
;; I use git a lot, so I choose to work with it through magit

(use-package magit
  :bind (("C-x g" . magit)))

;;; LSP
;; I use `lsp-mode' to handle my lsp needs.

(use-package flycheck
  :bind ((:map flycheck-mode-map
	       ("C-c ! l" . flycheck-list-errors-dwim)))
  :init
  (defun flycheck-list-errors-dwim ()
    "List the errors using `counsel-flycheck' unless prefixed, then call `flycheck-list-errors'."
    (interactive)
    (call-interactively (if current-prefix-arg #'flycheck-list-errors #'counsel-flycheck))))

(use-package lsp-mode
  :after corfu
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-completion-provider :none)
  :hook
  (lsp-completion-mode . lsp-mode--setup-completion-for-corfu)
  :init
  (setq lsp-keymap-prefix "C-c l")
  (defun lsp-mode--setup-completion-for-corfu ()
    "Setup `lsp-mode' to play well with `corfu'."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection
		     '("some-sass-language-server" "--stdio")
		     (lambda () (executable-find "some-sass-language-server")))
    :activation-fn (lsp-activate-on "scss")
    :add-on? t
    :server-id 'some-sass))
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("crystalline"))
    :activation-fn (lsp-activate-on "crystal")
    :priority '1
    :server-id 'crystalline)))

(use-package lsp-ui
  :after lsp-mode
  :bind ((:map lsp-mode-map
	       ("M-g M-i" . lsp-ui-imenu))
	 (:map lsp-ui-imenu-mode-map
	       ("C-'" . lsp-ui-avy-visit)))
  :init
  (defun lsp-ui-avy-visit ()
    (interactive)
    (avy-goto-line)
    (lsp-ui-imenu--visit)))

;;; Treesitter
;; I use tree-sitter for both movement and better syntax highlighting

(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all)
  (treesit-language-source-alist
   '((fsharp "https://github.com/ionide/tree-sitter-fsharp" "main" "fsharp/src")
     (emacs-lisp "https://github.com/Wilfred/tree-sitter-elisp")
     (scss "https://github.com/serenadeai/tree-sitter-scss")
     (elm "https://github.com/elm-tooling/tree-sitter-elm")))
  :config
  (enable-mode global-treesit-auto-mode))

(use-package expreg
  :straight '(expreg :type git :host github :repo "casouri/expreg")
  :bind (("M-SPC" . expreg-expand)))


;;; Formatting

(use-package apheleia
  :config

  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
	'(ruff ruff-isort))
  (setf (alist-get 'python-mode apheleia-mode-alist)
	'(ruff ruff-isort))
  (setf (alist-get 'python-base-mode apheleia-mode-alist)
	'(ruff ruff-isort))
  (setf (alist-get 'js-mode apheleia-mode-alist)
	'(biome))
  (setf (alist-get 'js-ts-mode apheleia-mode-alist)
	'(biome))
  (setf (alist-get 'js-json-mode apheleia-mode-alist)
	'(biome))
  (push '(djade . ("djade" inplace)) apheleia-formatters)
  (push 
   '(fantomas . ("fantomas" inplace "--out" inplace))
   apheleia-formatters)
  (setf (alist-get 'fsharp-mode apheleia-mode-alist)
	'(fantomas))
  :init
  (enable-mode apheleia-global-mode))

;;; Python
;; Python is my main programming language, and one that I use for most of
;; my actual work. I have quite a few plugins and pieces that I configure for it.

(use-package python
  :hook
  (python-base-mode . python--setup-lsp-with-uv)
  (python-base-mode . lsp)
  (python-base-mode . electric-pair-local-mode)
  :init
  (defun python--setup-lsp-with-uv ()
    "Configure `lsp-mode' to recognize the current project's `.venv' file."
    (interactive)
    (let* ((project-dir (project-root (project-current nil)))
	   (venv-dir-name (concat project-dir ".venv")))
      (when project-dir
	(setq lsp-pylsp-plugins-jedi-environment venv-dir-name))))

  (transient-define-prefix uv ()
    "UV Transient Map (WIP)"
    ["Commands"
     ("s" "sync" uv--sync)
     ("a" "add" uv--add)
     ("d" "add dev" uv--add-dev)])

  (defun uv--sync (&optional args)
    (interactive (list (transient-args 'uv)))
    (shell-command "uv sync"))

  (defun uv--add (&optional args)
    (interactive (list (transient-args 'uv)))
    (shell-command (concat "uv add \"" (read-string "Package Name: ") "\"")))

  (defun uv--add-dev (&optional args)
    (interactive (list (transient-args 'uv)))
    (shell-command (concat "uv add --dev \"" (read-string "Package Name: ") "\"")))

  (with-eval-after-load 'python
    (bind-key "C-c p" 'uv python-base-mode-map))

  ;;; Django
  ;; I do a lot of Django dev, so I have a few helpers for Django development.

  (defun django-project-p ()
    "Check if the given project is a Django project."
    (and
     (project-current nil)
     (file-exists-p (concat (project-root (project-current nil)) "manage.py")))))

;;; JS/TS/Web
;; I do a lot of web programming as well, and this is my configuration
;; for it.

(use-package js2-mode
  :custom
  (js-indent-level 2)
  :hook
  (js-ts-mode . js2-minor-mode)
  (js-ts-mode . electric-pair-local-mode)
  (js-ts-mode . lsp)
  :mode ("\\.\\(c\\|m\\)?\\(t\\|j\\)s\\'" . js-ts-mode))

(use-package web-mode
  :hook
  (web-mode . web-mode--activate-django-engine-if-django-project)
  :mode
  (("\\.phtml\\'" . web-mode)
   ("\\.php\\'" . web-mode)
   ("\\.tpl\\'" . web-mode)
   ("\\.[agj]sp\\'" . web-mode)
   ("\\.as[cp]x\\'" . web-mode)
   ("\\.erb\\'" . web-mode)
   ("\\.mustache\\'" . web-mode)
   ("\\.djhtml\\'" . web-mode)
   ("\\.j2\\'" . web-mode)
   ("\\.njk\\'" . web-mode)
   ("\\.html\\'" . web-mode))
  :init
  (defun web-mode--activate-django-engine-if-django-project ()
    "Set the current engine as `django' if the project is a django project."
    (when (django-project-p)
      (setq-local web-mode-engine "django")
      (setq-local apheleia-formatter 'djade))))

(use-package emmet-mode
  :hook (web-mode . emmet-mode))


(add-hook 'scss-mode-hook #'lsp)

;;; F#
;; I am trying to learn F# for fun, so here's a basic setup for that.

(use-package fsharp-mode
  :hook
  (fsharp-mode . lsp)
  :config
  (defun ad--fsharp-find-sln-only (dir-or-file)
    (fsharp-mode-search-upwards (rx (0+ nonl) ".sln" eol)
				(file-name-directory dir-or-file)))
  (advice-add 'fsharp-mode/find-sln-or-fsproj :override #'ad--fsharp-find-sln-only))

;;; Elm
;; I dabble in Elm

(use-package elm-mode
  :hook
  (elm-mode . lsp))

;;; Zig

(use-package zig-mode
  :hook (zig-mode . lsp))

(use-package ob-zig
  :straight '(ob-zig :type git :host github :repo "jolby/ob-zig.el"))

;;; Emacs Lisp

(add-hook 'emacs-lisp-mode-hook #'electric-pair-local-mode)

;;;; Writing 
;;; Org
;; I write primarily in org-mode. It's powerful, its helpful, and it's always worth it.
;; I use a fairly vanilla setup, with some additional helpers.

(use-package olivetti
  :custom
  (olivetti-body-width 120)
  (olivetti-style nil))

(use-package org
  :bind (("C-c j" . counsel-org-capture)
	 ("C-c a" . org-agenda)
	 (:map org-mode-map
	       ("M-g i" . counsel-org-goto)))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . org-indent-mode)
  (org-mode . setup-org-faces-height)
  (org-mode . olivetti-mode)
  :init
  (require 'org-tempo)
  (require 'org-habit)
  (require 'org-timer)
  (defun setup-org-faces-height ()
    (set-face-attribute 'org-level-1 nil :height 1.6 :foreground (face-attribute 'lambda-green :foreground))
    (set-face-attribute 'org-level-2 nil :height 1.4 :foreground (face-attribute 'lambda-red :foreground))
    (set-face-attribute 'org-level-3 nil :height 1.2 :foreground (face-attribute 'lambda-aqua :foreground))
    (set-face-attribute 'org-level-4 nil :height 1.0)
    (set-face-attribute 'org-level-5 nil :height 1.0)
    (set-face-attribute 'org-level-6 nil :height 1.0)
    (set-face-attribute 'org-level-7 nil :height 1.0)
    (set-face-attribute 'org-level-8 nil :height 1.0)
    (set-face-attribute 'org-document-title nil :height 2.0 :foreground (face-attribute 'lambda-blue :foreground)))
  (setq org-directory "~/Dropbox/ZK/"
	org-agenda-files '("~/Dropbox/Events.org"
			   "~/Dropbox/CUNE-Cal.org"
			   "~/Dropbox/GCal.org"
			   "~/Dropbox/UNL-Cal.org"
			   "~/Dropbox/ZK/")
	org-agenda-file-regexp "\\`[^.].*agenda.*\\.org\\'"
	org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d!)" "CANCELLED(l!)")))

  (setq org-capture-templates
	'(("i" "Idea" entry (file "~/Dropbox/Intray.org")
	   "* %?\n" :empty-lines 1)))

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

(require 'org)

;;; Denote
;; I use denote extensively for work, research, project prep, etc.

;; (use-package denote
;;   :after org
;;   :bind (("C-c n f" . ivy-open-denote))
;;   :hook
;;   (dired-mode . denote-dired-mode)
;;   (org-mode . denote-rename-buffer-mode)
;;   :init
;;   (setq denote-directory org-directory
;; 	xref-search-program 'ripgrep)
;;   (defun denote-create-empty-note-and-link (title)
;;     "Create an empty note with the title of region. Then link it back to the original document."
;;     (interactive (list (cond
;; 			((use-region-p)
;; 			 (buffer-substring-no-properties (region-beginning) (region-end)))
;; 			(t
;; 			 (ivy-read "Title: ")))))
;;     (let ((current-buf (current-buffer))
;; 	  (note (denote title)))
;;       (with-current-buffer (get-file-buffer note)
;; 	(save-buffer)
;; 	(if (not current-prefix-arg)
;; 	    (kill-this-buffer)))
;;       (with-current-buffer current-buf
;; 	(denote-link note 'org (org-get-title note)))
;;       (if current-prefix-arg
;; 	  (switch-to-buffer (get-file-buffer note))
;; 	(switch-to-buffer current-buf))))
;;   (defun fast-read-org-titles (dir)
;;     "Use ripgrep to extract #+TITLE lines from .org files under DIR."
;;     (let ((default-directory dir))
;;       (mapcar
;;        (lambda (line)
;; 	 (when (string-match "^\\(.*\\.org\\):[^\n]*#\\+title:[[:space:]]*\\(.*\\)" line)
;;            (cons (expand-file-name (match-string 1 line) dir)
;; 		 (string-trim (match-string 2 line)))))
;;        (process-lines
;; 	"rg"
;; 	"--with-filename"       ;; show filename prefix
;; 	"--no-heading"          ;; one line per match
;; 	"--smart-case"
;; 	"--glob" "*.org"        ;; only org files
;; 	"^#\\+title:"           ;; match title line
;; 	"."))))                 ;; search from current directory

;;   (defun ivy-rich--denote-filename-transform (candidate)
;;     "Switch the filename to the denote title property."
;;     (with-current-buffer (find-file-noselect (concat counsel--fzf-dir "/" candidate))
;;       (cadar (org-collect-keywords '("TITLE")))))


;;   (defun ivy-rich--denote-keyword-transform (candidate)
;;     "Grab the keywords."
;;     (string-replace "_" ", " (car (string-split (cadr (string-split (cadr (string-split candidate "--")) "__")) ".org"))))

;;   (defun denote-timestamp-to-time (timestamp)
;;     "Convert a Denote TIMESTAMP string like '20250401T105148' to an Emacs time object."
;;     (let ((year   (string-to-number (substring timestamp 0 4)))
;;           (month  (string-to-number (substring timestamp 4 6)))
;;           (day    (string-to-number (substring timestamp 6 8)))
;;           (hour   (string-to-number (substring timestamp 9 11)))
;;           (minute (string-to-number (substring timestamp 11 13)))
;;           (second (string-to-number (substring timestamp 13 15))))
;;       (encode-time second minute hour day month year)))

;;   (defun ivy-rich--denote-created-at-transform (candidate)
;;     "Extract a pretty date."
;;     (format-time-string "%b %d, %Y" (denote-timestamp-to-time (car (string-split candidate "--")))))

;;   (push
;;    '(:columns
;;      ((nerd-icons-ivy-rich-file-icon)
;;       (ivy-rich--denote-filename-transform (:width 0.5))
;;       (ivy-rich--denote-keyword-transform (:width 0.35))
;;       (ivy-rich--denote-created-at-transform (:width 0.15))))
;;    ivy-rich-display-transformers-list)
;;   (push 'ivy-open-denote ivy-rich-display-transformers-list)
;;   (ivy-rich-reload)

;;   (defun ivy-open-denote ()
;;     "Open a note using a rich ivy interface."
;;     (interactive)
;;     (let* ((directory (if current-prefix-arg
;; 			  (ivy-completing-read "Silo: " denote-silo-directories)
;; 			denote-directory))
;; 	   (counsel--fzf-dir directory))
;;       (with-environment-variables
;; 	  (("FZF_DEFAULT_COMMAND" "fd --type f -e org"))
;; 	(ivy-read "Open Note: "
;; 		  #'counsel-fzf-function
;; 		  :re-builder #'ivy--regex-fuzzy
;; 		  :dynamic-collection t
;; 		  :action (lambda (x)
;; 			    (with-ivy-window
;; 			      (let ((default-directory counsel--fzf-dir))
;; 				(when (bufferp x) (kill-buffer x))
;; 				(find-file x)))
;; 			    :caller 'ivy-open-denote))))))

;; (use-package denote-silo
;;   :after denote
;;   :bind (("C-c n d" . my/denote)
;; 	 ("C-c n j" . counsel-denote-silo-dired)
;; 	 (:map org-mode-map
;; 	       ("C-'" . avy-dwim)
;; 	       ("C-c C-x C-d" . denote-link)))
;;   :init
;;   (setq denote-silo-directories (list denote-directory
;; 				      "~/Dropbox/Work/UNL/"
;; 				      "~/Dropbox/Work/CUNE/"))

;;   (defun ivy-rich--denote-silo-extract-path (candidate)
;;     (car (reverse (butlast (string-split candidate "/")))))

;;   (defun ivy-rich--denote-silo-description (candidate)
;;     (cond
;;      ((equal candidate denote-directory)
;;       "Personal/Work")
;;      ((equal candidate "~/Dropbox/Work/UNL/")
;;       "Research Work")
;;      ((equal candidate "~/Dropbox/Work/CUNE/")
;;       "Concordia Work")
;;      (t
;;       "Other")))

;;   (push
;;    '(:columns
;;      ((nerd-icons-ivy-rich-file-icon)
;;       (ivy-rich--denote-silo-extract-path (:width 0.15))
;;       (ivy-rich--denote-silo-description (:width 0.8))))
;;    ivy-rich-display-transformers-list)
;;   (push 'counsel-denote-silo-dired ivy-rich-display-transformers-list)
;;   (ivy-rich-reload)

;;   (defun counsel-denote-silo-dired ()
;;     (interactive)
;;     (ivy-read "Silo: "
;; 	      denote-silo-directories
;; 	      :re-builder #'ivy--regex-fuzzy
;; 	      :action (lambda (x)
;; 			(unless (featurep 'denote-silo) (require 'denote-silo))
;; 			(denote-silo-with-silo x
;; 					       (dired x)))))

;;   (defun my/denote (&optional choose-silo)
;;     "Wrapper for denote that can switch silos based on `current-prefix-arg'."
;;     (interactive "P")
;;     (call-interactively (if choose-silo #'denote-silo-create-note #'denote))))

;;; Org Roam

(use-package org-roam
  :bind (("C-c n f" . org-roam-node-find)
	 (:map org-mode-map
	       ("C-c i" . org-roam-node-insert)))
  :custom
  (org-roam-directory (file-truename "~/Documents"))
  :init
  (org-roam-db-autosync-mode 1))

;;; Citar
;; I use citar to manage all my bibliographic needs.

(use-package citar
  :custom
  (org-cite-global-bibliography '("~/Dropbox/ZK/References.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography))

(use-package citar-org-roam
  :after (citar org-roam)
  :config (citar-org-roam-mode))

;; (use-package citar-denote
;;   :after (:all denote citar)
;;   :custom
;;   (citar-denote-file-type 'org)
;;   (citar-denote-keyword "bib")
;;   (citar-denote-signature nil)
;;   (citar-denote-subdir nil)
;;   (citar-denote-template nil)
;;   (citar-denote-title-format "title")
;;   (citar-denote-title-format-andstr "and")
;;   (citar-denote-use-bib-keywords nil)
;;   :init
;;   (enable-mode citar-denote-mode)
;;   ;; Enables Zotero links to work correctly.
;;   (org-link-set-parameters
;;    "zotero"
;;    :follow
;;    (lambda (path _)
;;      (call-process "xdg-open" nil nil nil (concat "zotero:" path)))))

;;; Org Modern
;; I like my Org /pretty/

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  :custom
  (org-modern-replace-stars "12345678")
  (org-modern-star 'replace)
  (org-modern-keyword '(("title" . "Τ")
			("author" . "Α")
			("date" . "Δ")
			("theme" . "τ")
			("identifier" . "Ι")
			("references" . "Ρ")
			("filetags" . "ϕ")
			(t . t))))

;;; Spell Check

(use-package jinx
  :bind (("M-$" . jinx-correct))
  :hook
  (text-mode . jinx-mode)
  :config
  (defun ivy-jinx-correct-select ()
    "Fix the bug when using `jinx-correct-select' in ivy."
    (interactive)
    (let* ((keys (this-command-keys-vector))
	   (word (nth (if (eq (aref keys 0) ?0)
			  (+ 9 (or (seq-position jinx--select-keys (aref keys 1)) 999))
			(- (aref keys 0) ?1))
		      (all-completions "" minibuffer-completion-table))))
      (unless (and word (get-text-property 0 'jinx--prefix word))
	(user-error "Invalid select key `%s'" (key-description keys)))
      (delete-minibuffer-contents)
      (ivy--done word)))
  (advice-add 'jinx-correct-select :override #'ivy-jinx-correct-select))

;;; AucTeX

(use-package pdf-tools
  :magic ("%PDF" . pdf-view-mode)
  :init
  (pdf-tools-install :no-query))

(use-package auctex
  :hook
  (LaTeX-mode . TeX-fold-mode)
  (LaTeX-mode . TeX-source-correlate-mode)
  (LaTeX-mode . olivetti-mode)
  (LaTeX-mode . jinx-mode)
  (LaTeX-mode . setup-pdf-view)
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  :init
  (defun setup-pdf-view ()
    (setq-local display-buffer-alist
		(append
		 display-buffer-alist
		 (list `(,(concat (file-name-base (buffer-file-name)) ".pdf")
			 (display-buffer-in-side-window)
			 (side . right)
			 (window-width . 0.5))))))
  (defun narrow-to-section ()
    "Narrow to the current `LaTeX' section."
    (interactive)
    (LaTeX-mark-section)
    (call-interactively #'narrow-to-region)
    (deactivate-mark t)))

(use-package reftex
  :after (:all auctex citar)
  :hook
  (LaTeX-mode . turn-on-reftex)
  (LaTeX-mode . activate-local-bib-if-tex)
  :custom
  (reftex-plug-into-AUCTeX t)
  :init
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

;;;; Quick Access (Harpoon)



(provide 'init)
;;; init.el ends here
