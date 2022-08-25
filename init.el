;; Copyright © 2021 Yevhenii Kolesnikov
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; --------------------------------
;; Package system
;; --------------------------------

;; Add package system
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  (add-to-list 'package-archives
               '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(let ((default-directory (expand-file-name "lisp" user-emacs-directory)))
  (normal-top-level-add-subdirs-to-load-path))

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --------------------------------
;; Basic setup
;; --------------------------------

;; Set some defaults
(setq-default
 indent-tabs-mode          nil          ;Use spaces instead of tabs
 tab-width                 4            ;Tab width
 scroll-step               1            ;Scroll step
 fill-column               80           ;Page width
 select-enable-clipboard   t            ;Merge system and emacs clipboard
 x-stretch-cursor          t            ;Stretch cursor
 sentence-end-double-space nil          ;Sane sentence end
 delete-by-moving-to-trash t            ;Deleted files go to rubbish bin
 visible-bell              t            ;Flash instead of beeping
 use-package-always-ensure t            ;No reason not to
 ;; C style
 c-default-style           "k&r"        ;Style
 c-basic-indent            3            ;As in Mesa
 c-basic-offset            3            ;As in Mesa
 )

(tool-bar-mode 0)                       ;Disable toolbar
(menu-bar-mode 0)                       ;Disable menubar
(scroll-bar-mode 0)                     ;Disable scroll bar
(blink-cursor-mode 0)                   ;Stop blinking cursor
(fringe-mode '(8 . 0))                  ;Display fringe
(if (< emacs-major-version 26)
    (global-linum-mode 1)
  (global-display-line-numbers-mode t)) ;Show line numbers
(show-paren-mode 1)                     ;Highlight pair brackets
(global-visual-line-mode 1)             ;Wrap the line near the screen edge
(global-hl-line-mode 1)                 ;Highlight current line
(electric-pair-mode 1)                  ;Autocomplete paired characters
(size-indication-mode t)                ;Show file size in modeline
(fset 'yes-or-no-p 'y-or-n-p)           ;y/n instead of yes/no
(windmove-default-keybindings 'super)   ;Navigate windows with super
(set-default-coding-systems 'utf-8)     ;Default to UTF-8

;; Don't pollute the modeline's minor mode list
(use-package diminish)

(diminish 'visual-line-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;; Smoother scrolling
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(3 ((shift) . 3)) ;Three lines at a time
        mouse-wheel-progressive-speed nil            ;Don't accelerate
        mouse-wheel-follow-mouse t))                 ;Scroll under mouse
(setq scroll-step 1                                  ;Keyboard scrolling
      scroll-margin 0                                ;Scroll at the edge
      scroll-conservatively 101)                     ;No weird scrolling

;; Experimental
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; Change default backup directory
(setq user-backup-directory (concat user-emacs-directory "emacs-backups/"))
(unless (file-exists-p user-backup-directory)
  (make-directory user-backup-directory))
(setq backup-directory-alist `((".*" . ,user-backup-directory)))

;; Change default autosave directory
(setq user-autosave-directory (concat user-emacs-directory "emacs-autosave/"))
(unless (file-exists-p user-autosave-directory)
  (make-directory user-autosave-directory))
(setq auto-save-file-name-transforms `((".*" ,user-autosave-directory t)))

;; Change default custom-file
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Because I'm running emacs as a server, starting it with a systemd
;; unit, it doesn't get a chance to read environment variables from my
;; .zshenv. Thus, I have to use this dirty hack.
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; Scroll a line without touching a mouse
(global-set-key (kbd "C-s-n") (lambda () (interactive) (scroll-up-line 1)))
(global-set-key (kbd "C-s-p") (lambda () (interactive) (scroll-down-line 1)))

;; Don't preserve font on copy
(add-to-list 'yank-excluded-properties 'font)
(add-to-list 'yank-excluded-properties 'face)

;; --------------------------------
;; Appearance
;; --------------------------------

(use-package all-the-icons
  :diminish)

;; Dashboard
(use-package dashboard
  :diminish
  :init
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))
        dashboard-items '((recents  . 5)  ;Recent files
                          (projects . 5)) ;Open projects
        dashboard-set-heading-icons t     ;Heading icons
        dashboard-set-file-icons t        ;File icons
        dashboard-set-init-info t         ;Emacs started in ...
        dashboard-startup-banner 'logo    ;Nicer logo
        dashboard-set-navigator t         ;Buttons under the logo
        dashboard-navigator-buttons `(((,(all-the-icons-octicon "gear"
                                                                :height 1.0
                                                                :v-adjust 0.0)
                                        "init.el"
                                        "Edit initialisation file"
                                        (lambda (&rest _)
                                          (open-user-init-file))))))
  :config
  (dashboard-setup-startup-hook))

;; Modeline
(use-package powerline
  :config
  (powerline-center-theme))

;; Colour theme
(use-package twilight-theme
  :config
  (load-theme 'twilight))

(set-face-background 'hl-line "#202020")
(set-face-foreground 'highlight nil)

;; Highlight matching parens
(use-package paren
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (with-no-warnings
    ;; Display matching line for off-screen paren.
    (defun display-line-overlay (pos str &optional face)
      "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
      (let ((ol (save-excursion
                  (goto-char pos)
                  (make-overlay (line-beginning-position)
                                (line-end-position)))))
        (overlay-put ol 'display str)
        (overlay-put ol 'face
                     (or face '(:inherit highlight)))
        ol))

    (defvar-local show-paren--off-screen-overlay nil)
    (defun show-paren-off-screen (&rest _args)
      "Display matching line for off-screen paren."
      (when (overlayp show-paren--off-screen-overlay)
        (delete-overlay show-paren--off-screen-overlay))
      ;; Check if it's appropriate to show match info,
      (when (and (overlay-buffer show-paren--overlay)
                 (not (or cursor-in-echo-area
                          executing-kbd-macro
                          noninteractive
                          (minibufferp)
                          this-command))
                 (and (not (bobp))
                      (memq (char-syntax (char-before)) '(?\) ?\$)))
                 (= 1 (logand 1 (- (point)
                                   (save-excursion
                                     (forward-char -1)
                                     (skip-syntax-backward "/\\")
                                     (point))))))
        ;; Rebind `minibuffer-message' called by `blink-matching-open'
        ;; to handle the overlay display.
        (cl-letf (((symbol-function #'minibuffer-message)
                   (lambda (msg &rest args)
                     (let ((msg (apply #'format-message msg args)))
                       (setq show-paren--off-screen-overlay
                             (display-line-overlay
                              (window-start) msg 'popup-tip-face))))))
          (blink-matching-open))))
    (advice-add #'show-paren-function :after #'show-paren-off-screen)))

;; Fonts
;; (set-default-font "Inconsolata LGC-12")
(add-to-list 'default-frame-alist
             '(font . "Inconsolata LGC-14"))

;; --------------------------------
;; Handy stuff
;; --------------------------------

;; Copy filepath to the clipboard
(defun copy-filepath ()
 "Copy the current buffer file name to the clipboard"
 (interactive)
 (let ((filename (if (equal major-mode 'dired-mode)
                     default-directory
                   (buffer-file-name))))
   (when filename
     (kill-new filename)
     (message "Copied '%s" filename))))

(global-set-key (kbd "s-c f") 'copy-filepath)

;; Copy filepath and line number to the clipboard
(defun copy-filepath-and-linum ()
 "Copy the current buffer file name to the clipboard along with
 active line number"
 (interactive)
 (let ((filename-linum (concat (if (equal major-mode 'dired-mode)
                                   default-directory
                                 (buffer-file-name))
                               ":"
                               (number-to-string (line-number-at-pos)))))
   (when filename-linum
     (kill-new filename-linum)
     (message "Copied '%s" filename-linum))))

(global-set-key (kbd "s-c l") 'copy-filepath-and-linum)

;; Fill-sentence
(defun fill-sentence ()
  (interactive)
  ;; optional 
  ;; (save-excursion
  (backward-sentence)
  (push-mark)
  (forward-sentence)
  (fill-region (point) (mark))
  ;; )
  )

(global-set-key (kbd "s-q") 'fill-sentence)

;; Setup russian input method and immediately switch back to english
;; Input method then can be toggled with C-\
;; Compared to system-set keyboard layout, this has the advantage of emacs
;; keybings working properly regardless of current input method.
(set-input-method 'russian-computer)
(toggle-input-method)

;; Open init file
(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))

(global-set-key (kbd "s-x i") 'open-user-init-file)

;; --------------------------------
;; Interaction
;; --------------------------------

;; Dired
(use-package dired-open
  :after (dired)
  :bind
  (("<f8>"    . dired-other-window)  ;Start dired
   ("M-RET"   . dired-open-xdg)      ;Open with xdg-open
   :map dired-mode-map
   ("<prior>" . dired-up-directory)) ;Use PageUp to navigate up
  :custom
  ;; Options for ls
  (dired-listing-switches "-lhGA --group-directories-first")
  ;; Open with external app if not subdir
  (dired-open-functions '(dired-open-by-extension dired-open-subdir)))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Colourful dired
(use-package diredfl
  :init (diredfl-global-mode 1))

;; Show help after hitting prefix
(use-package which-key
  :diminish
  :config
  (setq which-key-idle-delay 0.5)       ;Reduce delay
  (which-key-mode))

;; Automatically reload files modified by external program
(use-package autorevert
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Multiple cursors
(use-package multiple-cursors
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-."     . mc/mark-pop)))

;; Fancier Completion within emacs minibuffer
(use-package counsel
  :diminish
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :diminish)

(use-package rg
  :bind
  (("C-c r r" . rg-project)))

(use-package swiper
  :diminish
  :bind (("C-s"     . swiper)
         ("C-r"     . swiper)
         ("C-c C-r" . ivy-resume)
         ("C-c C-s" . counsel-rg)
         ("M-x"     . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-h f"   . counsel-describe-function)
         ("C-h v"   . counsel-describe-variable)
         ("<f2> u"  . counsel-unicode-char))
  :config
  (progn
    ;; Enable interactive completion
    (ivy-mode 1)
    (counsel-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)))

;; Fancier window navigation
(use-package ace-window
  :init
  (progn
    ;; Number windows when "C-x o"
    (global-set-key [remap other-window] 'ace-window)
    ;; Enlarge font
    (custom-set-faces
     '(aw-leading-char-face
       ((t (:inherit ace-jump-head-foreground :height 2.0)))))))

;; Undo tree
(use-package undo-tree
  :diminish
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

(use-package goto-line-preview
  :bind ([remap goto-line] . goto-line-preview))

;; Highlight lines that are too long and more
(use-package whitespace
  :diminish
  :config
  ;; (global-whitespace-mode 1)
  :custom
  (whitespace-style '(face empty tabs lines-tail trailing))
  (whitespace-line-column 78))

(use-package unicode-whitespace)

;; Expand selection to word, sentance, etc
(use-package expand-region
  :config
  :bind (("C-=" . er/expand-region)))

;; Iedit
;; Allows to edit multiple occurrences of a word
(use-package iedit)

;; Syntax checking
(use-package flycheck
  ;; :init
  ;; (global-flycheck-mode t)
  )

;; Spell checking
(use-package flyspell)

;; Use Perl Compatible Regular Expressions
(use-package pcre2el
  :diminish pcre-mode
  :config
  (pcre-mode))

;; Expand abbriviation to template
(use-package yasnippet
  :diminish yas-minor-mode
  :init
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode 1))

;; A collection of snippets
(use-package yasnippet-snippets
  :diminish)

;; Fancier buffer list
(defalias 'list-buffers 'ibuffer-other-window)

;; With sections for different categories of buffers
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("Dired" (mode . dired-mode))
               ("org" (mode . org-mode))
               ("TeX" (name . "^.*tex$"))
               ("shell" (or (mode . eshell-mode)
                            (mode . shell-mode)
                            (mode . term-mode)))
               ("Code" (or (mode . c-mode)
                           (mode . haskell-mode)
                           (mode . lisp-mode)
                           (mode . racket-mode)
                           (mode . perl-mode)))
               ("Emacs" (or (mode . emacs-lisp-mode)))))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-auto-mode 1)
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Magit
;; Graphical git interface
(use-package magit
  :bind (("C-c m" . magit-status)))

(use-package git-messenger
  :config
  (setq git-messenger:use-magit-popup t)
  :bind
  (("C-c M-b" . git-messenger:popup-message)))

;; Rectangle selection
;; Press C-RET to start
(use-package cua-base
  :config (cua-selection-mode 1))

;; Needs to be checked
(use-package projectile
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t
        projectile-mode-line-prefix "Proj")
  :bind (("C-c M-f" . projectile-switch-project)
         ("C-c C-f" . projectile-find-file)
         ("C-c C-g" . projectile-grep)))

(use-package counsel-projectile
  :init
  (counsel-projectile-mode))

;; (use-package ggtags
;;   :ensure t
;;   :init
;;   :bind (("C-c C" . ggtags-create-tags)
;;          ("C-c U" . ggtags-update-tags)
;;          ("C-c D" . ggtags-delete-tags)
;;          ("C-c j" . ggtags-find-definition)
;;          ("C-c s" . ggtags-show-definition)
;;          ("C-c J" . ggtags-find-tag-dwim)
;;          ("C-c >" . ggtags-find-tag-continue)
;;          ("C-c C-f" . ggtags-find-file)))

(use-package diffview
  :init)

(use-package treemacs
  :config
  (setq treemacs-position 'right)
  :bind
  (("s-t" . treemacs)))

(use-package company
  :diminish
  :custom
  (company-dabbrev-downcase 0)
  (company-idle-delay 0))

(use-package lsp-mode
  :commands lsp)

(use-package lsp-treemacs
  :after lsp-mode
  :bind (("s-l e" . lsp-treemacs-errors-list)
         ("s-l S" . lsp-treemacs-symbols)))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package ccls
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) .
   (lambda () (require 'ccls) (lsp))))

(use-package ivy-xref
  :diminish)

(use-package helm-dash)

(use-package org-superstar
  :hook (org-mode . org-superstar-mode))

;; Looking up a word in a dictionary
(use-package lexic
  :bind
  (("M-s M-l RET" . lexic-search-word-at-point)
   ("M-s M-l /"   . lexic-search)))

(use-package shell-pop
  :custom
    (shell-pop-universal-key "H-t")
    (shell-pop-window-position "bottom")
    (shell-pop-term-shell "/bin/zsh"))

(use-package apostil)

;; --------------------------------
;; Modes
;; --------------------------------

(use-package haskell-mode)

(use-package pandoc-mode)

(use-package markdown-mode)

(use-package yaml-mode)

(use-package ninja-mode)

(use-package meson-mode)

(use-package glsl-mode
  :bind (("C-c h" . glsl-find-man-page)))

(use-package cmake-mode)

(use-package pkgbuild-mode)

(use-package bison-mode)

(use-package csv-mode)

(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package systemd)

(use-package gitignore-mode)

;; --------------------------------
;; Garbage
;; --------------------------------

;; C++ comment style
(defun c++-comment-style-hook-2 ()
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */"))
(add-hook 'c++-mode-hook 'c++-comment-style-hook-2)
