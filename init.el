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
(diminish 'visual-line-mode)
(diminish 'eldoc-mode)
(diminish 'abbrev-mode)

;; Smoother scrolling
(when (display-graphic-p)
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1))
        mouse-wheel-progressive-speed nil))
(setq scroll-step 1
      scroll-margin 0
      scroll-conservatively 100000)

;; Experimental
(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")

;; Disable automatic indention
;;(electric-indent-mode -1)

;; Three lines at a time with wheel scrolling
;; (setq mouse-wheel-scroll-amount '(3 ((shift) . 3)))

;; Don't accelerate wheel scrolling
;; (setq mouse-wheel-progressive-speed nil)

;; Scroll window under mouse
;; (setq mouse-wheel-follow-mouse 't)

;; Change default backup directory
(setq backup-directory-alist
      `((".*" . ,(concat user-emacs-directory "emacs-backups/"))))
;; Change default autosave directory
(setq auto-save-file-name-transforms
      `((".*" ,(concat user-emacs-directory "emacs-autosave/") t)))
;; Change default custom-file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file)

;; Because I'm running emacs as a server, starting it with a systemd
;; unit, it doesn't get a chance to read environment variables from my
;; .zshenv. Thus, I have to use this dirty hack.
(use-package exec-path-from-shell
  :ensure t)
(exec-path-from-shell-initialize)

;; Do not use local variables (directoty and file)
;; (setq-default enable-local-variables nil)

;; Set bookmark
;; (global-set-key (kbd "<f5>") 'bookmark-set)

;; Load bookmark
;; (global-set-key (kbd "<f6>") 'bookmark-jump)

;; Save buffer
;; (global-set-key (kbd "<f12>") 'save-buffer)

;; Insert current date and time
;; (defun insert-current-date () (interactive)
       ;; (insert (shell-command-to-string "echo -n $(date +%d.%m.%Y)")))
;; (defun insert-current-time () (interactive)
       ;; (insert (shell-command-to-string "echo -n $(date +%H:%M)")))

;; Insert current date
;; (global-set-key (kbd "C-c d") 'insert-current-date)

;; Insert current time
;; (global-set-key (kbd "C-c t") 'insert-current-time)

;; Revert buffer
;; (global-set-key (kbd "C-<f5>") 'revert-buffer)

;; Goto line
;; (global-set-key (kbd "<f7>") 'goto-line)

;; Open thin horizontal buffer
;; (global-set-key (kbd "C-x 4") (kbd "C-x 2 C-x o C-x 2 C-x 0"))

;; Open thin vertical buffer
;; (global-set-key (kbd "C-x 5") (kbd "C-x 3 C-x o C-x 3 C-x 0"))

;; Open terminal
;; (global-set-key (kbd "<XF86Favorites>") (lambda () (interactive) (ansi-term "/usr/bin/zsh")))

;; Reload .emacs
;; Need fixing
;; (global-set-key (kbd "M-<Scroll_Lock>") (load-file "~/.emacs"))

(global-set-key (kbd "C-s-v") (lambda () (interactive) (scroll-up-line 1)))
(global-set-key (kbd "M-s-v") (lambda () (interactive) (scroll-down-line 1)))

(global-set-key (kbd "C-s-n") (lambda () (interactive) (scroll-up-line 1)))
(global-set-key (kbd "C-s-p") (lambda () (interactive) (scroll-down-line 1)))

;; --------------------------------
;; Appearance
;; --------------------------------

(use-package all-the-icons
  :ensure t
  :diminish)

;; Dashboard
(use-package dashboard
  :ensure t
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
  :ensure t
  :config
  (powerline-center-theme))

;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))

;; Colour theme
(use-package twilight-theme
  :ensure t
  :config
  (load-theme 'twilight))

;; Doom modeline
;; (use-package doom-modeline
;;   :ensure t
;;   :config
;;   (doom-modeline-mode t))

;; (use-package hl-line
;;   :ensure t
;;   :hook ((after-init . global-hl-line-mode)))

(set-face-background 'hl-line "#202020")
(set-face-foreground 'highlight nil)

;; Highlight matching parens
(use-package paren
  :ensure t
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

;; (use-package diff-hl
;;   :ensure t
;;   :custom-face
;;   (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
;;   (diff-hl-insert ((t (:background nil))))
;;   (diff-hl-delete ((t (:background nil))))
;;   :bind (:map diff-hl-command-map
;;          ("SPC" . diff-hl-mark-hunk))
;;   :hook ((after-init . global-diff-hl-mode)
;;          (dired-mode . diff-hl-dired-mode))
;;   :init (setq diff-hl-draw-borders nil)
;;   :config
;;   ;; Highlight on-the-fly
;;   (diff-hl-flydiff-mode 1)

;;   ;; Set fringe style
;;   (setq-default fringes-outside-margins t)

;;   (with-no-warnings
;;     (defun my-diff-hl-fringe-bmp-function (_type _pos)
;;       "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
;;       (define-fringe-bitmap 'my-diff-hl-bmp
;;         (vector (if sys/macp #b11100000 #b11111100))
;;         1 8
;;         '(center t)))
;;     (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

;;     (unless (display-graphic-p)
;;       (setq diff-hl-margin-symbols-alist
;;             '((insert . " ") (delete . " ") (change . " ")
;;               (unknown . " ") (ignored . " ")))
;;       ;; Fall back to the display margin since the fringe is unavailable in tty
;;       (diff-hl-margin-mode 1)
;;       ;; Avoid restoring `diff-hl-margin-mode'
;;       (with-eval-after-load 'desktop
;;         (add-to-list 'desktop-minor-mode-table
;;                      '(diff-hl-margin-mode nil))))

;;     ;; Integration with magit
;;     (with-eval-after-load 'magit
;;       (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
;;       (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))))

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
  :ensure t
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

(use-package dired-git-info
  :ensure t
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; Colourful dired
(use-package diredfl
  :ensure t
  :init (diredfl-global-mode 1))

;; List of extensions and corresponding programs
;; (load "~/.emacs.d/dired-open-extensions.el")

;; Show help after hitting prefix
(use-package which-key
  :ensure t
  :diminish
  :config (which-key-mode))

;; (use-package org-mode
  ;; :ensure t
  ;; :bind
;; (("<Scroll-Lock>" . org-time-stamp-inactive)))

;; Automatically reload files modified by external program
(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

;; Multiple cursors
(use-package multiple-cursors
  :ensure t
  :bind
  (("C->"     . mc/mark-next-like-this)
   ("C-<"     . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this)
   ("C-."     . mc/mark-pop)))

;; Fancier Completion within emacs minibuffer
(use-package counsel
  :ensure t
  :diminish
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)))

(use-package ivy
  :ensure t
  :diminish)

(use-package swiper
  :ensure t
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
  :ensure t
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
  :ensure t
  :diminish
  :init
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-timestamps t)
  (with-no-warnings
    (make-variable-buffer-local 'undo-tree-visualizer-diff)
    (setq-default undo-tree-visualizer-diff t)))

(use-package goto-line-preview
  :ensure t
  :bind ([remap goto-line] . goto-line-preview))

;; Delete all the whitespaces
;; (use-package hungry-delete
;;   :ensure
;;   :config
;;   (global-hungry-delete-mode))

;; Highlight lines that are too long and more
(use-package whitespace
  :ensure
  :diminish
  :config
  ;; (global-whitespace-mode 1)
  :custom
  (whitespace-style '(face empty tabs lines-tail trailing))
  (whitespace-line-column 78))

(use-package unicode-whitespace
  :ensure t)

;; Expand selection to word, sentance, etc
(use-package expand-region
  :ensure
  :config
  :bind (("C-=" . er/expand-region)))

;; Iedit
;; Allows to edit multiple occurrences of a word
(use-package iedit
  :ensure)

;; Syntax checking
(use-package flycheck
  :ensure t
  ;; :init
  ;; (global-flycheck-mode t)
  )

;; Spell checking
(use-package flyspell
  :ensure t)

;; Use Perl Compatible Regular Expressions
(use-package pcre2el
  :ensure t
  :diminish
  :config
  (pcre-mode)
  (diminish 'pcre-mode))

;; Auto completition
;; (require 'auto-complete)
;; (require 'auto-complete-config)
;; (ac-config-default)

;; Expand abbriviation to template
(use-package yasnippet
  :ensure t
  :diminish
  :init
  (setq yas-prompt-functions '(yas-ido-prompt))
  (yas-global-mode 1)
  (diminish 'yas-minor-mode))

;; A collection of snippets
(use-package yasnippet-snippets
  :ensure t
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
               ("Programming" (or (mode . c-mode)
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
  :ensure t
  :bind (("C-c m" . magit-status)))

(use-package git-messenger
  :ensure t)

;; Rectangle selection
;; Press C-RET to start
(use-package cua-base
  :ensure t
  :config (cua-selection-mode 1))

;; Needs to be checked
(use-package projectile
  :ensure t
  :init
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  :bind (("C-c M-f" . projectile-switch-project)
         ("C-c C-f" . projectile-find-file)
         ("C-c C-g" . projectile-grep)))

(use-package counsel-projectile
  :ensure t
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
  :ensure t
  :init)

(use-package company
  :ensure t
  :diminish
  :custom
  (company-dabbrev-downcase 0)
  (company-idle-delay 0))

(use-package lsp-mode
  :ensure t
  :commands lsp)

(use-package lsp-treemacs
  :ensure t
  :after lsp-mode
  :bind (("s-l e" . lsp-treemacs-errors-list)
         ("s-l S" . lsp-treemacs-symbols)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package dap-mode
  :ensure t)

(use-package company-lsp
  :ensure t
  :commands company-lsp)

(use-package ccls
  :hook
  ((c-mode c++-mode objc-mode cuda-mode) .
   (lambda () (require 'ccls) (lsp))))

(use-package ivy-xref
  :ensure t
  :diminish)

(use-package helm-dash
  :ensure t)

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; --------------------------------
;; Modes
;; --------------------------------

(use-package haskell-mode
  :ensure t)

(use-package pandoc-mode
  :ensure t)

(use-package markdown-mode
  :ensure t)

(use-package yaml-mode
  :ensure t)

(use-package ninja-mode
  :ensure t)

(use-package meson-mode
  :ensure t)

(use-package glsl-mode
  :ensure t
  :bind (("C-c h" . glsl-find-man-page)))

(use-package cmake-mode
  :ensure t)

(use-package pkgbuild-mode
  :ensure t)

(use-package bison-mode
  :ensure t)

(use-package csv-mode
  :ensure t)

(use-package rust-mode
  :hook (rust-mode . lsp))

(use-package systemd
  :ensure t)

(use-package gitignore-mode
  :ensure t)

;; --------------------------------
;; Garbage
;; --------------------------------

;; C++ comment style
(defun c++-comment-style-hook-2 ()
  (set (make-local-variable 'comment-start) "/* ")
  (set (make-local-variable 'comment-end)   " */"))
(add-hook 'c++-mode-hook 'c++-comment-style-hook-2)


;;C Headers
;; (defun own:ac-c-header-init ()
  ;; (require 'auto-complete-c-headers)
  ;; (add-to-list 'ac-sources 'ac-source-c-headers))

;; (add-hook 'c++-mode-hook 'own:ac-c-header-init)
;; (add-hook 'c-mode-hook 'own:ac-c-header-init)

;;CEDIT

;;Turn on semantic
;; (semantic-mode 1)
;; (defun own:add-semantic-to-autocomplete ()
  ;; (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-common-hook 'own:add-semantic-to-autocomplete)

;; Bind some CEDIT keys
;; (defun own:cedit-hook ()
;;   Jump to definition
;;   (local-set-key (kbd "C-c b") 'semantic-ia-fast-jump)
;;   Show summury
;;   (local-set-key (kbd "C-c s") 'semantic-ia-show-summary)
;;   Show documentation
;;   (local-set-key (kbd "C-c i") 'semantic-ia-show-doc)
;;   Jump between the declaration and implementation of teh function
;;   (local-set-key (kbd "C-c C-j") 'semantic-analyze-proto-impl-toggle)
;;   Jump to variable/function
;;   (local-set-key (kbd "C-c C-k") 'semantic-complete-jump))
;; (add-hook 'c-mode-common-hook 'own:cedit-hook)

;;Use CEDIT as a source for autocompletion
;; (defun own:c-mode-cedit-hook ()
  ;; (add-to-list 'ac-sources 'ac-source-gtags)
  ;; (add-to-list 'ac-sources 'ac-source-semantic))
;; (add-hook 'c-mode-hook 'own:c-mode-cedit-hook)

;;Automatic reparsing open buffers
;;(globsal-semantic-idle-scheduler-mode 1)

;; Own haskell-hook
;; (defun own:haskell-hook ()
  ;;Start with interactive-haskell-mode
  ;; (interactive-haskell-mode t))
;; (add-hook 'haskell-mode-hook 'own:haskell-hook)

;;TODO
;;Symbol summary

;; (add-to-list 'auto-mode-alist '("\\.wiki\\'" . creole-mode))

;;LaTeX mode forjjj all *.tex files
;; (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))

;;Autocomplete in LaTeX-mode
;; (add-hook 'LaTeX-mode-hook
;;           (lambda()
;;             (local-set-key (kbd "<C-tab>") 'TeX-complete-symbol)))

;;Common Lisp
;; (require 'cl)
;; (setq-default inferior-lisp-program "sbcl")

;;Slime
;; (require 'slime)
;; (require 'slime-autoloads)
;; (slime-setup '(slime-asdf
               ;; slime-fancy
               ;; slime-indentation))
;; (setq-default slime-net-coding-system 'utf-8-unix)

;; (require 'ox-mediawiki)

;; (setq python-shell-interpreter "python3")
