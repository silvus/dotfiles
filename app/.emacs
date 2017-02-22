(set-language-environment "UTF-8")

;; Packages
;; -------------------------------------------------------------------------------

;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Markdown
(use-package markdown-mode)
;; PHP
(use-package php-mode)
;; Html / JS
(use-package web-mode)
;; Python
(use-package elpy
	     :config
	     (progn
	       (elpy-enable)
	       (setq elpy-rpc-python-command "/usr/bin/python3")
	       (add-hook 'python-mode-hook (highlight-indentation-mode 0))))
;; Betters commands
(use-package smex
	     :config
	     (progn
	       (smex-initialize)
	       (global-set-key (kbd "M-x") 'smex)))
;; Autocomplete
(use-package auto-complete
	     :config
	     (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))
(use-package auto-complete-config
	     :config
	     (ac-config-default))
;; Undo
(use-package undo-tree
	     :config
	     (global-undo-tree-mode))
;; Like CtrlP for vim
(use-package fiplr
	     :config
	     (global-set-key (kbd "C-p") 'fiplr-find-file))
;; Sidebar file explorer
(use-package neotree
	     :config
	     (progn
	       (setq neo-smart-open t)
	       (setq-default neo-show-hidden-files t)
	       (global-set-key (kbd "<f2>") 'neotree-toggle)))
;; Project management
(use-package projectile)

(require 'ido)
(ido-mode t)
;; (require 'ido-vertical-mode)
;; (ido-mode 1)
;; (ido-vertical-mode 1)


;; Keep emacs Custom-settings in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Org-mode
;; -------------------------------------------------------------------------------
 (custom-set-variables
 '(org-directory "/data/doc/orgs")
 '(org-default-notes-file (concat org-directory "/todo.org"))
 '(org-agenda-files (list org-directory)))

;; support shift-selection-mode
(setq org-support-shift-select t)

;; for date selection start on Mondays
(setq calendar-week-start-day 1)

(setq org-todo-keywords
       '((sequence "TODO" "WAIT" "|" "DONE" "CANCELLED")))

;; Interface
;; -------------------------------------------------------------------------------
; (menu-bar-mode -1)
;; Prevent the warning "Symbol's function definition is void" when running emacs in the console
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;; Show bell
; (setq visible-bell t)

;; Cursor
;; (set-cursor-color "#ffffff")
;; (setq-default cursor-type 'bar))

;; auto show completions for execute-extended-command
(icomplete-mode 1)

;; Lines numbers
(global-linum-mode t)
;; (setq linum-format "%d ")
(setq linum-format "%4d \u2502")



;; (unless (display-graphic-p)
;;   (setq linum-format (concat linum-format " ")))
;; (setq-default left-fringe-width  10)
;; (setq-default right-fringe-width  0)
;; (set-face-attribute 'fringe nil :background "black")

;; Highlight Current Line
;; (global-hl-line-mode 1)

;; Theme
(load-theme 'wombat t)
; (load-theme 'tango-dark t)
;;(load-theme 'monokai t)
;;(load-theme 'moe-theme t)
(custom-set-faces `(default ((t (:background "#121212")))))
(set-face-attribute 'default nil :height 90)

;; Window title (with edited status + remote indication)
(setq frame-title-format
      '(""
        invocation-name
        " %@ "
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " [%*]"))

;; Disable auto-recentering on scrolling
(setq scroll-step 1)
;; Places lines between the current line and the screen edge
(setq scroll-margin 10)

;; Splash screen
(setq inhibit-splash-screen t
      initial-scratch-message nil)

;; See matching pairs of parentheses
(setq show-paren-delay 0)
(show-paren-mode 1)

;; Save positions in files between sessions
(require 'saveplace)
(setq-default save-place t)

;; yes or no becomes y or n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Follow symlinks without asking
(setq vc-follow-symlinks t)

;; Show empty lines
(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Delete marking text
(delete-selection-mode t)
(transient-mark-mode t)

;; Use X clipboard
(setq x-select-enable-clipboard t)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 25)
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Change default folder
;(setq default-directory (getenv "SILVUSPROJECT"))
;;(setq default-directory "/data/dev")

;; Enable backup files.
(setq make-backup-files t)
;; Copy all files, don't rename them.
(setq backup-by-copying t)
;; Versioning backup file
(setq delete-old-versions t ;; Don't ask to delete excess backup versions.
  kept-new-versions 6 ;; Number of newest versions to keep.
  kept-old-versions 2 ;; Number of oldest versions to keep.
  version-control t) ;; Use version numbers for backups.
;; Backup / autosave directories
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
;; (setq backup-directory-alist `((".*" . "~/.emacs.d/backup")))
;; (setq auto-save-file-name-transforms `((".*" "~/.emacs.d/autosave" t)))

;; Scratch mode
;; (setq initial-major-mode 'python-mode)


;; Bindings
;; -------------------------------------------------------------------------------

;; Use standard keybindings for copy, paste, cut
(cua-mode 1)
;; CUA mode and ISearch
(define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)

;; Shift + Arrows keys (default in Emacs 24)
;; (setq shift-select-mode t)

;; Getting PgDn to End of BufferDC
(setq scroll-error-top-bottom t)

;; Bindings
(global-set-key (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-d") 'kill-whole-line)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "C-e") 'buffer-menu)

(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "M-o") 'recentf-open-files)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)

(global-set-key (kbd "C-z") 'undo)
(global-set-key (kbd "C-y") 'redo)

;; Splits navigation
(global-set-key (kbd "C-w") 'other-window)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; One escape to quit
;; (global-set-key (kbd "<escape>") 'keyboard-quit)
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
;; (global-set-key (kbd "ESC")      'keyboard-escape-quit)
;; (global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else
; Map escape to cancel (like C-g)...
;; (define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
;; (define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals


;; Functions
;; -------------------------------------------------------------------------------

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(defun px-toggle-comments ()
  "If region is set, [un]comments it. Otherwise [un]comments current line."
  (interactive)
  (if (eq mark-active nil)
      (progn
        (beginning-of-line 1)
        (set-mark (point))
        (forward-line)
        (comment-dwim nil))
    (comment-dwim nil))
  (deactivate-mark))
(global-set-key (kbd "C-l") 'comment-or-uncomment-region-or-line)

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-beginning-position 2)))))

(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active
       (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position) (line-beginning-position 2)))))

;; Tmux Fix
;; -------------------------------------------------------------------------------
;; Ctrl + Arrows keys
;; (global-set-key "\M-[1;5C"    'forward-word)  ; Ctrl+right   => forward word
;; (global-set-key "\M-[1;5D"    'backward-word) ; Ctrl+left    => backward wordy
;; (define-key input-decode-map "\e[1;5A" [C-up])
;; (define-key input-decode-map "\e[1;5B" [C-down])

(defadvice terminal-init-screen
   ;; The advice is named `tmux', and is run before `terminal-init-screen' runs.
   (before tmux activate)
   ;; Docstring.  This describes the advice and is made available inside emacs;
   ;; for example when doing C-h f terminal-init-screen RET
   "Apply xterm keymap, allowing use of keys passed through tmux."
   ;; This is the elisp code that is run before `terminal-init-screen'.
   (if (getenv "TMUX")
       (let ((map (copy-keymap xterm-function-map)))
 	(set-keymap-parent map (keymap-parent input-decode-map))
 	(set-keymap-parent input-decode-map map))))
