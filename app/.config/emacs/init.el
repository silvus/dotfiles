;;; -*- lexical-binding: t; -*-

;; straight.el
;; -------------------------------------------------------------------------------
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
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Keep emacs Custom-settings in separate file
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)

;; Fuzzy find selector
;; -------------------------------------------------------------------------------
;; Smart framework for minibuffer
(use-package vertico
  :ensure t
  :init
  (setq vertico-resize t)
  (setq vertico-count 22)
  (vertico-mode))

;; Use the `orderless' completion style
(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Rich annotations in the minibuffer (docstrings, file sizes, etc.)
(use-package marginalia
  :ensure t
  :init
  (marginalia-mode))

;; Recent files
(use-package recentf
  :ensure t
  :init
  (recentf-mode 1)
  :custom
  (recentf-max-saved-items 50)
  (recentf-auto-cleanup 'never))

;; Project and find file management
(use-package consult
  :ensure t
  :init
  ;; Search everything except .git
  (setq consult-ripgrep-args
	"rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --hidden --glob '!.git'")
  ;; How to defined a root project? Default to .git, use Syncthing too
  (setq project-vc-extra-root-markers '(".stfolder"))
  ;; Always opens file selection after switching
  (setq project-switch-commands 'project-find-file)
  ;; Disable preview to select file with just one enter
  ;; (setq consult-preview-key nil)
  :bind (
	 ("C-p" . project-find-file) ;; Find file in current project
	 ("C-S-F" . consult-ripgrep)  ;; Find string in current project
	 ("C-S-p" . project-switch-project) ;; Change project
	 ("C-S-o" . consult-recent-file) ;; List recent files
	 ("C-e" . consult-buffer) ;; List buffers
	 ("C-S-e" . dired) ;; List Files
	 ))

;; Markdown
;; -------------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  :mode "\\.md\\'"  ;; Enable only for md files
  ;; :mode ("README\\.md\\'" . gfm-mode)
  ;; :init (setq markdown-command "multimarkdown")
  ;; :bind (:map markdown-mode-map
  ;;  ("C-c C-e" . markdown-do)))
  )

;; Git Gutter
;; -------------------------------------------------------------------------------
(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode 1)
  (diff-hl-flydiff-mode 1))

;; Undo tree
;; -------------------------------------------------------------------------------
;; (use-package undo-tree
;;  :ensure t
;;  :config
;; (global-undo-tree-mode 1))

;; Code
;; -------------------------------------------------------------------------------
;; LSP
(use-package eglot
  :ensure nil
  :hook ((python-ts-mode . eglot-ensure)
         (rust-ts-mode . eglot-ensure)
         (nix-mode . eglot-ensure)
         (lua-mode . eglot-ensure))
  :config
  (add-to-list 'eglot-server-programs
               '(nix-mode . ("nil"))))

;; Suggestions
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :init
  (global-corfu-mode))

;; Treesitter
(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
        (rust-mode . rust-ts-mode)
        (css-mode . css-ts-mode)
        (js-mode . js-ts-mode)
        (javascript-mode . js-ts-mode)
        (yaml-mode . yaml-ts-mode)
        (bash-mode . bash-ts-mode)
        (sh-mode . bash-ts-mode)
        (json-mode . json-ts-mode)))

;; Formatter
(use-package apheleia
  :config
  (setf (alist-get 'markdown-mode apheleia-mode-alist)
        'dprint)
  (setf (alist-get 'python-ts-mode apheleia-mode-alist) 'ruff)
  (setf (alist-get 'rust-ts-mode apheleia-mode-alist) 'rustfmt)
  (setf (alist-get 'lua-mode apheleia-mode-alist) 'stylua)
  (setf (alist-get 'nix-mode apheleia-mode-alist) 'alejandra)

  (setf (alist-get 'dprint apheleia-formatters)
        '("dprint"
          "fmt"
          "--stdin"
          ;; "--stdin-file-path"
          filepath))

  (apheleia-global-mode +1))

;; Format on save
;; (add-hook 'before-save-hook #'apheleia-format-buffer)
(dolist (hook '(python-ts-mode-hook
                rust-ts-mode-hook
                nix-mode-hook
                lua-mode-hook
                markdown-mode-hook))
  (add-hook hook
            (lambda ()
              (add-hook 'before-save-hook
                        #'apheleia-format-buffer
                        nil t))))

;; Rust
(use-package rust-mode
  :mode "\\.rs\\'")

;; Interface
;; -------------------------------------------------------------------------------
;; (menu-bar-mode -1)
;; Prevent the warning "Symbol's function definition is void" when running emacs in the console
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)))
;; (scroll-bar-mode -1)))

;; Show bell
;; (setq visible-bell t)

;; Minibuffer
;; (setq resize-mini-windows t)
;; (setq max-mini-window-height 0.25)

;; Cursor
;; (set-cursor-color "#ffffff")
(setq-default cursor-type 'bar)
(add-to-list 'default-frame-alist '(cursor-type . bar))
(modify-all-frames-parameters '((cursor-type . bar)))

;; Auto show completions for execute-extended-command
;; (icomplete-mode 1)

;; Use mouse
(xterm-mouse-mode 1)

;; Lines numbers
(global-display-line-numbers-mode 1)

;; Show trailings spaces
(setq-default show-trailing-whitespace t)

;; (unless (display-graphic-p)
;;   (setq linum-format (concat linum-format " ")))
;; (setq-default left-fringe-width  10)
;; (setq-default right-fringe-width  0)
;; (set-face-attribute 'fringe nil :background "black")

;; Highlight Current Line
(global-hl-line-mode 1)
(custom-set-faces
 '(hl-line ((t (:background "#344530")))))

;; Theme
(load-theme 'wombat t)
;;(load-theme 'tango-dark t)
;;(load-theme 'monokai t)
;;(load-theme 'moe-theme t)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background "#121212"))))
;;  '(cursor ((t (:background "white")))))

;; Selected text
(set-face-attribute 'region nil :background "#3a5f8a")

;; Line Height
(set-face-attribute 'default nil :height 90)

;; Window title (with edited status + remote indication)
;; (setq frame-title-format
;;       '(""
;;         invocation-name
;;         (:eval (if (buffer-file-name)
;;                    (abbreviate-file-name (buffer-file-name))
;;                  "%b"))
;;         " [%*]"))

;; Disable auto-recentering on scrolling
(setq scroll-step 1)
;; Places lines between the current line and the screen edge
;; (setq scroll-margin 20)
(setq scroll-margin 80)

;; Save all current buffers to a "desktop" file
;; (desktop-save-mode 1)

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

;; Use X primary clipboard
(setq select-enable-clipboard t)
(setq select-enable-primary t)

;; Change default folder
;; (setq default-directory (getenv "SILVUSPROJECT"))
;;(setq default-directory "/data/dev")

;; Backup files (~ files)
(defvar emacs-backup-dir
  (expand-file-name "backup/" user-emacs-directory))
(setq backup-directory-alist
      `(("." . ,emacs-backup-dir)))
(setq make-backup-files t
      backup-by-copying t
      version-control t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2)

;; Auto-save files (#file#)
(defvar emacs-autosave-dir
  (expand-file-name "autosave/" user-emacs-directory))
(setq auto-save-file-name-transforms
      `((".*" ,emacs-autosave-dir t)))
(setq auto-save-list-file-prefix
      (expand-file-name "saves-" emacs-autosave-dir))

;; Lock files (. #file)
(setq create-lockfiles nil)
;; (defvar emacs-lock-dir
;;   (expand-file-name "lock/" user-emacs-directory))
;; (unless (file-directory-p emacs-lock-dir)
;;   (make-directory emacs-lock-dir t))
;; (setq lock-file-name-transforms
;;       `((".*" ,emacs-lock-dir t)))

;; Ensure directories exist
;; (dolist (dir (list emacs-backup-dir emacs-autosave-dir emacs-lock-dir)))
(dolist (dir (list emacs-backup-dir emacs-autosave-dir))
  (unless (file-directory-p dir)
    (make-directory dir t)))

;; Scratch mode
;; (setq initial-major-mode 'python-mode)

;; Which key
(which-key-mode 1)
(which-key-setup-side-window-right-bottom)

;; project.el
;; (defun my/project-files-no-hidden (project)
;;  (seq-filter
;;   (lambda (file)
;;     (not (string-match-p "/\\." file)))  ;; exclude any path component starting with .
;;   (project-files-filtered project)))

;; Do not ask for project each time
;; (setq project-current-inhibit-prompt t)

;; Open dired in project root
;; (global-set-key (kbd "C-c e")
;;   (lambda () (interactive)
;;     (let ((proj (project-current)))
;;       (dired (if proj (project-root proj) default-directory)))))


;; Org-mode
;; -------------------------------------------------------------------------------
(load (expand-file-name (concat user-emacs-directory "org")))


;; Bindings
;; -------------------------------------------------------------------------------
;; Use standard keybindings for copy, paste, cut
(cua-mode 1)
(with-eval-after-load 'cua-base
  (define-key cua--cua-keys-keymap (kbd "M-v") nil)
  (define-key cua--cua-keys-keymap (kbd "M-h") nil))

;; Getting PgDn to End of BufferDC
(setq scroll-error-top-bottom t)

;; Buffer management
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-a") 'mark-whole-buffer)
;; (define-key org-mode-map (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
;; (global-set-key (kbd "C-e") 'buffer-menu)

;; (global-set-key (kbd "C-o") 'find-file)
;; (global-set-key (kbd "M-o") 'recentf-open-files)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
;; (define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)

;; Delete line
(global-set-key (kbd "C-d")
		(lambda ()
		  (interactive)
		  (if (use-region-p)
		      (delete-region (region-beginning) (region-end))
		    (delete-region
		     (line-beginning-position)
		     (min (point-max)
			  (line-beginning-position 2))))))
;; Comment line
(global-set-key (kbd "C-/")
		(lambda () (interactive)
		  (if (use-region-p)
		      (comment-or-uncomment-region (region-beginning) (region-end))
		    (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

;; (global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-y") 'redo)
;; (global-set-key (kbd "C-S-y") 'redo)
;; (define-key org-mode-map (kbd "C-y") 'redo)

;; (global-set-key;; (global-set-key (kbd "C-z") 'undo)
;; (global-set-key (kbd "C-y") 'redo)
;; (global-set-key (kbd "C-S-y") 'redo)
;; (define-key org-mode-map (kbd "C-y") 'redo)

;; Horizontal word movement with Ctrl + arrow
;; (global-set-key (kbd "C-<right>") 'forward-word)
;; (global-set-key (kbd "C-<left>")  'backward-word)

;; (global-set-key (kbd "C-<left>") 'my/backward-word-stop-at-bol)
;; (global-set-key (kbd "C-S-<left>") 'my/backward-word-stop-at-bol)

;; Close the pane if there are multiple panes; otherwise close the document
(defun my/close ()
  (interactive)
  (if (one-window-p)
      (if (buffer-file-name)
          (kill-current-buffer)
        (bury-buffer))
    (delete-window)))

;; Split and focus
(defun my/split-window-right ()
  (interactive)
  (select-window (split-window-right)))
(defun my/split-window-below ()
  (interactive)
  (select-window (split-window-below)))
(if (display-graphic-p)
    (progn
      ;; GUI bindings
      ;; (global-set-key (kbd "M-w") 'kill-buffer-and-window)
      ;; (global-set-key (kbd "M-w") #'kill-current-buffer)
      (global-set-key (kbd "M-w") #'my/close)
      (global-set-key (kbd "M-<up>") 'windmove-up)
      (global-set-key (kbd "M-<down>") 'windmove-down)
      (global-set-key (kbd "M-<left>") 'windmove-left)
      (global-set-key (kbd "M-<right>") 'windmove-right)
      (global-set-key (kbd "M-v") #'my/split-window-right)
      (global-set-key (kbd "M-h") #'my/split-window-below))
  (progn
    ;; Terminal bindings
    ;; (global-set-key (kbd "C-x w") 'kill-buffer-and-window)
    ;; (global-set-key (kbd "C-x w") 'kill-current-buffer)
    (global-set-key (kbd "C-x w") #'my/close)
    (global-set-key (kbd "C-x <up>") 'windmove-up)
    (global-set-key (kbd "C-x <down>") 'windmove-down)
    (global-set-key (kbd "C-x <left>") 'windmove-left)
    (global-set-key (kbd "C-x <right>") 'windmove-right)
    (global-set-key (kbd "C-x v") #'my/split-window-right)
    (global-set-key (kbd "C-x h") #'my/split-window-below))
  )
(global-set-key (kbd "C-o") #'other-window)

;; One escape to quit
(global-set-key (kbd "<escape>") 'keyboard-quit)
(define-key minibuffer-local-map (kbd "<escape>") 'minibuffer-keyboard-quit)
;; (global-set-key (kbd "<escape>") 'keyboard-quit)
;; (define-key org-mode-map (kbd "<escape>") 'keyboard-quit)

;; Reload emacs config
(global-set-key (kbd "C-c C-r") (lambda () (interactive) (load-file user-init-file)))
;; Edit emacs config
(global-set-key (kbd "C-c C-o") (lambda () (interactive) (find-file user-init-file)))

;; Org agenda
(global-set-key (kbd "C-c a") 'org-agenda)
;; Org capture
(global-set-key (kbd "C-c c") 'org-capture)

;; (global-set-key (kbd "<escape>") 'keyboard-quit)
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; (global-set-key (kbd "<escape>")      'keyboard-escape-quit)
;; (global-set-key (kbd "ESC")      'keyboard-escape-quit)
;; (global-set-key [escape] 'keyboard-escape-quit)         ;; everywhere else
					; Map escape to cancel (like C-g)...
;; (define-key isearch-mode-map [escape] 'isearch-abort)   ;; isearch
;; (define-key isearch-mode-map "\e" 'isearch-abort)   ;; \e seems to work better for terminals
