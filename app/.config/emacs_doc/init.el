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
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Fuzzy find selector
;; -------------------------------------------------------------------------------
;; Fuzzy find all
;; (use-package helm
;;   :ensure t
;;   :straight t
;;  )

;; ;; Handle project, do not change default directory for each file opened
;; (use-package projectile
;;   :ensure t
;;   :init (projectile-mode))

;; Markdown
;; -------------------------------------------------------------------------------
(use-package markdown-mode
  :ensure t
  ;; :mode ("README\\.md\\'" . gfm-mode)
  ;; :init (setq markdown-command "multimarkdown")
  ;; :bind (:map markdown-mode-map
        ;;  ("C-c C-e" . markdown-do)))
  )

;; Dim unfocused splits
;; -------------------------------------------------------------------------------
;; (use-package dimmer
  ;; :ensure t
  ;; :config
  ;; (dimmer-mode 1)
  ;; (setq dimmer-fraction 0.4))

(custom-set-faces
  '(mode-line          ((t (:background "#37633f" :foreground "white"))))
  '(mode-line-inactive ((t (:background "#333333" :foreground "dim gray")))))

;; Interface
;; -------------------------------------------------------------------------------
; (menu-bar-mode -1)
;; Prevent the warning "Symbol's function definition is void" when running emacs in the console
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)))
      ;(scroll-bar-mode -1)))

;; Show bell
; (setq visible-bell t)

;; Cursor
;; (set-cursor-color "#ffffff")
(setq-default cursor-type 'bar)

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
;; (global-hl-line-mode 1)

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
;; (setq scroll-step 1)
;; Places lines between the current line and the screen edge
(setq scroll-margin 10)

;; Save all current buffers to a "desktop" file
(desktop-save-mode 1)

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

; (require 'recentf)
; (recentf-mode 1)
; (setq recentf-max-menu-items 25)
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

;; Which key
(which-key-mode 1)
(which-key-setup-side-window-right-bottom)


;; Org-mode
;; -------------------------------------------------------------------------------
(load (expand-file-name (concat user-emacs-directory "org")))


;; Bindings
;; -------------------------------------------------------------------------------
;; Use standard keybindings for copy, paste, cut
(cua-mode 1)
;; CUA mode and ISearch
;; (define-key isearch-mode-map (kbd "C-v") 'isearch-yank-kill)

;; Shift + Arrows keys (default in Emacs 24)
;; (setq shift-select-mode t)

;; Getting PgDn to End of BufferDC
(setq scroll-error-top-bottom t)

;;   "Delete text from current position to end of line char. This command does not push text to `kill-ring'."
;;   (interactive)
;;     (delete-region (progn (forward-line 0) (point))
;;       (progn (forward-line 1) (point))))

;; (global-set-key (kbd "C-a") 'mark-whole-buffer)
;; (define-key org-mode-map (kbd "C-a") 'mark-whole-buffer)
(global-set-key (kbd "C-s") 'save-buffer)
;; ;(global-set-key (kbd "C-e") 'buffer-menu)

;; (global-set-key (kbd "C-o") 'find-file)
;; (global-set-key (kbd "M-o") 'recentf-open-files)

(global-set-key (kbd "C-f") 'isearch-forward)
(global-set-key (kbd "C-S-f") 'isearch-backward)
;; (define-key isearch-mode-map [(control f)] 'isearch-repeat-forward)

;; Delete line
(global-set-key (kbd "C-d")
  (lambda () (interactive)
    (if (use-region-p)
        (delete-region (region-beginning) (region-end))
      (kill-whole-line))))

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

(global-set-key (kbd "C-<left>") 'my/backward-word-stop-at-bol)
(global-set-key (kbd "C-S-<left>") 'my/backward-word-stop-at-bol)

;; Splits navigation
(global-set-key (kbd "C-x w") 'kill-buffer-and-window)
(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; Open File
;; (global-set-key (kbd "C-x f") 'helm-find-files)

;; One escape to quit
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(define-key org-mode-map (kbd "<escape>") 'keyboard-escape-quit)

;; Reload emacs config
(global-set-key (kbd "C-c C-r") (lambda () (interactive) (load-file user-init-file)))
;; Edit emacs config
(global-set-key (kbd "C-c C-e") (lambda () (interactive) (find-file user-init-file)))

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
