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
(use-package markdown-mode
	:ensure t)
;; PHP
(use-package php-mode
	:ensure t)
;; Html / JS
(use-package web-mode
	:ensure t)
;; Python
(use-package elpy
		 :ensure t
	     :config
	     (progn
	       (elpy-enable)
	       (setq elpy-rpc-python-command "/usr/bin/python3")
	       (add-hook 'python-mode-hook (highlight-indentation-mode 0))))
;; Betters commands
(use-package smex
		 :ensure t
	     :config
	     (progn
	       (smex-initialize)
	       (global-set-key (kbd "M-x") 'smex)))
;; Autocomplete
(use-package auto-complete
		 :ensure t
	     :config
	     (add-to-list 'ac-dictionary-directories "~/.emacs.d/dict"))
;; Undo
(use-package undo-tree
		 :ensure t
	     :config
	     (global-undo-tree-mode))
;; Like CtrlP for vim
(use-package fiplr
	     :ensure t
	     :config
	     (global-set-key (kbd "C-p") 'fiplr-find-file))
;; Sidebar file explorer
(use-package neotree
	     :ensure t
	     :config
	     (progn
	       (setq neo-smart-open t)
	       (setq-default neo-show-hidden-files t)
	       (global-set-key (kbd "<f2>") 'neotree-toggle)))
;; Project management
; (use-package projectile)

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

;; ;; Org mode on start-up
;; ;; (add-hook 'after-init-hook 'org-agenda-list)
;; (setq initial-buffer-choice (lambda ()
;;   (org-agenda nil "s")
;;   (get-buffer "*Org Agenda*")))
;;
;; ;; Open agenda in current window, not on a split
;; (setq org-agenda-window-setup (quote current-window))
;;
;; ;; Agenda view Presenting longer than 1 week
;; (setq org-agenda-span 14)
;;
;; ;; Starting view from today, not monday
;; ; (setq org-agenda-start-on-weekday nil)
;;
;; ;; Starting view somes days ago
;; ; (setq org-agenda-start-day "-3d")
;;
;; ;; Agenda clock report parameters
;; ;; (setq org-agenda-clockreport-parameter-plist '(:fileskip0 t :maxlevel 5 :tstart t :link t :narrow 80 :indent t :timestamp t))
;;
;; ;; Custom agenda
;; ;; (setq org-agenda-custom-commands
;; ;;   '(("s" "Simple global view"
;; ;;      ((agenda ""))
;; ;;       ((org-agenda-overriding-header "Agenda")
;; ;;       (alltodo "")))))
;; (setq org-agenda-custom-commands
;;   '(("s" "Work agenda"
;;       ((agenda ""
;;         ((org-agenda-overriding-header "Agenda")
;;         (org-agenda-files '("/data/doc/org/work.org"))))
;;       (tags-todo "projet|support/!+TODO|+NEXT"
;;         ((org-agenda-sorting-strategy '(priority-down todo-state-down))
;;         (org-agenda-files '("/data/doc/org/work.org"))
;;         (org-agenda-overriding-header "Tasks")))
;;       ; (tags-todo "projet|support|organisation/!+WAITING"
;;       ;   ((org-agenda-overriding-header "Stuck")))
;;       (tags-todo "organisation/!+TODO|+NEXT"
;;         ((org-agenda-sorting-strategy '(priority-down todo-state-down))
;;         (org-agenda-overriding-header "Organisation")
;;         (org-agenda-files '("/data/doc/org/work.org"))))
;;       (tags "break"
;;         ((org-agenda-overriding-header "Breaks")
;;         (org-agenda-files '("/data/doc/org/work.org"))))))))
;;
;; ;; Work report for today
;; (add-to-list 'org-agenda-custom-commands
;;   '("wc" "Work report"
;;     ((agenda ""
;;       ((org-agenda-show-log 'clockcheck)
;;        (org-agenda-start-with-clockreport-mode t)
;;        (org-agenda-clockreport-parameter-plist '(:fileskip0 t :maxlevel 5 :tstart t :link t :narrow 80 :indent t :timestamp t))
;;        (org-agenda-span 'day)
;;        (org-agenda-files '("/data/doc/org/work.org"))
;;        (org-agenda-overriding-header "Work report")
;;        (org-agenda-time-grid nil))))))
;;
;; ;; Work Deadlines list
;; (add-to-list 'org-agenda-custom-commands
;;   '("wd" agenda "Deadlines"
;;     ((org-agenda-span 'week)
;;     (org-agenda-time-grid nil)
;;     (org-agenda-ndays 7)
;;     (org-agenda-start-on-weekday 0)
;;     (org-agenda-show-all-dates nil)
;;     (org-agenda-entry-types '(:scheduled))
;;     (org-agenda-overriding-header "Deadlines "))))
;;
;; ;; Today report
;; (add-to-list 'org-agenda-custom-commands
;;      '("f" "Today"
;;        ((agenda ""
;;                 ((org-agenda-entry-types '(:timestamp :sexp))
;;                  (org-agenda-overriding-header
;;                   (concat "CALENDAR Today"
;;                           (format-time-string "%a %d" (current-time))))
;;                  (org-agenda-span 'day)))
;;         (tags-todo "LEVEL=1+REFILE"
;;                    ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")))
;;         (tags-todo "DEADLINE=\"<+0d>\""
;;                    ((org-agenda-overriding-header "DUE TODAY")
;;                     (org-agenda-skip-function
;;                      '(org-agenda-skip-entry-if 'notedeadline))
;;                     (org-agenda-sorting-strategy '(priority-down))))
;;         (tags-todo "DEADLINE<\"<+0d>\""
;;                    ((org-agenda-overriding-header "OVERDUE")
;;                     (org-agenda-skip-function
;;                      '(org-agenda-skip-entry-if 'notedeadline))
;;                     (org-agenda-sorting-strategy '(priority-down))))
;;         (agenda ""
;;                 ((org-agenda-entry-types '(:scheduled))
;;                  (org-agenda-overriding-header "SCHEDULED")
;;                  (org-agenda-skip-function
;;                   '(org-agenda-skip-entry-if 'todo 'done))
;;                  (org-agenda-sorting-strategy
;;                   '(priority-down time-down))
;;                  (org-agenda-span 'day)
;;                  (org-agenda-start-on-weekday nil)
;;                  (org-agenda-time-grid nil)))
;;         (todo "DONE"
;;               ((org-agenda-overriding-header "COMPLETED"))))
;;        ((org-agenda-format-date "")
;;         (org-agenda-start-with-clockreport-mode nil))) t)
;;
;; ;; Org files paths
;;  (custom-set-variables
;;  '(org-directory "/data/doc/org")
;;  '(org-default-notes-file (concat org-directory "/todo.org"))
;;  '(org-agenda-files (list org-directory)))
;;
;; ;; Start in org folder
;; (setq default-directory "/data/doc/org")
;;
;; ;; Show all logged state changes
;; ; (setq org-agenda-log-mode-items '(state))
;;
;; ;; support shift-selection-mode
;; (setq org-support-shift-select t)
;;
;; ;; Return to activate a link
;; (setq org-return-follows-link t)
;;
;; ;; for date selection start on Mondays
;; (setq calendar-week-start-day 1)
;;
;; ;; warn me of any deadlines in next 7 days
;; (setq org-deadline-warning-days 7)
;;
;; ;; show me tasks scheduled or due in next fortnight
;; ; (setq org-agenda-span (quote fortnight))
;;
;; ;; don't show tasks as scheduled if they are already shown as a deadline
;; ; (setq org-agenda-skip-scheduled-if-deadline-is-shown t)
;;
;; ;; Agenda : do not dim blocked tasks
;; ; (setq org-agenda-dim-blocked-tasks nil)
;;
;; ;; Compact the block agenda view
;; ; (setq org-agenda-compact-blocks t)
;;
;; ;; Keep track of when a TODO item was finished
;; (setq org-log-done 'time)
;; ;; (setq org-log-done 'note)
;;
;; ;; Changes and notes will be stored into a drawer called LOGBOOK
;; (setq org-log-into-drawer t)
;;
;; ;; Keywords
;; (setq org-todo-keywords
;;        '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")))
;;
;; ;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
;;
;; ; 			 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
;; ; 			("r" "respond" entry (file "~/git/org/refile.org")
;; ; 			 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
;; ; 			("n" "note" entry (file "~/git/org/refile.org")
;; ; 			 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
;; ; 			("j" "Journal" entry (file+datetree "~/git/org/diary.org")
;; ; 			 "* %?\n%U\n" :clock-in t :clock-resume t)
;; ; 			("w" "org-protocol" entry (file "~/git/org/refile.org")
;; ; 			 "* TODO Review %c\n%U\n" :immediate-finish t)
;; ; 			("m" "Meeting" entry (file "~/git/org/refile.org")
;; ; 			 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
;; ; 			("p" "Phone call" entry (file "~/git/org/refile.org")
;; ; 			 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
;; ; 			("h" "Habit" entry (file "~/git/org/refile.org")
;; ; 			 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))
;;
;; (defun org-summary-todo (n-done n-not-done)
;;     "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;     (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook 'org-summary-todo)
;;
;; ;; Clocking
;; ; Continuous clocking
;; (setq org-clock-continuously t)
;; ; Save the running clock and all clock history when exiting Emacs, load it on startup
;; (setq org-clock-persist t)
;; (org-clock-persistence-insinuate)
;; ; Do not prompt to resume an active clock
;; (setq org-clock-persist-query-resume nil)
;; ; Removes clocked tasks with 0:00 duration
;; (setq org-clock-out-remove-zero-time-clocks t)
;; ; Clock out when moving task to a done state
;; (setq org-clock-out-when-done t)
;; ; Include current clocking task in clock reports
;; (setq org-clock-report-include-clocking-task t)
;; ;; Separate drawers for clocking and logs
;; (setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; ;; Save clock data and state changes and notes in the LOGBOOK drawer
;; (setq org-clock-into-drawer t)
;;
;; ;; Bindings
;; (global-set-key (kbd "<f5>") (org-insert-time-stamp nil t))
;; (global-set-key (kbd "<f6>") 'org-capture)
;; (global-set-key (kbd "<f7>") 'org-clock-in)
;; (global-set-key (kbd "<f8>") 'org-clock-out)
;; (global-set-key (kbd "<f9>") 'org-agenda)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (org-defkey org-mode-map (kbd "C-e") 'buffer-menu)
;;
;; ;; Fix \emsp in clocktable
;; (defun my-org-clocktable-indent-string (level)
;;   (if (= level 1)
;;       ""
;;     (let ((str "|"))
;;       (while (> level 2)
;;         (setq level (1- level)
;;               str (concat str "--")))
;;       (concat str "-> "))))
;;
;; (advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)


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
;; (setq-default cursor-type 'bar))

;; auto show completions for execute-extended-command
(icomplete-mode 1)

;; Use mouse
(xterm-mouse-mode 1)

;; Lines numbers
(global-linum-mode t)
;; (setq linum-format "%d ")
; (setq linum-format "%4d \u2502")


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
        (:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        " [%*]"))

;; Disable auto-recentering on scrolling
(setq scroll-step 1)
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
