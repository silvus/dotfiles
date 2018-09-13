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
;; Sidebar file explorer
(use-package neotree
	     :ensure t
	     :config
	     (progn
	       (setq neo-smart-open t)
	       (setq-default neo-show-hidden-files t)
	       (global-set-key (kbd "<f2>") 'neotree-toggle)))
;; Modeline
(use-package spaceline
	:ensure t
	:config
	(progn
	    (require 'spaceline-config)
		(setq spaceline-highlight-face-func 'spaceline-highlight-face-modified)
		(spaceline-emacs-theme)))
;; 2048
(use-package 2048-game
	:ensure t)

;; Fuzzy find
(use-package helm
	:ensure t
	:config
	(progn
		(require 'helm-config)
		(helm-mode 1)
		(global-set-key (kbd "C-e") 'helm-buffers-list)
		(global-set-key (kbd "C-p") 'helm-find-files)))

;(require 'ido)
;(ido-mode t)
;; (require 'ido-vertical-mode)
;; (ido-mode 1)
;; (ido-vertical-mode 1)


;; Keep emacs Custom-settings in separate file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Games scores files
(if (file-directory-p "/data/doc/org")
    ; at home
    (setq gamegrid-user-score-file-directory "/data/doc/.gamesscores")
    ; at work
    (setq gamegrid-user-score-file-directory "~/Notes/.games"))


;; Org-mode
;; -------------------------------------------------------------------------------

(require 'org)

;; Org files paths
(if (file-directory-p "/data/doc/org")
	; at home
	(setq org-directory "/data/doc/org")
	; at work
	(setq org-directory "~/Notes"))
(setq org-default-notes-file (concat org-directory "/todo.org"))

;; Collect all .org from my Org directory and subdirs
(load-library "find-lisp")
(if (file-directory-p "/data/doc/org")
	; at home (more than just-org directory to include work folder
	(setq org-agenda-files (find-lisp-find-files "/data/doc" "\\.org$"))
	; at work
	(setq org-agenda-files (find-lisp-find-files org-directory "\\.org$")))

;; Org mode on start-up
;; (add-hook 'after-init-hook 'org-agenda-list)
(setq initial-buffer-choice (lambda ()
  (org-agenda nil "s")
  (get-buffer "*Org Agenda*")))

;; Hooks
(defun my-org-clock-out ()
	" Call external script on org-mode clock out "
	(shell-command (format "%s %s"
		"~/.emacs.d/hooks/org-clock-out.sh"
		(shell-quote-argument(
			;format "%s: %s" (buffer-file-name (window-buffer (minibuffer-selected-window))) (org-clock-get-clock-string)
			format "%s: %s" (file-name-sans-extension (file-name-nondirectory buffer-file-name)) (org-clock-get-clock-string)
			;(concat "lebuffer" ": " (org-clock-get-clock-string))
			;(concat (buffer-file-name (window-buffer (minibuffer-selected-window))) ": " (org-clock-get-clock-string))
			;(concat (buffer-file-name (window-buffer (minibuffer-selected-window))) ": " (org-clock-get-clock-string))
			;; format "%s: %s" (buffer-file-name (window-buffer (minibuffer-selected-window))) (org-clock-get-clock-string)))
		)))
	)
)
(when (file-readable-p "~/.emacs.d/hooks/org-clock-out.sh")
	(add-hook 'org-clock-out-hook 'my-org-clock-out))

;; Start in org folder
(setq default-directory org-directory)

;; Open agenda in current window, not on a split
(setq org-agenda-window-setup (quote current-window))

;; Agenda view Presenting longer than 1 week
(setq org-agenda-span 14)

; Hide in sheduled if already
(setq org-agenda-skip-scheduled-if-done t)

;; Don't show tasks as scheduled if they are already shown as a deadline
; (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

;; Agenda look at archives files too
; (setq org-agenda-archives-mode t)

;; Starting view from today, not monday
; (setq org-agenda-start-on-weekday nil)

;; Starting view somes days ago
;(setq org-agenda-start-day "-3d")

;; Agenda clock report parameters
(setq org-agenda-clockreport-parameter-plist '(:stepskip0 t :fileskip0 t :maxlevel 5 :tcolumns 1 :link t :narrow 80 :indent t :timestamp t))

;; Custom agenda
;; (setq org-agenda-custom-commands
;;   '(("s" "Simple global view"
;;      ((agenda ""))
;;       ((org-agenda-overriding-header "Agenda")
;;       (alltodo "")))))
(setq org-agenda-custom-commands
  '(("s" "Work agenda"
      ((agenda ""
      	((org-agenda-overriding-header "WORKLOG")
          (org-agenda-span 1)
          (org-agenda-start-day "today")
          (org-agenda-start-with-log-mode t)
          ;(org-agenda-time-grid nil)
          (org-agenda-show-log 'clockcheck)))
      (agenda ""
        ((org-agenda-overriding-header "AGENDA")
          ;(org-agenda-max-entries 10)
          (org-agenda-span 14)
          (org-agenda-start-day "today")
          (org-agenda-time-grid nil)
          (org-agenda-show-all-dates nil)))
          ;(org-agenda-start-day "today")
      ; (tags-todo "projet|support|organisation/!+WAITING"
      ;   ((org-agenda-overriding-header "Stuck")))
      ;(tags-todo "SCHEDULED=\"<+0d>\"|DEADLINE=\"<+0d>\""
  	  ; (tags-todo "SCHEDULED<=\"<today>\"|DEADLINE<=\"<today>\""
      ;  ((org-agenda-overriding-header "TODAY ")
      ;      (org-agenda-sorting-strategy '(priority-down))))
      ; (tags-todo "SCHEDULED<\"<today>\"|DEADLINE<\"<today>\""
      ;   ((org-agenda-overriding-header "Retard")
      ;     (org-agenda-sorting-strategy '(priority-down))))
      ; (tags-todo "SCHEDULED>\"<today>\"|DEADLINE>\"<today>\""
      ;  ((org-agenda-overriding-header "SCHEDULED")
      ;	  (org-agenda-sorting-strategy '(priority-down))))
      (todo "NEXT"
        ((org-agenda-sorting-strategy '(priority-down todo-state-down))
          (org-agenda-skip-entry-if 'scheduled)
          (org-agenda-overriding-header "NEXT")))
      (todo "TODO"
        ((org-agenda-sorting-strategy '(priority-down todo-state-down))
          (org-agenda-overriding-header "TODO")))
      (tags "breaks"
        ((org-agenda-overriding-header "Breaks")))))))

;; Work report for today
(add-to-list 'org-agenda-custom-commands
  '("wc" "Work report"
    ((agenda ""
       ; TODO: to be set with setq
       ((org-agenda-start-with-clockreport-mode t)
       (org-agenda-clockreport-parameter-plist '(:step day :stepskip0 t :fileskip0 t :maxlevel 5 :tcolumns 1 :link t :narrow 80 :indent t :timestamp t))
       (org-agenda-show-log 'clockcheck)
       ;(org-agenda-log-mode-items '(clock))
       (org-agenda-start-with-log-mode t)
       (org-agenda-span 2)
       (org-agenda-start-day "-1d")
       (org-agenda-start-on-weekday nil)
       (org-agenda-overriding-header "Work report")
       (org-agenda-time-grid nil))))))

;; Work Deadlines list
; (add-to-list 'org-agenda-custom-commands
;   '("wd" agenda "Deadlines"
;     ((org-agenda-span 'week)
;     (org-agenda-time-grid nil)
;     (org-agenda-ndays 7)
;     (org-agenda-start-on-weekday 0)
;     (org-agenda-show-all-dates nil)
;     (org-agenda-entry-types '(:scheduled))
;     (org-agenda-overriding-header "Deadlines "))))

;; Today report (WIP and tests)
(add-to-list 'org-agenda-custom-commands
     '("f" "Today"
       ((agenda ""
                ((org-agenda-entry-types '(:timestamp :sexp))
                 (org-agenda-overriding-header
                  (concat "CALENDAR Today " (format-time-string "%a %d" (current-time))))
                 (org-agenda-span 'day)))
        (tags-todo "LEVEL=1+REFILE"
                   ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")))
        (tags-todo "DEADLINE=\"<+0d>\""
                   ((org-agenda-overriding-header "DUE TODAY")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'notedeadline))
                    (org-agenda-sorting-strategy '(priority-down))))
        (tags-todo "DEADLINE<\"<+0d>\""
                   ((org-agenda-overriding-header "OVERDUE")
                    (org-agenda-skip-function
                     '(org-agenda-skip-entry-if 'notedeadline))
                    (org-agenda-sorting-strategy '(priority-down))))
        (agenda ""
                ((org-agenda-entry-types '(:scheduled))
                 (org-agenda-overriding-header "SCHEDULED")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'todo 'done))
                 (org-agenda-sorting-strategy
                  '(priority-down time-down))
                 (org-agenda-span 'day)
                 (org-agenda-start-on-weekday nil)
                 (org-agenda-time-grid nil)))
        (todo "DONE"
              ((org-agenda-overriding-header "COMPLETED"))))
       ((org-agenda-format-date "")
        (org-agenda-start-with-clockreport-mode nil))) t)

;; Show all logged state changes
; (setq org-agenda-log-mode-items '(state))

;; support shift-selection-mode
(setq org-support-shift-select t)

;; Return to activate a link
(setq org-return-follows-link t)

;; For date selection start on Mondays
(setq calendar-week-start-day 1)

;; Display events from calendar diary
(setq org-agenda-include-diary t)
;; Turn off somes holidays
(setq holiday-bahai-holidays nil)
(setq holiday-hebrew-holidays nil)
(setq holiday-islamic-holidays nil)

;; French  holiday (https://www.emacswiki.org/emacs/french-holidays.el)
(require 'calendar)
(require 'holidays)
(defvar holiday-french-holidays nil "French holidays")
(setq holiday-french-holidays
      `((holiday-fixed 1 1 "Jour de l'an")
	(holiday-fixed 1 6 "Épiphanie")
	(holiday-fixed 2 2 "Chandeleur")
	(holiday-fixed 2 14 "Saint Valentin")
	(holiday-fixed 5 1 "Fête du travail")
	(holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
	(holiday-fixed 6 21 "Fête de la musique")
	(holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
	(holiday-fixed 8 15 "Assomption (Religieux)")
	(holiday-fixed 11 11 "Armistice de 1918")
	(holiday-fixed 11 1 "Toussaint")
	(holiday-fixed 11 2 "Commémoration des fidèles défunts")
	(holiday-fixed 12 25 "Noël")
        ;; fetes a date variable
	(holiday-easter-etc 0 "Pâques")
        (holiday-easter-etc 1 "Lundi de Pâques")
        (holiday-easter-etc 39 "Ascension")
        (holiday-easter-etc 49 "Pentecôte")
        (holiday-easter-etc -47 "Mardi gras")
	(holiday-float 5 0 4 "Fête des mères")
	;; dernier dimanche de mai ou premier dimanche de juin si c'est le
	;; même jour que la pentecôte TODO
	(holiday-float 6 0 3 "Fête des pères"))) ;; troisième dimanche de juin
(setq calendar-holidays holiday-french-holidays)

;; Warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;; Show me tasks scheduled or due in next fortnight
; (setq org-agenda-span (quote fortnight))

;; Agenda : do not dim blocked tasks
; (setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
; (setq org-agenda-compact-blocks t)

;; Show inline images
(setq org-startup-with-inline-images t)

;; In modeline, show current timer for today and not total for a task
(setq org-clock-mode-line-total 'today)

;; Keep track of when a TODO item was finished
(setq org-log-done 'time)
;; Keep track of when a TODO item was finished with a note
;; (setq org-log-done 'note)

;; Non-nil means undone TODO entries will block switching the parent to DONE
(setq org-enforce-todo-dependencies t)

;; Changes and notes will be stored into a drawer called LOGBOOK
(setq org-log-into-drawer t)

;; Keywords
(setq org-todo-keywords
       '(
       	;; Sequence for TASKS
       	(sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)")
       	;; Sequence for EVENTS
       	;;(sequence "VISIT(v@/!)" "|" "DIDNOTGO(z@/!)" "MEETING(m@/!)" "VISITED(y@/!)")
       	))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline org-default-notes-file "Collect")
        "* TODO %?\n  %i" :empty-lines 1)
   ("b" "Bookmark" entry (file+headline (concat org-directory "/bookmark.org") "Bookmarks")
        "* %?\n:PROPERTIES:\n:CREATED: %U\n:END:\n\n" :empty-lines 1)
   ("j" "Journal" entry (file+datetree org-default-notes-file)
        "* %?\nLe %U\n  %i" :empty-lines 1)))

;; Clocking
; Continuous clocking
(setq org-clock-continuously t)
; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
; Removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Change tasks to whatever when clocking in
; (setq org-clock-in-switch-to-state "NEXT")

;; Bindings
;(global-set-key (kbd "<f5>") (org-insert-time-stamp nil t))
(global-set-key (kbd "<f5>") (lambda () (interactive)
	(org-insert-time-stamp nil t)))
;(global-set-key (kbd "<f5>") (org-insert-time-stamp (current-time)))
(global-set-key (kbd "<f6>") 'org-capture)
(global-set-key (kbd "<f7>") 'org-clock-in)
(global-set-key (kbd "<f8>") 'org-clock-out)
(global-set-key (kbd "<f9>") 'org-agenda)
(global-set-key (kbd "C-c a") 'org-agenda)
;(org-defkey org-mode-map (kbd "C-e") 'buffer-menu)
(org-defkey org-mode-map (kbd "C-e") 'helm-buffers-list)

;; Fix \emsp in clocktable
(defun my-org-clocktable-indent-string (level)
  (if (= level 1)
      ""
    (let ((str "|"))
      (while (> level 2)
        (setq level (1- level)
              str (concat str "--")))
      (concat str "-> "))))

(advice-add 'org-clocktable-indent-string :override #'my-org-clocktable-indent-string)


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
(icomplete-mode 1)

;; Use mouse
(xterm-mouse-mode 1)

;; Lines numbers
(global-linum-mode t)

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#121212"))))
 '(cursor ((t (:background "white")))))

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
;(global-set-key (kbd "C-e") 'buffer-menu)

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


