;; Org
;; -----------------------------------------------------------------------------

;; Detect if we're on a personal or work computer
(defvar my/is-personal-computer (file-directory-p "/data/doc")
  "Non-nil if this is a personal computer (determined by presence of /data/doc).")

;; Set org directory based on computer type (must be set before org loads)
(setq org-directory (if my/is-personal-computer
                      "/data/doc"
                      "/data/work"))

;; Set initial agenda view based on computer type
;; (add-hook 'after-init-hook 'org-agenda-list)
(setq initial-buffer-choice (lambda ()
                              (if my/is-personal-computer
                                  (org-agenda nil "e")  ; personal agenda
                                (org-agenda nil "s"))   ; work agenda
                              (get-buffer "*Org Agenda*")))

;; Start in org folder
(setq default-directory org-directory)

;; Org configuration wrapped in after! block
(after! org
  (setq org-default-notes-file (concat org-directory "/todo.org"))

  ;; Function to find org files while ignoring hidden files and directories
  (defun my/find-org-files (directory)
    "Find all .org files in DIRECTORY, ignoring hidden files and directories.
    This excludes files and directories starting with '.' (like .git, .backup, .stversions, etc.)"
    (let ((org-files (directory-files-recursively directory "\\.org$")))
      (seq-filter (lambda (file)
                    (not (my/path-contains-hidden-directory-p file)))
                  org-files)))

  ;; Helper function to check if path contains any hidden directories
  (defun my/path-contains-hidden-directory-p (path)
    "Return t if PATH contains any directory component starting with '.'."
    (let ((path-components (split-string path "/" t)))
      (seq-some (lambda (component) (string-match "^\\." component)) path-components)))

  ;; Collect all .org files from org directory and subdirs (excluding hidden)
  (setq org-agenda-files (my/find-org-files org-directory))

  ;; Open agenda in current window, not on a split
  (setq org-agenda-window-setup (quote current-window))

  ;; Agenda view Presenting longer than 1 week
  (setq org-agenda-span 14)

  ;; Hide in scheduled if already done
  (setq org-agenda-skip-scheduled-if-done t)

  ;; Don't show tasks as scheduled if they are already shown as a deadline
  ;; (setq org-agenda-skip-scheduled-if-deadline-is-shown t)

  ;; Agenda look at archives files too
  ;; (setq org-agenda-archives-mode t)

  ;; Starting view from today, not monday
  ;; (setq org-agenda-start-on-weekday nil)

  ;; Starting view somes days ago
  ;; (setq org-agenda-start-day "-3d")

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
            (org-agenda-skip-entry-if 'scheduled)
            (org-agenda-overriding-header "TODO")))
        (todo "WAIT"
          ((org-agenda-sorting-strategy '(priority-down todo-state-down))
            (org-agenda-skip-entry-if 'scheduled)
            (org-agenda-overriding-header "WAIT")))))))

  ;; Home agenda
  (add-to-list 'org-agenda-custom-commands
    '("e" "Agenda"
      ((agenda ""
          ((org-agenda-overriding-header "Today")
           (org-agenda-span 'day)
           (org-agenda-start-day "today")
           (org-agenda-start-on-weekday nil)
           (org-agenda-sorting-strategy '(todo-state-up priority-down time-up category-up))))
       (agenda ""
          ((org-agenda-overriding-header "This week")
           (org-agenda-span 'week)
           (org-agenda-start-on-weekday nil)
           (org-agenda-start-day "+1d")
           (org-agenda-sorting-strategy '(time-up priority-down category-up todo-state-up))))
       (tags-todo "-SCHEDULED={.+}-DEADLINE={.+}"
          ((org-agenda-overriding-header "To schedule")
           (org-agenda-sorting-strategy '(todo-state-up priority-down category-up)))))))

  ; (defun org-agenda-skip-tag (tag &optional others)
  ;   "Skip all entries that correspond to TAG.
  ;   If OTHERS is true, skip all entries that do not correspond to TAG."
  ;   (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
  ;         (current-headline (or (and (org-at-heading-p)
  ;                                    (point))
  ;                               (save-excursion (org-back-to-heading)))))
  ;     (if others
  ;         (if (not (member tag (org-get-tags-at current-headline)))
  ;             next-headline
  ;           nil)
  ;       (if (member tag (org-get-tags-at current-headline))
  ;           next-headline
  ;         nil))))

  ;; Family agenda (for exports)
  ; (add-to-list 'org-agenda-custom-commands
  ;   '("f" "Family Agenda"
  ;     ((agenda ""
  ;         ((org-agenda-overriding-header "")
  ;           (org-agenda-span 7)
  ;           (org-agenda-start-day "today")
  ;           (org-agenda-skip-function '(org-agenda-skip-tag "famille" 't))
  ;           (org-agenda-remove-tags t)
  ;           (org-agenda-start-on-weekday nil)
  ;           (org-agenda-time-grid nil)
  ;           (org-agenda-show-all-dates nil)))
  ;     (tags-todo "+famille-SCHEDULED={.+}-DEADLINE={.+}/+TODO|+NEXT"
  ;       ((org-agenda-sorting-strategy '(priority-down todo-state-down))
  ;         (org-agenda-skip-entry-if 'scheduled)
	 ;    	(org-agenda-remove-tags t)
  ;         ; org-agenda-tag-filter-preset doesn't work in -batch ?!
		;     ;(org-agenda-tag-filter-preset '("+famille" "-SCHEDULE"))
  ;         (org-agenda-overriding-header "NEXT TODOS"))))))

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
  ; (add-to-list 'org-agenda-custom-commands
  ;      '("t" "Today"
  ;        ((agenda ""
  ;                 ((org-agenda-entry-types '(:timestamp :sexp))
  ;                  (org-agenda-overriding-header
  ;                   (concat "CALENDAR Today " (format-time-string "%a %d" (current-time))))
  ;                  (org-agenda-span 'day)))
  ;         (tags-todo "LEVEL=1+REFILE"
  ;                    ((org-agenda-overriding-header "COLLECTBOX (Unscheduled)")))
  ;         (tags-todo "DEADLINE=\"<+0d>\""
  ;                    ((org-agenda-overriding-header "DUE TODAY")
  ;                     (org-agenda-skip-function
  ;                      '(org-agenda-skip-entry-if 'notedeadline))
  ;                     (org-agenda-sorting-strategy '(priority-down))))
  ;         (tags-todo "DEADLINE<\"<+0d>\""
  ;                    ((org-agenda-overriding-header "OVERDUE")
  ;                     (org-agenda-skip-function
  ;                      '(org-agenda-skip-entry-if 'notedeadline))
  ;                     (org-agenda-sorting-strategy '(priority-down))))
  ;         (agenda ""
  ;                 ((org-agenda-entry-types '(:scheduled))
  ;                  (org-agenda-overriding-header "SCHEDULED")
  ;                  (org-agenda-skip-function
  ;                   '(org-agenda-skip-entry-if 'todo 'done))
  ;                  (org-agenda-sorting-strategy
  ;                   '(priority-down time-down))
  ;                  (org-agenda-span 'day)
  ;                  (org-agenda-start-on-weekday nil)
  ;                  (org-agenda-time-grid nil)))
  ;         (todo "DONE"
  ;               ((org-agenda-overriding-header "COMPLETED"))))
  ;        ((org-agenda-format-date "")
  ;         (org-agenda-start-with-clockreport-mode nil))) t)

  ;; Show all logged state changes
  ; (setq org-agenda-log-mode-items '(state))

  ;; support shift-selection-mode
  (setq org-support-shift-select t)

  ;; Return to activate a link
  (setq org-return-follows-link t)

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
  	(holiday-fixed 12 25 "Noël")
  	;; fetes a date variable
  	(holiday-easter-etc 0 "Pâques")
  	(holiday-easter-etc 1 "Lundi de Pâques")
  	(holiday-easter-etc 39 "Ascension")
  	(holiday-easter-etc 49 "Pentecôte")
  	(holiday-easter-etc 50 "Lundi de Pentecôte")
  	;; dernier dimanche de mars/Dernier dimanche d'octobre
  	;(holiday-float 3 0 -1 "Heure d'été")
  	;(holiday-float 10 0 -1 "Heure d'hiver")
  	(holiday-float 5 0 1 "Fête des mères")
  	;; Premier dimanche de mai, ou deuxième dimanche de mai si
  	;; le premier dimanche tombe le jour de la fête du travail.
  	(holiday-float 6 0 3 "Fête des pères")
  	;; Troisième dimanche de juin
  	))

  (setq calendar-holidays holiday-french-holidays)

  ;; French date and calendar localization
  (setq calendar-week-start-day 1
        calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"]
        calendar-day-abbrev-array ["Dim" "Lun" "Mar" "Mer" "Jeu" "Ven" "Sam"]
        calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin"
                                   "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre"]
        calendar-month-abbrev-array ["Jan" "Fév" "Mar" "Avr" "Mai" "Jun"
                                     "Jul" "Aoû" "Sep" "Oct" "Nov" "Déc"])

  ;; For date selection start on Mondays
  (setq calendar-week-start-day 1)

  ;; Set French date format for org-agenda
  (setq system-time-locale "fr_FR.UTF-8")

  ;; Tasks
  ;; State
  (setq org-todo-keywords
        '((sequence "NEXT(n)" "TODO(t)" "WAIT(w@/!)" "BACKLOG(b)" "|" "INACTIVE(i@)" "DELEGATED(g@)" "CANCELED(c@)" "DONE(d!)")))

  ;; Custom colors for the keywords
  ; (setq org-todo-keyword-faces
  ; '(("TODO" :foreground "red" :weight bold)
  ; ("NEXT" :foreground "orange" :weight bold)
  ; ("WAIT" :foreground "black" :weight bold)
  ; ("DONE" :foreground "forest green" :weight bold)
  ; ("CANCELLED" :foreground "forest green" :weight bold)))

  ;; State triggers
  ; (setq org-todo-state-tags-triggers
  ;       (quote (("CANCELLED" ("CANCELLED" . t))
  ;               ("WAIT" ("WAIT" . t))
  ;               ("HOLD" ("WAIT") ("HOLD" . t))
  ;               (done ("WAIT") ("HOLD"))
  ;               ("TODO" ("WAIT") ("CANCELLED") ("HOLD"))
  ;               ("NEXT" ("WAIT") ("CANCELLED") ("HOLD"))
  ;               ("DONE" ("WAIT") ("CANCELLED") ("HOLD")))))

  (setq org-use-fast-todo-selection t)
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Clockning
  ;; Resume clocking task when emacs is restarted
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

  ; Export
  ;; (setq org-html-html5-fancy t)
  ;; (setq org-html-doctype "html5")
  ;; (setq org-html-validation-link nil)
  ;; (defun my-org-inline-css-hook (exporter)
  ;;   "Insert custom inline css"
  ;;   (when (eq exporter 'html)
  ;;     (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
  ;;            (path (concat dir "style.css"))
  ;;            (homestyle (or (null dir) (null (file-exists-p path))))
  ;;            (final (if homestyle "~/.emacs.d/org-style.css" path)))
  ;;       (setq org-html-head-include-default-style nil)
  ;;       (setq org-html-head (concat
  ;;                            "<style type=\"text/css\">\n"
  ;;                            "<!--/*--><![CDATA[/*><!--*/\n"
  ;;                            (with-temp-buffer
  ;;                              (insert-file-contents final)
  ;;                              (buffer-string))
  ;;                            "/*]]>*/-->\n"
  ;;                            "</style>\n")))))
  ;; (add-hook 'org-export-before-processing-hook 'my-org-inline-css-hook)

  ;; Bindings
  (global-set-key (kbd "<f6>") 'cfw:open-org-calendar)
  ;(global-set-key (kbd "<f5>") (org-insert-time-stamp nil t))
  (global-set-key (kbd "<f5>") (lambda () (interactive)
  	(org-insert-time-stamp nil t)))
  ;(global-set-key (kbd "<f5>") (org-insert-time-stamp (current-time)))
  ;(global-set-key (kbd "<f6>") 'org-capture)
  (global-set-key (kbd "<f7>") 'org-clock-in)
  (global-set-key (kbd "<f8>") 'org-clock-out)
  (global-set-key (kbd "<f9>") 'org-agenda)
  ;(global-set-key (kbd "C-c a") 'org-agenda)
  ;(org-defkey org-mode-map (kbd "C-e") 'buffer-menu)
  ; (org-defkey org-mode-map (kbd "C-e") 'helm-buffers-list)
)
