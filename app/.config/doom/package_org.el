;; Org
;; -----------------------------------------------------------------------------

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(if (file-directory-p "/data/doc/org")
	; at home
	(setq org-directory "/data/doc/org")
	; at work
	(setq org-directory "/data/work/org"))
(setq org-default-notes-file (concat org-directory "/todo.org"))


;; Collect all .org from my Org directory and subdirs
(load-library "find-lisp")
(if (file-directory-p "/data/doc/org")
	; at home (more than just-org directory to include work folder)
	;(setq org-agenda-files (find-lisp-find-files "/data/doc" "\\.org$"))
	; at home (just-org directory to exclude work folder)
	(setq org-agenda-files (find-lisp-find-files org-directory "\\.org$"))
	; at work
	(setq org-agenda-files (find-lisp-find-files org-directory "\\.org$")))

;; Org mode on start-up
;; (add-hook 'after-init-hook 'org-agenda-list)
(setq initial-buffer-choice (lambda ()
	(if (file-directory-p "/data/doc/org")
	  ; at home
	  (org-agenda nil "e")
	  ; at work
	  (org-agenda nil "s"))
  (get-buffer "*Org Agenda*")))

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
        ((org-agenda-overriding-header "AGENDA")
          (org-agenda-span 7)
          (org-agenda-start-day "today")
          (org-agenda-start-on-weekday nil)
          (org-agenda-time-grid nil)))
    (todo "NEXT"
      ((org-agenda-sorting-strategy '(priority-down todo-state-down))
        (org-agenda-skip-entry-if 'scheduled)
        (org-agenda-overriding-header "NEXT")))
    (todo "TODO"
      ((org-agenda-sorting-strategy '(priority-down todo-state-down))
        (org-agenda-overriding-header "TODO")))
    (todo "WAIT"
      ((org-agenda-sorting-strategy '(priority-down todo-state-down))
        (org-agenda-skip-entry-if 'scheduled)
        (org-agenda-overriding-header "WAIT"))))))

(defun org-agenda-skip-tag (tag &optional others)
  "Skip all entries that correspond to TAG.
  If OTHERS is true, skip all entries that do not correspond to TAG."
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (if others
        (if (not (member tag (org-get-tags-at current-headline)))
            next-headline
          nil)
      (if (member tag (org-get-tags-at current-headline))
          next-headline
        nil))))

;; Family agenda (for exports)
(add-to-list 'org-agenda-custom-commands
  '("f" "Family Agenda"
    ((agenda ""
        ((org-agenda-overriding-header "")
          (org-agenda-span 7)
          (org-agenda-start-day "today")
          (org-agenda-skip-function '(org-agenda-skip-tag "famille" 't))
          (org-agenda-remove-tags t)
          (org-agenda-start-on-weekday nil)
          (org-agenda-time-grid nil)
          (org-agenda-show-all-dates nil)))
    (tags-todo "+famille-SCHEDULED={.+}-DEADLINE={.+}/+TODO|+NEXT"
      ((org-agenda-sorting-strategy '(priority-down todo-state-down))
        (org-agenda-skip-entry-if 'scheduled)
	    	(org-agenda-remove-tags t)
        ; org-agenda-tag-filter-preset doesn't work in -batch ?!
		    ;(org-agenda-tag-filter-preset '("+famille" "-SCHEDULE"))
        (org-agenda-overriding-header "NEXT TODOS"))))))

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
     '("t" "Today"
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

;; French localization
(setq calendar-week-start-day 1
  calendar-day-name-array ["Dimanche" "Lundi" "Mardi" "Mercredi" "Jeudi" "Vendredi" "Samedi"]
  calendar-month-name-array ["Janvier" "Février" "Mars" "Avril" "Mai" "Juin" "Juillet" "Août" "Septembre" "Octobre" "Novembre" "Décembre"])

;; Warn me of any deadlines in next 7 days
(setq org-deadline-warning-days 7)

;; Show me tasks scheduled or due in next fortnight
; (setq org-agenda-span (quote fortnight))

;; Agenda : do not dim blocked tasks
; (setq org-agenda-dim-blocked-tasks nil)

;; Compact the block agenda view
; (setq org-agenda-compact-blocks t)

;; Agenda style (colors: http://www.raebear.net/computers/emacs-colors/)
;(setq org-agenda-date-today '(:foreground "yellow"))
;(setq org-agenda-date-weekend '(:foreground "green"))
;(set-face-attribute 'org-agenda-date-today (:foreground "green"))
;; (defun my-org-agenda-get-day-face-fn (date)
;; "Return the face DATE should be displayed with."
;;   (let ((day-of-week (calendar-day-of-week date)))
;;     (cond
;;       ((org-agenda-todayp date)
;;         ;'org-agenda-date-today)
;;         '(:foreground "LightGoldenrod"))
;;       ;((member day-of-week org-agenda-weekend-days)
;;       ; 'org-agenda-date-weekend)
;;       ((or (= day-of-week 6) (= day-of-week 0))
;;         '(:foreground "DarkSeaGreen2"))
;;       (t 'org-agenda-date))))
;; (setq org-agenda-day-face-function 'my-org-agenda-get-day-face-fn)

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
;; Tracking TODO state changes (https://orgmode.org/manual/Tracking-TODO-state-changes.html) :
;; - '!' for a timestamp
;; - '@' for a note with timestamp
;; - '@/!' in addition to the note taken when entering the state, a timestamp should be recorded when leaving the state, if the target state does not configure logging for entering it
(setq org-todo-keywords
       '(
       	;; Sequence for TASKS
       	(sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "BACKLOG(b@/!)" "|" "DONE(d!)" "CANCELED(c@)" "DELEGATED(g@)" "INACTIVE(i@)")
       	;; Sequence for EVENTS
       	;;(sequence "VISIT(v@/!)" "|" "DIDNOTGO(z@/!)" "MEETING(m@/!)" "VISITED(y@/!)")
       	))

(setq org-capture-templates
 '(("t" "Todo" entry (file+headline org-default-notes-file "Idea box")
        "* TODO %?\n %i" :empty-lines 1 :prepend t)
   ("b" "Bookmark" entry (file (concat org-directory "/bookmark.org"))
        "* %?\n  :PROPERTIES:\n  :CREATED: %U\n  :END:\n\n" :empty-lines 1 :prepend t)
   ("j" "Journal" entry (file (concat org-directory "/journal.org.gpg"))
   	    "* %U %?\n  :PROPERTIES:\n  :CREATED: %T\n  :END:\n\n" :empty-lines 1 :prepend t)))

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
(org-defkey org-mode-map (kbd "C-e") 'helm-buffers-list)
