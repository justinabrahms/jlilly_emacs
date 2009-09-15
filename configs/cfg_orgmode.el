;; Configurations for orgmode
(add-to-list 'load-path "~/.emacs.d/vendor/org-mode/lisp")
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; hide the initial stars. they're distracting
(setq org-hide-leading-stars t)

(setq org-return-follows-link t)

;; The abbrev list allows me to insert links like
;; [[foo:google]]
;; which will google for "foo"
(setq org-link-abbrev-alist
      '(("google"  . "http://www.google.com/search?q=")))



;; set state changes. pattern is (state-change (tag . flag))
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t) ("NEXT"))
              ("SOMEDAY" ("WAITING" . t))
              (done ("NEXT") ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("STARTED" ("WAITING"))
              ("PROJECT" ("CANCELLED") ("PROJECT" . t)))))


;; resume clocking time when emacs is started
(setq org-clock-persistence-insinuate)
;; Don't put that file in my checkin. Put it with the other private stuff
(setq org-clock-persist-file "~/private/org/org-clock-save.el")

;; Yes it's long... but more is better ;)
(setq org-clock-history-length 35)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change task state to STARTED when clocking in
(setq org-clock-in-switch-to-state "STARTED")
;; Save clock data and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
; Set default column view headings: Task Effort Clock_Summary
(setq org-columns-default-format "%80ITEM(Task) %10Effort(Effort){:} %10CLOCKSUM")
; global Effort estimate values
(setq org-global-properties (quote (("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00 8:00"))))
; generate unique attachment id's
(setq org-id-method (quote uuidgen))
; copy org attachments
(setq org-attach-method 'cp)
; set copy directory
(setq org-attach-directory "~/private/org/data")
; underline the line in the agenda that you are on
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
 
(add-hook 'org-clock-in-prepare-hook 
          'my-org-mode-ask-effort)

; use org-clock-in-prepare-hook to add an effort estimate. 
; This way you can easily have a "tea-timer" for your tasks when they don't 
; already have an effort estimate.
(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort 
           (completing-read 
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))

;; set up remember to use org mode
(org-remember-insinuate)
(setq org-directory "~/private/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)

(setq org-agenda-files (quote ("~/private/org")))

;; Start clock if a remember buffer includes :CLOCK-IN:
(add-hook 'remember-mode-hook 'my-start-clock-if-needed 'append)

(defun my-start-clock-if-needed ()
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward " *:CLOCK-IN: *" nil t)
      (replace-match "")
      (org-clock-in))))

;; Keep clocks running
(setq org-remember-clock-out-on-exit nil)
 
;; C-c C-c stores the note immediately
(setq org-remember-store-without-prompt t)
 
;;; I don't use this -- but set it in case I forget to specify a location in a future template
;;;(setq org-remember-default-headline "Tasks")

;;
;; REFILES
;;
; Use IDO for target completion
(setq org-completion-use-ido t)
 
; Targets include this file and any file contributing to the agenda - up to 5 levels deep
(setq org-refile-targets (quote ((org-agenda-files :maxlevel . 5) (nil :maxlevel . 5))))
 
; Targets start with the file name - allows creating level 1 tasks
(setq org-refile-use-outline-path (quote file))

;;
;; CUSTOM AGENDAS
;;
(setq org-agenda-custom-commands 
      (quote (("P" "Projects" tags "/!PROJECT" ((org-use-tag-inheritance nil)))
              ("s" "Started Tasks" todo "STARTED" ((org-agenda-todo-ignore-with-date nil)))
              ("w" "Tasks waiting on something" tags "WAITING" ((org-use-tag-inheritance nil)))
              ("r" "Refile New Notes and Tasks" tags "REFILE" ((org-agenda-todo-ignore-with-date nil)))
              ("n" "Notes" tags "NOTES" nil))))

 ; Tags with fast selection keys
(setq org-tag-alist (quote ((:startgroup)
                            ("@Errand" . ?e)
                            ("@Work" . ?w)
                            ("@Home" . ?h)
                            ("@Phone" . ?p)
                            ("@Mind" . ?m)
                            ("@Studio" . ?s)
                            (:endgroup)
                            ("NEXT" . ?N)
                            ("PROJECT" . ?P)
                            ("WAITING" . ?W)
                            ("HOME" . ?H)
                            ("ORG" . ?O)
                            ("PLAY" . ?p)
                            ("R&D" . ?r)
                            ("MIND" . ?m)
                            ("STUDIO" . ?S)
                            ("CANCELLED" . ?C))))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

; Erase all reminders and rebuilt reminders for today from the agenda
(defun my-org-agenda-to-appt ()
  (interactive)
  (setq appt-time-msg-list nil)
  (org-agenda-to-appt))
 
; Rebuild the reminders everytime the agenda is displayed
(add-hook 'org-finalize-agenda-hook 'my-org-agenda-to-appt)

; If we leave Emacs running overnight - reset the appointments one minute after midnight
(run-at-time "24:01" nil 'my-org-agenda-to-appt)

;;
;; NARROW THE VIEW TO A SUBTREE
;;
(global-set-key (kbd "<f5>") 'my-org-todo)
 
(defun my-org-todo ()
  (interactive)
  (org-narrow-to-subtree)
  (org-show-todo-tree nil)
  (widen))

(setq org-use-fast-todo-selection t)

;;
;; Remove Tasks With Dates From The Global Todo Lists
;;
;; Keep tasks with dates off the global todo lists
(setq org-agenda-todo-ignore-with-date t)
 
;; Remove completed deadline tasks from the agenda view
(setq org-agenda-skip-deadline-if-done t)
 
;; Remove completed scheduled tasks from the agenda view
(setq org-agenda-skip-scheduled-if-done t)

;; ask me for a note when I mark something as done
(setq org-log-done 'note)


;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)
 
;; Show all agenda dates - even if they are empty
(setq org-agenda-show-all-dates t)
 
;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down effort-up category-up)
              (todo priority-down)
              (tags priority-down))))
 
;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)
 
;; Disable display of the time grid
(setq org-agenda-time-grid
      (quote (nil "----------------"
                  (800 1000 1200 1400 1600 1800 2000))))
 
;; save all org files every minute
(run-at-time "00:59" 3600 'org-save-all-org-buffers)

;; Custom Key Bindings
(global-set-key (kbd "<f12>") 'org-agenda)
(global-set-key (kbd "<f5>") 'my-org-todo)
(global-set-key (kbd "<S-f5>") 'widen)
(global-set-key (kbd "<f7>") 'set-truncate-lines)
(global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
(global-set-key (kbd "<f9> c") 'calendar)
(global-set-key (kbd "<f9> f") 'boxquote-insert-file)
(global-set-key (kbd "<f9> i") (lambda ()
                                 (interactive)
                                 (info "~/git/org-mode/doc/org.info")))
(global-set-key (kbd "<f9> o") 'org-occur)
(global-set-key (kbd "<f9> r") 'boxquote-region)
(global-set-key (kbd "<f9> u") (lambda ()
                                 (interactive)
                                 (untabify (point-min) (point-max))))
(global-set-key (kbd "<f9> v") 'visible-mode)
(global-set-key (kbd "C-<f9>") 'previous-buffer)
(global-set-key (kbd "C-x n r") 'narrow-to-region)
(global-set-key (kbd "C-<f10>") 'next-buffer)
(global-set-key (kbd "<f11>") 'org-clock-goto)
(global-set-key (kbd "C-s-<f12>") 'my-save-then-publish)


;; several of the methods that begin with 'sasha' come from
;; http://sachachua.com/wp/2007/12/22/a-day-in-a-life-with-org/

;; This stuff seems useful, but is a bit over my head for the moment.
;;
;; (defun sacha/org-show-load ()
;;   "Show my unscheduled time and free time for the day."
;;   (interactive)
;;   (let ((time (sacha/org-calculate-free-time
;;                ;; today
;;                (calendar-gregorian-from-absolute (time-to-days (current-time)))
;;                ;; now
;;                (let* ((now (decode-time))
;;                       (cur-hour (nth 2 now))
;;                       (cur-min (nth 1 now)))
;;                  (+ (* cur-hour 60) cur-min))
;;                ;; until the last time in my time grid
;;                (let ((last (car (last (elt org-agenda-time-grid 2)))))
;;                  (+ (* (/ last 100) 60) (% last 100))))))
;;     (message "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
;;             (/ (car time) (* .01 (cdr time)))
;;             (car time)
;;             (cdr time)
;;             (- (cdr time) (car time)))))

;; (defun sacha/org-agenda-load (match)
;;   "Can be included in `org-agenda-custom-commands'."
;;   (let ((inhibit-read-only t)
;;         (time (sacha/org-calculate-free-time
;;                ;; today
;;                (calendar-gregorian-from-absolute org-starting-day)
;;                ;; now if today, else start of day
;;                (if (= org-starting-day
;;                       (time-to-days (current-time)))
;;                    (let* ((now (decode-time))
;;                           (cur-hour (nth 2 now))
;;                           (cur-min (nth 1 now)))
;;                      (+ (* cur-hour 60) cur-min))
;;                  (let ((start (car (elt org-agenda-time-grid 2))))
;;                    (+ (* (/ start 100) 60) (% start 100))))
;;                  ;; until the last time in my time grid
;;                (let ((last (car (last (elt org-agenda-time-grid 2)))))
;;                  (+ (* (/ last 100) 60) (% last 100))))))
;;     (goto-char (point-max))
;;     (insert (format
;;              "%.1f%% load: %d minutes to be scheduled, %d minutes free, %d minutes gap\n"
;;              (/ (car time) (* .01 (cdr time)))
;;              (car time)
;;              (cdr time)
;;              (- (cdr time) (car time))))))

;; (defun sacha/org-calculate-free-time (date start-time end-of-day)
;;   "Return a cons cell of the form (TASK-TIME . FREE-TIME) for DATE, given START-TIME and END-OF-DAY.
;; DATE is a list of the form (MONTH DAY YEAR).
;; START-TIME and END-OF-DAY are the number of minutes past midnight."
;;   (save-window-excursion
;;   (let ((files org-agenda-files)
;;         (total-unscheduled 0)
;;         (total-gap 0)
;;         file
;;         rtn
;;         rtnall
;;         entry
;;         (last-timestamp start-time)
;;         scheduled-entries)
;;     (while (setq file (car files))
;;       (catch 'nextfile
;;         (org-check-agenda-file file)
;;         (setq rtn (org-agenda-get-day-entries file date :scheduled :timestamp))
;;         (setq rtnall (append rtnall rtn)))
;;       (setq files (cdr files)))
;;     ;; For each item on the list
;;     (while (setq entry (car rtnall))
;;       (let ((time (get-text-property 1 'time entry)))
;;         (cond
;;          ((and time (string-match "\\([^-]+\\)-\\([^-]+\\)" time))
;;           (setq scheduled-entries (cons (cons
;;                                          (save-match-data (appt-convert-time (match-string 1 time)))
;;                                          (save-match-data (appt-convert-time (match-string 2 time))))
;;                                         scheduled-entries)))
;;          ((and time
;;                (string-match "\\([^-]+\\)\\.+" time)
;;                (string-match "^[A-Z]+ \\(\\[#[A-Z]\\]\\)? \\([0-9]+\\)" (get-text-property 1 'txt entry)))
;;           (setq scheduled-entries
;;                 (let ((start (and (string-match "\\([^-]+\\)\\.+" time)
;;                                  (appt-convert-time (match-string 1 time)))))
;;                   (cons (cons start
;;                               (and (string-match "^[A-Z]+ \\(\\[#[A-Z]\\]\\)? \\([0-9]+\\) " (get-text-property 1 'txt entry))
;;                                    (+ start (string-to-number (match-string 2 (get-text-property 1 'txt entry))))))
;;                         scheduled-entries))))
;;          ((string-match "^[A-Z]+ \\([0-9]+\\)" (get-text-property 1 'txt entry))
;;           (setq total-unscheduled (+ (string-to-number
;;                                       (match-string 1 (get-text-property 1 'txt entry)))
;;                                      total-unscheduled)))))
;;       (setq rtnall (cdr rtnall)))
;;     ;; Sort the scheduled entries by time
;;     (setq scheduled-entries (sort scheduled-entries (lambda (a b) (< (car a) (car b)))))

;;     (while scheduled-entries
;;       (let ((start (car (car scheduled-entries)))
;;             (end (cdr (car scheduled-entries))))
;;       (cond
;;        ;; are we in the middle of this timeslot?
;;        ((and (>= last-timestamp start)
;;              (< = last-timestamp end))
;;         ;; move timestamp later, no change to time
;;         (setq last-timestamp end))
;;        ;; are we completely before this timeslot?
;;        ((< last-timestamp start)
;;         ;; add gap to total, skip to the end
;;         (setq total-gap (+ (- start last-timestamp) total-gap))
;;         (setq last-timestamp end)))
;;       (setq scheduled-entries (cdr scheduled-entries))))
;;     (if (< last-timestamp end-of-day)
;;         (setq total-gap (+ (- end-of-day last-timestamp) total-gap)))
;;     (cons total-unscheduled total-gap))))

;; (defun sacha/org-clock-in-if-starting ()
;;   "Clock in when the task is marked STARTED."
;;   (when (and (string= state "STARTED")
;;              (not (string= last-state state)))
;;     (org-clock-in)))
;; (add-hook 'org-after-todo-state-change-hook
;; 	  'sacha/org-clock-in-if-starting)
;; (defadvice org-clock-in (after sacha activate)
;;   "Set this task's status to 'STARTED'."
;;   (org-todo "STARTED"))

;; (defun sacha/org-clock-out-if-waiting ()
;;   "Clock in when the task is marked STARTED."
;;   (when (and (string= state "WAITING")
;;              (not (string= last-state state)))
;;     (org-clock-out)))
;; (add-hook 'org-after-todo-state-change-hook
;; 	  'sacha/org-clock-out-if-waiting)

;; (defun sacha/org-agenda-clock (match)
;;   ;; Find out when today is
;;   (let* ((inhibit-read-only t))
;;     (goto-char (point-max))
;;     (org-dblock-write:clocktable
;;      `(:scope agenda
;;        :maxlevel 4
;;        :tstart ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (1+ org-starting-day) 0))
;;        :tend ,(format-time-string "%Y-%m-%d" (calendar-time-from-absolute (+ org-starting-day 2) 0))))))

(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-custom-commands
      '(("c" "My custom agenda"
	 ((org-agenda-list nil nil 1)
;          (sacha/org-agenda-load)
	  (tags "PROJECT-WAITING")
	  (tags-todo "WAITING")
	  (tags-todo "-MAYBE")))))