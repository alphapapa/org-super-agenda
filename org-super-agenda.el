;;; Commentary:

;; To try this out, evaluate this file and run this code to open an example:

;; (let ((org-agenda-custom-commands
;;        (list '("u" "SUPER Agenda"
;;                org-super-agenda ""
;;                ((org-agenda-super-filters '(
;;                                             ;; Optionally specify the section name
;;                                             (:name "Schedule" :fn osa/filter-time)
;;                                             (:name "Bills" :fn osa/filter-any-tag
;;                                                    ;; One arg may be given alone, without a list
;;                                                    :args "bills")
;;                                             ;; This function needs no args so it may be specified like this
;;                                             osa/filter-habit
;;                                             ;; Filter functions supply their own section names when none are given
;;                                             (:fn osa/filter-todo-keyword :args "WAITING")
;;                                             (:fn osa/filter-todo-keyword
;;                                                  ;; Multiple args given in a list
;;                                                  :args ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                                                  ;; Show this section at the end of the agenda.  If you specified
;;                                                  ;; this filter last, items with these todo keywords that have
;;                                                  ;; priority A, B, or C would be displayed in those sections
;;                                                  ;; instead, because items are filtered out in the order the
;;                                                  ;; filters are listed.
;;                                                  :last t)
;;                                             (:fn osa/filter-priority :args "A")
;;                                             (:fn osa/filter-priority :args ("B" "C"))))
;;                 (org-agenda-span 'day))))))
;;   (org-agenda nil "u"))

;; You can adjust the `super-filters' to create as many different sections as you like.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'org)
(require 'cl-lib)
(require 's)

;;;; Filter macro and functions

(cl-defmacro osa/deffilter (name docstring &key section-name test)
  "Define an agenda-item filter function.
NAME is a symbol that will be appended to `osa/filter-' to
construct the name of the filter function.

DOCSTRING is a string used for the function's docstring.

:TEST is a lisp form that is run for each item, with the variable
`item' available.  Items passing this test are filtered into a
separate list.

:SECTION-NAME is a string or a lisp form that is run once, with
the variable `items' available.

Finally a list of three items is returned to the calling
function, with the value returned by :SECTION-NAME as the first
item, a list of items matching the :TEST as the second, and a
list of items not matching as the third."
  (declare (indent defun))
  (let ((function-name (intern (concat "osa/filter-" (symbol-name name)))))
    `(defun ,function-name (items args)
       ,docstring
       (unless (listp args)
         (setq args (list args)))
       (cl-loop with section-name = ,section-name
                for item in items
                if ,test
                collect item into matching
                else collect item into non-matching
                finally return (list section-name non-matching matching)))))

(osa/deffilter time
  "Filter items that have a time associated.
Items with an associated timestamp that has a time (rather than
just a date) are filtered."
  :section-name "Schedule"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :test (when-let ((time (org-find-text-property-in-string 'dotime item)))
          (not (eql (org-find-text-property-in-string 'dotime item) 'time))))

(osa/deffilter any-tag
  "Filter items that match any of the given tags.
Argument may be a string or list of strings."
  :section-name (concat "Items tagged with: " (s-join " OR " args))
  :test (seq-intersection (osa/get-tags item) args))

(osa/deffilter habit
  "Filter habit items.
Habit items have a \"STYLE: habit\" Org property."
  :section-name "Habits"
  :test (org-is-habit-p (org-find-text-property-in-string 'org-marker item)))

(osa/deffilter todo-keyword
  "Filter items that match any of the given TODO keywords.
Argument may be a string or list of strings."
  :section-name (concat (s-join " and " args) " items")
  :test (cl-member (org-find-text-property-in-string 'todo-state item) args :test 'string=))

(osa/deffilter priority
  "Filter items that match any of the given priorities.
Argument may be a string or list of strings, which should be,
e.g. \"A\" or (\"B\" \"C\")."
  :section-name (concat "Priority " (s-join " and " args) " items")
  :test (cl-member (osa/get-priority-cookie item) args :test 'string=))

;;;; Agenda command

(defun org-super-agenda (&optional arg start-day span with-hour)
  "This function is a copy of `org-agenda-list' which filters according to `org-agenda-super-filters'.
It is otherwise identical.

Produce a daily/weekly view from all files in variable `org-agenda-files'.
The view will be for the current day or week, but from the overview buffer
you will be able to go to other days/weeks.

With a numeric prefix argument in an interactive call, the agenda will
span ARG days.  Lisp programs should instead specify SPAN to change
the number of days.  SPAN defaults to `org-agenda-span'.

START-DAY defaults to TODAY, or to the most recent match for the weekday
given in `org-agenda-start-on-weekday'.

When WITH-HOUR is non-nil, only include scheduled and deadline
items if they have an hour specification like [h]h:mm."
  (interactive "P")
  (if org-agenda-overriding-arguments
      (setq arg (car org-agenda-overriding-arguments)
	    start-day (nth 1 org-agenda-overriding-arguments)
	    span (nth 2 org-agenda-overriding-arguments)))
  (if (and (integerp arg) (> arg 0))
      (setq span arg arg nil))
  (catch 'exit
    (setq org-agenda-buffer-name
	  (or org-agenda-buffer-tmp-name
	      (if org-agenda-sticky
		  (cond ((and org-keys (stringp org-match))
			 (format "*Org Agenda(%s:%s)*" org-keys org-match))
			(org-keys
			 (format "*Org Agenda(%s)*" org-keys))
			(t "*Org Agenda(a)*")))
	      org-agenda-buffer-name))
    (org-agenda-prepare "Day/Week")
    (setq start-day (or start-day org-agenda-start-day))
    (if (stringp start-day)
	;; Convert to an absolute day number
	(setq start-day (time-to-days (org-read-date nil t start-day))))
    (org-compile-prefix-format 'agenda)
    (org-set-sorting-strategy 'agenda)
    (let* ((span (org-agenda-ndays-to-span (or span org-agenda-span)))
	   (today (org-today))
	   (sd (or start-day today))
	   (ndays (org-agenda-span-to-ndays span sd))
	   (org-agenda-start-on-weekday
	    (if (or (eq ndays 7) (eq ndays 14))
		org-agenda-start-on-weekday))
	   (thefiles (org-agenda-files nil 'ifmode))
	   (files thefiles)
	   (start (if (or (null org-agenda-start-on-weekday)
			  (< ndays 7))
		      sd
		    (let* ((nt (calendar-day-of-week
				(calendar-gregorian-from-absolute sd)))
			   (n1 org-agenda-start-on-weekday)
			   (d (- nt n1)))
		      (- sd (+ (if (< d 0) 7 0) d)))))
	   (day-numbers (list start))
	   (day-cnt 0)
	   (inhibit-redisplay (not debug-on-error))
	   (org-agenda-show-log-scoped org-agenda-show-log)
	   s e rtn rtnall file date d start-pos end-pos todayp
	   clocktable-start clocktable-end filter)
      (setq org-agenda-redo-command
	    (list 'org-agenda-list (list 'quote arg) start-day (list 'quote span) with-hour))
      (dotimes (n (1- ndays))
	(push (1+ (car day-numbers)) day-numbers))
      (setq day-numbers (nreverse day-numbers))
      (setq clocktable-start (car day-numbers)
	    clocktable-end (1+ (or (org-last day-numbers) 0)))
      (setq-local org-starting-day (car day-numbers))
      (setq-local org-arg-loc arg)
      (setq-local org-agenda-current-span (org-agenda-ndays-to-span span))
      (unless org-agenda-compact-blocks
	(let* ((d1 (car day-numbers))
	       (d2 (org-last day-numbers))
	       (w1 (org-days-to-iso-week d1))
	       (w2 (org-days-to-iso-week d2)))
	  (setq s (point))
	  (if org-agenda-overriding-header
	      (insert (org-add-props (copy-sequence org-agenda-overriding-header)
			  nil 'face 'org-agenda-structure) "\n")
	    (insert (org-agenda-span-name span)
		    "-agenda"
		    (if (< (- d2 d1) 350)
			(if (= w1 w2)
			    (format " (W%02d)" w1)
			  (format " (W%02d-W%02d)" w1 w2))
		      "")
		    ":\n")))
	(add-text-properties s (1- (point)) (list 'face 'org-agenda-structure
						  'org-date-line t))
	(org-agenda-mark-header-line s))
      (while (setq d (pop day-numbers))
	(setq date (calendar-gregorian-from-absolute d)
	      s (point))
	(if (or (setq todayp (= d today))
		(and (not start-pos) (= d sd)))
	    (setq start-pos (point))
	  (if (and start-pos (not end-pos))
	      (setq end-pos (point))))
	(setq files thefiles
	      rtnall nil)
	(while (setq file (pop files))
	  (catch 'nextfile
	    (org-check-agenda-file file)
	    (let ((org-agenda-entry-types org-agenda-entry-types))
	      ;; Starred types override non-starred equivalents
	      (when (member :deadline* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :deadline org-agenda-entry-types)))
	      (when (member :scheduled* org-agenda-entry-types)
		(setq org-agenda-entry-types
		      (delq :scheduled org-agenda-entry-types)))
	      ;; Honor with-hour
	      (when with-hour
		(when (member :deadline org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :deadline org-agenda-entry-types))
		  (push :deadline* org-agenda-entry-types))
		(when (member :scheduled org-agenda-entry-types)
		  (setq org-agenda-entry-types
			(delq :scheduled org-agenda-entry-types))
		  (push :scheduled* org-agenda-entry-types)))
	      (unless org-agenda-include-deadlines
		(setq org-agenda-entry-types
		      (delq :deadline* (delq :deadline org-agenda-entry-types))))
	      (cond
	       ((memq org-agenda-show-log-scoped '(only clockcheck))
		(setq rtn (org-agenda-get-day-entries
			   file date :closed)))
	       (org-agenda-show-log-scoped
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 (append '(:closed) org-agenda-entry-types))))
	       (t
		(setq rtn (apply 'org-agenda-get-day-entries
				 file date
				 org-agenda-entry-types)))))
	    (setq rtnall (append rtnall rtn)))) ;; all entries
	(if org-agenda-include-diary
	    (let ((org-agenda-search-headline-for-time t))
	      (require 'diary-lib)
	      (setq rtn (org-get-entries-from-diary date))
	      (setq rtnall (append rtnall rtn))))
	(if (or rtnall org-agenda-show-all-dates)
	    (progn
	      (setq day-cnt (1+ day-cnt))
	      (insert
	       (if (stringp org-agenda-format-date)
		   (format-time-string org-agenda-format-date
				       (org-time-from-absolute date))
		 (funcall org-agenda-format-date date))
	       "\n")
	      (put-text-property s (1- (point)) 'face
				 (org-agenda-get-day-face date))
	      (put-text-property s (1- (point)) 'org-date-line t)
	      (put-text-property s (1- (point)) 'org-agenda-date-header t)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt)
	      (when todayp
		(put-text-property s (1- (point)) 'org-today t))
	      (setq rtnall
		    (org-agenda-add-time-grid-maybe rtnall ndays todayp))
              ;; Insert items
	      (when rtnall
                (osa/insert-sections rtnall))
	      (put-text-property s (1- (point)) 'day d)
	      (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))
      (when (and org-agenda-clockreport-mode clocktable-start)
	(let ((org-agenda-files (org-agenda-files nil 'ifmode))
	      ;; the above line is to ensure the restricted range!
	      (p (copy-sequence org-agenda-clockreport-parameter-plist))
	      tbl)
	  (setq p (org-plist-delete p :block))
	  (setq p (plist-put p :tstart clocktable-start))
	  (setq p (plist-put p :tend clocktable-end))
	  (setq p (plist-put p :scope 'agenda))
	  (setq tbl (apply 'org-clock-get-clocktable p))
	  (insert tbl)))
      (goto-char (point-min))
      (or org-agenda-multi (org-agenda-fit-window-to-buffer))
      (unless (and (pos-visible-in-window-p (point-min))
		   (pos-visible-in-window-p (point-max)))
	(goto-char (1- (point-max)))
	(recenter -1)
	(if (not (pos-visible-in-window-p (or start-pos 1)))
	    (progn
	      (goto-char (or start-pos 1))
	      (recenter 1))))
      (goto-char (or start-pos 1))
      (add-text-properties (point-min) (point-max)
			   `(org-agenda-type agenda
					     org-last-args (,arg ,start-day ,span)
					     org-redo-cmd ,org-agenda-redo-command
					     org-series-cmd ,org-cmd))
      (if (eq org-agenda-show-log-scoped 'clockcheck)
	  (org-agenda-show-clocking-issues))
      (org-agenda-finalize)
      (setq buffer-read-only t)
      (message ""))))

;;;;; Support functions

(defun osa/insert-sections (all-items)
  "Divide ALL-ITEMS into sections and insert them into the agenda."
  ;; This essentially replaces the part of `org-agenda-list' that
  ;; finally inserts the `rtnall' variable.
  (if (bound-and-true-p org-agenda-super-filters)
      (cl-loop with filter-fn
               with args
               with custom-section-name
               for filter in org-agenda-super-filters
               if (functionp filter) do (setq custom-section-name nil
                                              filter-fn filter
                                              args nil
                                              last nil)
               else do (setq custom-section-name (plist-get filter :name)
                             filter-fn (plist-get filter :fn)
                             args (plist-get filter :args)
                             last (plist-get filter :last))
               for (auto-section-name non-matching matching) = (funcall filter-fn all-items args)
               for section-name = (or custom-section-name auto-section-name)

               ;; FIXME: This repetition is kind of ugly, but I guess cl-loop is worth it...
               if last collect (cons section-name matching) into last-sections
               and do (setq all-items non-matching)
               else collect (cons section-name matching) into sections
               and do (setq all-items non-matching)

               finally do
               (progn
                 ;; Insert sections
                 (cl-loop for (section-name . items) in sections
                          when items
                          do (progn
                               (osa/insert-agenda-header section-name)
                               (insert (org-agenda-finalize-entries items 'agenda)
                                       "\n\n")))
                 (when non-matching
                   ;; Insert non-matching items in main section
                   (osa/insert-agenda-header "Other items")
                   (insert (org-agenda-finalize-entries non-matching 'agenda)
                           "\n\n"))

                 ;; Insert final sections
                 (cl-loop for (section-name . items) in last-sections
                          when items
                          do (progn
                               (osa/insert-agenda-header section-name)
                               (insert (org-agenda-finalize-entries items 'agenda)
                                       "\n\n")))))
    ;; No super-filters; insert normally
    (insert (org-agenda-finalize-entries all-items 'agenda)
            "\n")))

(defun osa/get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defun osa/insert-agenda-header (s)
  "Insert agenda header into current buffer containing string S and a newline."
  (insert (org-add-props s nil 'face 'org-agenda-structure) "\n"))

(defun osa/get-priority-cookie (s)
  "Return priority character for string S.
Matches `org-priority-regexp'."
  (when (string-match org-priority-regexp s)
    (match-string-no-properties 2 item)))
