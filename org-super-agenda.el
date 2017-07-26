;;; org-super-agenda.el --- Supercharge your agenda by grouping it into sections

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-super-agenda
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; This package lets you "supercharge" your Org daily/weekly agenda.
;; The idea is to group items into sections, rather than having them
;; all in one big list.

;; Now you can sort-of do this already with custom agenda commands,
;; but when you do that, you lose the daily/weekly aspect of the
;; agenda: items are no longer shown based on deadline/scheduled
;; timestamps, but are shown no-matter-what.

;; So this `org-super-agenda' command essentially copies the
;; `org-agenda-list' command, but right before it inserts the agenda
;; items, it runs them through a set of filters that separate them
;; into groups.  Then the groups are inserted into the agenda buffer,
;; and any remaining items are inserted at the end.  Empty groups are
;; not displayed.

;; The end result is your standard daily/weekly agenda, but arranged
;; into groups defined by you.  You might put items with certain tags
;; in one group, habits in another group, items with certain todo
;; keywords in another, and items with certain priorities in another.
;; The possibilities are only limited by the grouping functions.

;; The `org-super-agenda' command works as a custom agenda command, so
;; you can add it to your `org-agenda-custom-commands' list.  You can
;; also test it quickly like this:

;; (let ((org-agenda-custom-commands
;;        '(("u" "SUPER Agenda"
;;           org-super-agenda ""
;;           ((org-agenda-span 'day)
;;            (org-super-agenda-groups
;;             '(;; Each group has an implicit boolean OR operator between its selectors.
;;               (:name "Today"  ; Optionally specify section name
;;                      :time t  ; Items that have a time associated
;;                      :todo "TODAY")  ; Items that have this TODO keyword
;;               (:name "Important"
;;                      ;; Single arguments given alone
;;                      :any-tags "bills"
;;                      :priority "A")
;;               (:name "Food-related"
;;                      ;; Multiple args given in list
;;                      :any-tags ("food" "dinner"))
;;               (:name "Personal"
;;                      :habit t
;;                      :any-tags "personal")
;;               ;; Filter functions supply their own section names when none are given
;;               (:todo "WAITING")
;;               (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                      ;; Show this section at the end of the agenda. If you specified
;;                      ;; this filter last, items with these todo keywords that have
;;                      ;; priority A, B, or C would be displayed in those sections
;;                      ;; instead, because items are filtered out in the order the
;;                      ;; filters are listed.
;;                      :last t)
;;               (:priority ("B" "C")))))))))
;;   (org-agenda nil "u"))

;; You can adjust the `org-super-agenda-groups' to create as many different
;; groups as you like.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'org)
(require 'cl-lib)
(require 's)

;;;; Variables

(defvar org-super-agenda-group-types nil
  "List of agenda grouping keywords and associated functions.
Populated automatically by `osa/defgroup'.")

(defcustom org-super-agenda-fontify-whole-header-line nil
  "Fontify the whole line for section headers.
This is mostly useful if section headers have a highlight color, making it stretch across the screen."
  :group 'org)

;;;; Filters

(cl-defmacro osa/defgroup (name docstring &key section-name test)
  "Define an agenda-item group function.
NAME is a symbol that will be appended to `osa/group-' to
construct the name of the group function.  A symbol like `:name'
will be added to the `org-super-agenda-group-types' list, associated
with the function, which is used by the dispatcher.

DOCSTRING is a string used for the function's docstring.

:SECTION-NAME is a string or a lisp form that is run once, with
the variable `items' available.

:TEST is a lisp form that is run for each item, with the variable
`item' available.  Items passing this test are filtered into a
separate list.

Finally a list of three items is returned, with the value
returned by :SECTION-NAME as the first item, a list of items not
matching the :TEST as the second, and a list of items matching as
the third."
  (declare (indent defun))
  (let ((group-type (intern (concat ":" (symbol-name name))))
        (function-name (intern (concat "osa/group-" (symbol-name name)))))
    ;; Associate the group type with this function so the dispatcher can find it
    (setq org-super-agenda-group-types (plist-put org-super-agenda-group-types group-type function-name))
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

(osa/defgroup time
  "Group items that have a time associated.
Items with an associated timestamp that has a time (rather than
just a date) are filtered."
  :section-name "Schedule"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :test (when-let ((time (org-find-text-property-in-string 'dotime item)))
          (not (eql (org-find-text-property-in-string 'dotime item) 'time))))

(osa/defgroup any-tags
  "Group items that match any of the given tags.
Argument may be a string or list of strings."
  :section-name (concat "Items tagged with: " (s-join " OR " args))
  :test (seq-intersection (osa/get-tags item) args 'equalp))

(osa/defgroup habit
  "Group habit items.
Habit items have a \"STYLE: habit\" Org property."
  :section-name "Habits"
  :test (org-is-habit-p (org-find-text-property-in-string 'org-marker item)))

(osa/defgroup todo
  "Group items that match any of the given TODO keywords.
Argument may be a string or list of strings."
  :section-name (concat (s-join " and " args) " items")
  :test (cl-member (org-find-text-property-in-string 'todo-state item) args :test 'string=))

(osa/defgroup priority
  "Group items that match any of the given priorities.
Argument may be a string or list of strings, which should be,
e.g. \"A\" or (\"B\" \"C\")."
  :section-name (concat "Priority " (s-join " and " args) " items")
  :test (cl-member (osa/get-priority-cookie item) args :test 'string=))

(defun osa/filter-or (items filters)
  "Group ITEMS with boolean OR according to FILTERS."
  (let* ((matches (cl-loop with fn with args
                           for filter in filters
                           if (functionp filter) do (setq fn filter
                                                          args nil)
                           else do (setq fn (plist-get filter :filter)
                                         args (plist-get filter :args))
                           for (auto-section-name non-matching matching) = (funcall fn items args)
                           append matching
                           and do (setq items non-matching))))
    ;; Return results without a name
    (list nil items matches)))

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
	    (list 'org-super-agenda (list 'quote arg) start-day (list 'quote span) with-hour))
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
  (if (bound-and-true-p org-super-agenda-groups)
      (cl-loop with filter-fn
               with args
               with last
               for filter in org-super-agenda-groups
               for custom-section-name = (plist-get filter :name)
               for last = (plist-get filter :last)
               for (auto-section-name non-matching matching) = (osa/group-dispatch all-items filter)
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

(defun osa/group-dispatch (items group)
  "Group ITEMS with the appropriate grouping functions for GROUP.
Grouping functions are listed in `org-super-agenda-group-types', which
see."
  (cl-loop with name with fn with auto-section-name with non-matching with matching
           for (group-type args) on group by 'cddr  ; plist access
           for fn = (plist-get org-super-agenda-group-types group-type)
           ;; This double "when fn" is an ugly hack, but it lets us
           ;; use the destructuring-bind; otherwise we'd have to put
           ;; all the collection logic in a progn, or do the
           ;; destructuring ourselves, which would be uglier.
           when fn
           for (auto-section-name non-matching matching) = (funcall fn items args)
           when fn
           append matching into all-matches
           and collect auto-section-name into names
           and do (setq items non-matching)
           finally return (list (s-join " and " (-non-nil names)) items all-matches)))

(defsubst osa/get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defsubst osa/insert-agenda-header (s)
  "Insert agenda header into current buffer containing string S and a newline."
  (let ((start (point)))
    (insert (org-add-props s nil 'face 'org-agenda-structure))
    (insert "\n")
    (when org-super-agenda-fontify-whole-header-line
      (add-text-properties start (point) '(face org-agenda-structure)))))

(defsubst osa/get-priority-cookie (s)
  "Return priority character for string S.
Matches `org-priority-regexp'."
  (when (string-match org-priority-regexp s)
    (match-string-no-properties 2 item)))

;;;; Footer

(provide 'org-super-agenda)

;;; org-super-agenda.el ends here
