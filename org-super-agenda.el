;;; org-super-agenda.el --- Supercharge your agenda by grouping it into sections  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-super-agenda
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13"))
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
;;                      :tag "bills"
;;                      :priority "A")
;;               ;; Set order of multiple groups at once
;;               (:order-multi (2 (:name "Shopping in town"
;;                                       ;; Boolean AND group matches items that match all subgroups
;;                                       :and (:tag "shopping" :tag "@town"))
;;                                (:name "Food-related"
;;                                       ;; Multiple args given in list with implicit OR
;;                                       :tag ("food" "dinner"))
;;                                (:name "Personal"
;;                                       :habit t
;;                                       :tag "personal")
;;                                (:name "Space-related (non-moon-or-planet-related)"
;;                                       ;; Regexps match case-insensitively on the entire entry
;;                                       :and (:regexp ("space" "NASA")
;;                                                     ;; Boolean NOT also has implicit OR between selectors
;;                                                     :not (:regexp "moon" :tag "planet")))))
;;               ;; Groups supply their own section names when none are given
;;               (:todo "WAITING" :order 8)  ; Set order of this section
;;               (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                      ;; Show this group at the end of the agenda (since it has the
;;                      ;; highest number). If you specified this group last, items
;;                      ;; with these todo keywords that e.g. have priority A would be
;;                      ;; displayed in that group instead, because items are grouped
;;                      ;; out in the order the groups are listed.
;;                      :order 9)
;;               (:priority ("B" "C")
;;                          ;; Show this section after "Today" and "Important", because
;;                          ;; their order is unspecified, defaulting to 0.  Sections
;;                          ;; are displayed lowest-number-first.
;;                          :order 1)
;;               ;; After the last group, the agenda will display items that didn't
;;               ;; match any of these groups, with the default order position of 99
;;               )))))))
;;   (org-agenda nil "u"))

;; You can adjust the `org-super-agenda-groups' to create as many different
;; groups as you like.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'org)
(require 'cl-lib)
(require 'dash)
(require 's)

;;;; Variables

(defvar org-super-agenda-group-types nil
  "List of agenda grouping keywords and associated functions.
Populated automatically by `org-super-agenda--defgroup'.")

(defvar org-super-agenda-group-transformers nil
  "List of agenda group transformers.")

(defgroup org-super-agenda nil
  "Settings for `org-super-agenda'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-super-agenda"))

(defcustom org-super-agenda-unmatched-order 99
  "Default order setting for agenda section containing items unmatched by any filter.")

(defcustom org-super-agenda-fontify-whole-header-line nil
  "Fontify the whole line for section headers.
This is mostly useful if section headers have a highlight color,
making it stretch across the screen.")

;;;; Filters

(cl-defmacro org-super-agenda--defgroup (name docstring &key section-name test)
  "Define an agenda-item group function.
NAME is a symbol that will be appended to `org-super-agenda--group-' to
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
        (function-name (intern (concat "org-super-agenda--group-" (symbol-name name)))))
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

(org-super-agenda--defgroup time
  "Group items that have a time associated.
Items with an associated timestamp that has a time (rather than
just a date) are selected."
  :section-name "Schedule"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :test (when-let ((time (org-find-text-property-in-string 'dotime item)))
          (not (eql time 'time))))

(org-super-agenda--defgroup tag
  "Group items that match any of the given tags.
Argument may be a string or list of strings."
  :section-name (concat "Items tagged with: " (s-join " OR " args))
  :test (seq-intersection (org-super-agenda--get-tags item) args 'equalp))

(org-super-agenda--defgroup habit
  "Group habit items.
Habit items have a \"STYLE: habit\" Org property."
  :section-name "Habits"
  :test (org-is-habit-p (org-super-agenda--get-marker item)))

(org-super-agenda--defgroup todo
  "Group items that match any of the given TODO keywords.
Argument may be a string or list of strings."
  :section-name (concat (s-join " and " args) " items")
  :test (cl-member (org-find-text-property-in-string 'todo-state item) args :test 'string=))

(org-super-agenda--defgroup priority
  "Group items that match any of the given priorities.
Argument may be a string or list of strings, which should be,
e.g. \"A\" or (\"B\" \"C\")."
  :section-name (concat "Priority " (s-join " and " args) " items")
  :test (cl-member (org-super-agenda--get-priority-cookie item) args :test 'string=))

(org-super-agenda--defgroup regexp
  "Group items that match a regular expression.
Argument may be a string or list of strings, each of which should
be a regular expression.  You'll probably want to override the
section name for this group."
  :section-name (concat "Items matching regexps: " (s-join " and " args))
  :test (let* ((case-fold-search t)
               (marker (org-super-agenda--get-marker item))
               (entry (with-current-buffer (marker-buffer marker)
                        (goto-char marker)
                        (buffer-substring (org-entry-beginning-position) (org-entry-end-position)))))
          (cl-loop for regexp in args
                   thereis (string-match-p regexp entry))))

(org-super-agenda--defgroup deadline
  "Group items that have deadlines."
  :section-name "Deadline items"
  :test (when-let ((m (org-super-agenda--get-marker item)))
          (with-current-buffer (marker-buffer m)
            (org-get-deadline-time m))))

(org-super-agenda--defgroup scheduled
  "Group items that are scheduled."
  :section-name "Scheduled items"
  :test (when-let ((m (org-super-agenda--get-marker item)))
          (with-current-buffer (marker-buffer m)
            (org-get-scheduled-time m))))

;;;; Group transformers

(defun org-super-agenda--group-transform-order (groups)
  "Return GROUPS with their order set.
GROUPS is a list of groups, but the first element of the list is
actually the ORDER for the groups."
  (cl-loop with order = (pop groups)
           for group in groups
           collect (plist-put group :order order)))
;; FIXME: Is there a better way to do this?  Maybe if I ever have any more transformers...
(setq org-super-agenda-group-transformers (plist-put org-super-agenda-group-transformers
                                                     :order-multi 'org-super-agenda--group-transform-order))

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
                (org-super-agenda--insert-sections rtnall))
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

(defun org-super-agenda--insert-sections (all-items)
  "Divide ALL-ITEMS into sections and insert them into the agenda."
  ;; This essentially replaces the part of `org-agenda-list' that
  ;; finally inserts the `rtnall' variable.
  (if (bound-and-true-p org-super-agenda-groups)
      ;; Transform groups
      (let ((org-super-agenda-groups (org-super-agenda--transform-groups org-super-agenda-groups)))
        ;; Collect and insert groups
        (cl-loop with filter-fn with args with last

                 for filter in org-super-agenda-groups
                 for custom-section-name = (plist-get filter :name)
                 for order = (or (plist-get filter :order) 0)  ; Lowest number first, 0 by default
                 for (auto-section-name non-matching matching) = (org-super-agenda--group-dispatch all-items filter)
                 for section-name = (or custom-section-name auto-section-name)

                 collect (list :name section-name :items matching :order order) into sections
                 and do (setq all-items non-matching)

                 ;; Sort sections by :order then :name
                 finally do (setq non-matching (list :name "Other items" :items non-matching :order org-super-agenda-unmatched-order))
                 finally do (setq sections (--sort (let ((o-it (plist-get it :order))
                                                         (o-other (plist-get other :order)))
                                                     (cond ((and (= o-it o-other)
                                                                 (/= o-it 0))
                                                            ;; Sort by string only for items with a set order
                                                            (string< (plist-get it :name)
                                                                     (plist-get other :name)))
                                                           (t (< o-it o-other))))
                                                   (push non-matching sections)))
                 ;; Insert sections
                 finally do (progn
                              ;; cl-loop doesn't technically support plist destructuring, but
                              ;; as long as they are given in the same order, it should work
                              (cl-loop for (:name name :items items) in sections
                                       when items
                                       do (progn
                                            (org-super-agenda--insert-agenda-header name)
                                            (insert (org-agenda-finalize-entries items 'agenda)
                                                    "\n\n"))))))
    ;; No super-filters; insert normally
    (insert (org-agenda-finalize-entries all-items 'agenda)
            "\n")))

(defun org-super-agenda--group-dispatch (items group)
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
           ;; This is the implicit OR
           append matching into all-matches
           and collect auto-section-name into names
           and do (setq items non-matching)
           finally return (list (s-join " and " (-non-nil names)) items all-matches)))

;; TODO: This works, but it seems inelegant to basically copy the
;; group-dispatch function.  A more pure-functional approach might be
;; more DRY, but that would preclude using the loop macro, and might
;; be slower.  Decisions, decisions...

(defun org-super-agenda--group-dispatch-and (items group)
  "Group ITEMS that match all selectors in GROUP."
  ;; Used for the `:and' selector.
  (cl-loop with name with fn with auto-section-name with non-matching with matching
           with final-non-matches with final-matches
           with all-items = items  ; Save for later
           for (group-type args) on group by 'cddr  ; plist access
           for fn = (plist-get org-super-agenda-group-types group-type)
           ;; This double "when fn" is an ugly hack, but it lets us
           ;; use the destructuring-bind; otherwise we'd have to put
           ;; all the collection logic in a progn, or do the
           ;; destructuring ourselves, which would be uglier.
           when fn
           for (auto-section-name non-matching matching) = (funcall fn items args)
           when fn
           collect matching into all-matches
           and collect auto-section-name into names

           ;; Now for the AND
           finally do (setq final-matches (reduce 'seq-intersection all-matches))
           finally do (setq final-non-matches (seq-difference all-items final-matches))
           finally return (list (s-join " AND " (-non-nil names))
                                final-non-matches
                                final-matches)))
;; FIXME: This must be done but is this the way to do it?  Do I need eval-when-compile?
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types :and 'org-super-agenda--group-dispatch-and))

(defun org-super-agenda--group-dispatch-not (items group)
  "Group ITEMS that match no selectors in GROUP."
  ;; Used for the `:not' selector.
  ;; I think all I need to do is re-dispatch and reverse the results
  (-let (((name non-matching matching) (org-super-agenda--group-dispatch items group)))
    (list name matching non-matching)))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types :not 'org-super-agenda--group-dispatch-not))

(defun org-super-agenda--transform-groups (groups)
  (cl-loop for group in groups
           for fn = (plist-get org-super-agenda-group-transformers (car group))
           if fn
           do (setq group (funcall fn (cadr group)))
           and append group
           else collect group))

(defsubst org-super-agenda--get-marker (s)
  (org-find-text-property-in-string 'org-marker s))

(defsubst org-super-agenda--get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defsubst org-super-agenda--insert-agenda-header (s)
  "Insert agenda header into current buffer containing string S and a newline."
  (let ((start (point))
        (s (concat " " s)))
    (insert (org-add-props s nil 'face 'org-agenda-structure)
            "\n")
    (when org-super-agenda-fontify-whole-header-line
      (add-text-properties start (point) '(face org-agenda-structure)))))

(defsubst org-super-agenda--get-priority-cookie (s)
  "Return priority character for string S.
Matches `org-priority-regexp'."
  (when (string-match org-priority-regexp s)
    (match-string-no-properties 2 s)))

;;;; Footer

(provide 'org-super-agenda)

;;; org-super-agenda.el ends here
