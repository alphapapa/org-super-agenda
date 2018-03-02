;;; org-super-agenda.el --- Supercharge your agenda  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-super-agenda
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13") (org "9.0") (ht "2.2"))
;; Keywords: hypermedia, outlines, Org, agenda

;;; Commentary:

;; This package lets you "supercharge" your Org daily/weekly agenda.
;; The idea is to group items into sections, rather than having them
;; all in one big list.

;; Now you can sort-of do this already with custom agenda commands,
;; but when you do that, you lose the daily/weekly aspect of the
;; agenda: items are no longer shown based on deadline/scheduled
;; timestamps, but are shown no-matter-what.

;; So this package filters the results from
;; `org-agenda-finalize-entries', which runs just before items are
;; inserted into agenda views.  It runs them through a set of filters
;; that separate them into groups.  Then the groups are inserted into
;; the agenda buffer, and any remaining items are inserted at the end.
;; Empty groups are not displayed.

;; The end result is your standard daily/weekly agenda, but arranged
;; into groups defined by you.  You might put items with certain tags
;; in one group, habits in another group, items with certain todo
;; keywords in another, and items with certain priorities in another.
;; The possibilities are only limited by the grouping functions.

;; The primary use of this package is for the daily/weekly agenda,
;; made by the `org-agenda-list' command, but it also works for other
;; agenda views, like `org-tags-view', `org-todo-list',
;; `org-search-view', etc.

;; Here's an example which you can test by evaluating the `let' form:

;; (let ((org-super-agenda-groups
;;        '(;; Each group has an implicit boolean OR operator between its selectors.
;;          (:name "Today" ; Optionally specify section name
;;                 :time-grid t ; Items that appear on the time grid
;;                 :todo "TODAY") ; Items that have this TODO keyword
;;          (:name "Important"
;;                 ;; Single arguments given alone
;;                 :tag "bills"
;;                 :priority "A")
;;          ;; Set order of multiple groups at once
;;          (:order-multi (2 (:name "Shopping in town"
;;                                  ;; Boolean AND group matches items that match all subgroups
;;                                  :and (:tag "shopping" :tag "@town"))
;;                           (:name "Food-related"
;;                                  ;; Multiple args given in list with implicit OR
;;                                  :tag ("food" "dinner"))
;;                           (:name "Personal"
;;                                  :habit t
;;                                  :tag "personal")
;;                           (:name "Space-related (non-moon-or-planet-related)"
;;                                  ;; Regexps match case-insensitively on the entire entry
;;                                  :and (:regexp ("space" "NASA")
;;                                                ;; Boolean NOT also has implicit OR between selectors
;;                                                :not (:regexp "moon" :tag "planet")))))
;;          ;; Groups supply their own section names when none are given
;;          (:todo "WAITING" :order 8) ; Set order of this section
;;          (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                 ;; Show this group at the end of the agenda (since it has the
;;                 ;; highest number). If you specified this group last, items
;;                 ;; with these todo keywords that e.g. have priority A would be
;;                 ;; displayed in that group instead, because items are grouped
;;                 ;; out in the order the groups are listed.
;;                 :order 9)
;;          (:priority<= "B"
;;                       ;; Show this section after "Today" and "Important", because
;;                       ;; their order is unspecified, defaulting to 0. Sections
;;                       ;; are displayed lowest-number-first.
;;                       :order 1)
;;          ;; After the last group, the agenda will display items that didn't
;;          ;; match any of these groups, with the default order position of 99
;;          )))
;;   (org-agenda nil "a"))

;; You can adjust the `org-super-agenda-groups' to create as many different
;; groups as you like.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'org)
(require 'org-agenda)
(require 'cl-lib)
(require 'dash)
(require 's)
(require 'ht)

;; I think this is the right way to do this...
(eval-when-compile
  (require 'org-macs))

;;;; Variables

(defconst org-super-agenda-special-selectors
  '(:name :order)
  ;; This needs to be manually updated if any are added.
  "Special, non-grouping selectors.")

(defvar org-super-agenda-group-types nil
  "List of agenda grouping keywords and associated functions.
Populated automatically by `org-super-agenda--defgroup'.")

(defvar org-super-agenda-group-transformers nil
  "List of agenda group transformers.")

(defgroup org-super-agenda nil
  "Settings for `org-super-agenda'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-super-agenda"))

(defcustom org-super-agenda-groups nil
  "List of groups to apply to agenda views when `org-super-agenda-mode' is on.
See readme for information."
  :type 'list)

(defcustom org-super-agenda-properties-inherit t
  "Use property inheritance when checking properties with the :auto-group selector.
With this enabled, you can set the \"agenda-group\" property for
an entire subtree, and every entry below it will inherit the
agenda group.  It seems most natural for it to be enabled, so the
default is.  But in case of performance problems, it can be
disabled.  This sets the INHERIT argument to `org-entry-get'."
  :type 'boolean)

(defcustom org-super-agenda-unmatched-order 99
  "Default order setting for agenda section containing items unmatched by any filter."
  :type 'integer)

(defcustom org-super-agenda-fontify-whole-header-line nil
  "Fontify the whole line for section headers.
This is mostly useful if section headers have a highlight color,
making it stretch across the screen."
  :type 'boolean)

;;;; Macros

(defmacro org-super-agenda--when-with-marker-buffer (form &rest body)
  "When FORM is a marker, run BODY in the marker's buffer, with point starting at it."
  (declare (indent defun) (debug (form body)))
  (org-with-gensyms (marker)
    `(let ((,marker ,form))
       (when (markerp ,marker)
         (with-current-buffer (marker-buffer ,marker)
           (save-excursion
             (goto-char ,marker)
             ,@body))))))

(cl-defmacro org-super-agenda--map-children (&key form any)
  "Return FORM mapped across child entries of entry at point, if it has any.
If ANY is non-nil, return as soon as FORM returns non-nil."
  (declare (indent defun))
  (org-with-gensyms (tree-start tree-end result all-results)
    `(let ((,tree-start (point))
           ,tree-end ,all-results)
       (when (org-goto-first-child)
         (goto-char ,tree-start)
         ,(when any
            `(save-excursion
               (setq ,tree-end (org-end-of-subtree))))
         (setq ,all-results (org-map-entries (lambda ()
                                               (let ((,result ,form))
                                                 ,(when any
                                                    `(when ,result
                                                       (setq org-map-continue-from ,tree-end)))
                                                 ,result))
                                             nil 'tree))
         (if ,any
             (--any (not (null it)) ,all-results)
           ,all-results)))))

;;;; Support functions

(defsubst org-super-agenda--get-marker (s)
  "Return `org-marker' text properties of string S."
  (org-find-text-property-in-string 'org-marker s))

(defsubst org-super-agenda--get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defun org-super-agenda--make-agenda-header (s)
  "Return agenda header containing string S and a newline."
  (pcase s
    ('none "")
    (_ (setq s (concat " " s))
       (org-add-props s nil 'face 'org-agenda-structure)
       (concat "\n" s))))

(defsubst org-super-agenda--get-priority-cookie (s)
  "Return priority character for string S.
Matches `org-priority-regexp'."
  (when (string-match org-priority-regexp s)
    (match-string-no-properties 2 s)))

(defun org-super-agenda--get-item-entry (item)
  "Get entry for ITEM.
ITEM should be a string with the `org-marker' property set to a
marker."
  (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
    (buffer-substring (org-entry-beginning-position)
                      (org-entry-end-position))))

;;;; Minor mode

;;;###autoload
(define-minor-mode org-super-agenda-mode
  "Global minor mode to group items in Org agenda views according to `org-super-agenda-groups'.
With prefix argument ARG, turn on if positive, otherwise off."
  :global t
  (let ((advice-function (if org-super-agenda-mode
                             (lambda (to fn)
                               ;; Enable mode
                               (advice-add to :filter-return fn))
                           (lambda (from fn)
                             ;; Disable mode
                             (advice-remove from fn)))))
    (funcall advice-function #'org-agenda-finalize-entries
             #'org-super-agenda--filter-finalize-entries)
    ;; Display message
    (message (if org-super-agenda-mode
                 "org-super-agenda-mode enabled."
               "org-super-agenda-mode disabled."))))

;;;; Group selectors

(cl-defmacro org-super-agenda--defgroup (name docstring &key section-name test let*)
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

:LET* is a `let*' binding form that is bound around the function
body after the ARGS are made a list.

Finally a list of three items is returned, with the value
returned by :SECTION-NAME as the first item, a list of items not
matching the :TEST as the second, and a list of items matching as
the third."
  (declare (indent defun)
           (debug (symbolp stringp body)))
  (let ((group-type (intern (concat ":" (symbol-name name))))
        (function-name (intern (concat "org-super-agenda--group-" (symbol-name name)))))
    ;; Associate the group type with this function so the dispatcher can find it
    `(progn
       (setq org-super-agenda-group-types (plist-put org-super-agenda-group-types ,group-type ',function-name))
       (defun ,function-name (items args)
         ,docstring
         (unless (listp args)
           (setq args (list args)))
         (let* ,let*
           (cl-loop with section-name = ,section-name
                    for item in items
                    if ,test
                    collect item into matching
                    else collect item into non-matching
                    finally return (list section-name non-matching matching)))))))

;;;;; Date/time-related

;; TODO: I guess these should be in a date-matcher macro

(org-super-agenda--defgroup date
  "Group items that have a date associated.
Argument can be `t' to match items with any date, `nil' to match
items without a date, or `today' to match items with today's
date.  The `ts-date' text-property is matched against. "
  :section-name "Dated items"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :let* ((today (org-today)))
  :test (pcase (car args)
          ('t ;; Test for any date
           (org-find-text-property-in-string 'ts-date item))
          ('nil ;; Test for not having a date
           (not (org-find-text-property-in-string 'ts-date item)))
          ('today  ;; Items that have a time sometime today
           ;; TODO: Maybe I can use the ts-date property in some other places, might be faster
           (when-let ((day (org-find-text-property-in-string 'ts-date item)))
             (= day today)))
          (_ ;; Oops
           (user-error "Argument to `:date' must be `t', `nil', or `today'"))))

(org-super-agenda--defgroup time-grid
  "Group items that appear on a time grid.
This matches the `dotime' text-property, which, if NOT set to
`time' (I know, this gets confusing), means it WILL appear in the
agenda time-grid. "
  :section-name "Timed items"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :test (or (--when-let (org-find-text-property-in-string 'time item)
              ;; This property is a string; if empty, it doesn't match
              (not (string-empty-p it)))
            ;; This property is nil if it doesn't match
            (org-find-text-property-in-string 'time-of-day item)
            (--when-let (org-find-text-property-in-string 'dotime item)
              ;; For this to match, the 'dotime property must be set, and
              ;; it must not be equal to 'time.  If it is not set, or if
              ;; it is set and is equal to 'time, the item is not part of
              ;; the time-grid.  Yes, this is confusing.  :)
              (not (eql it 'time)))))

(org-super-agenda--defgroup deadline
  "Group items that have a deadline.
Argument can be `t' (to match items with any deadline), `nil' (to
match items that have no deadline), `past` (to match items with a
deadline in the past), `today' (to match items whose deadline is
today), or `future' (to match items with a deadline in the
future).  Argument may also be given like `before DATE' or `after
DATE', where DATE is a date string that
`org-time-string-to-absolute' can process."
  :section-name (pcase (car args)
                  ('t "Deadline items")
                  ('nil "Items without deadlines")
                  ('past "Past due")
                  ('today "Due today")
                  ('future "Due soon")
                  ('before (concat "Due before " (second args)))
                  ('on (concat "Due on " (second args)))
                  ('after (concat "Due after " (second args))))
  :let* ((today (pcase (car args)  ; Perhaps premature optimization
                  ((or 'past 'today 'future 'before 'on 'after)
                   (org-today))))
         (target-date (pcase (car args)
                        ((or 'before 'on 'after)
                         (org-time-string-to-absolute (second args))))))
  :test (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
          (let ((entry-time (org-entry-get (point) "DEADLINE")))
            (pcase (car args)
              ('t entry-time)  ; Has any deadline info
              ('nil (not entry-time))  ; Has no deadline info
              (comparison
               (when entry-time
                 (let ((entry-time (org-time-string-to-absolute entry-time))
                       (compare-date (pcase comparison
                                       ((or 'past 'today 'future) today)
                                       ((or 'before 'on 'after) target-date))))
                   (org-super-agenda--compare-dates comparison entry-time compare-date))))))))

(org-super-agenda--defgroup scheduled
  "Group items that are scheduled.
Argument can be `t' (to match items scheduled for any date),
`nil' (to match items that are not schedule), `past` (to match
items scheduled for the past), `today' (to match items scheduled
for today), or `future' (to match items scheduled for the
future).  Argument may also be given like `before DATE' or `after
DATE', where DATE is a date string that
`org-time-string-to-absolute' can process."
  :section-name (pcase (car args)
                  ('t "Scheduled items")
                  ('nil "Unscheduled items ")
                  ('past "Past scheduled")
                  ('today "Scheduled today")
                  ('future "Scheduled soon")
                  ('before (concat "Scheduled before " (second args)))
                  ('on (concat "Scheduled on " (second args)))
                  ('after (concat "Scheduled after " (second args))))
  :let* ((today (pcase (car args)  ; Perhaps premature optimization
                  ((or 'past 'today 'future 'before 'on 'after)
                   (org-today))))
         (target-date (pcase (car args)
                        ((or 'before 'on 'after)
                         (org-time-string-to-absolute (second args))))))
  :test (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
          (let ((entry-time (org-entry-get (point) "SCHEDULED")))
            (pcase (car args)
              ('t entry-time)  ; Has any scheduled info
              ('nil (not entry-time))  ; Has no scheduled info
              (comparison
               (when entry-time
                 (let ((entry-time (org-time-string-to-absolute entry-time))
                       (compare-date (pcase comparison
                                       ((or 'past 'today 'future) today)
                                       ((or 'before 'on 'after) target-date))))
                   (org-super-agenda--compare-dates comparison entry-time compare-date))))))))

(defun org-super-agenda--compare-dates (comparison date-a date-b)
  "Compare DATE-A and DATE-B according to COMPARISON.
COMPARISON should be a symbol, one of: `past' or `before',
`today' or `on', `future' or `after'."
  (pcase comparison
    ((or 'past 'before) (< date-a date-b))
    ((or 'today 'on) (= date-a date-b))
    ((or 'future 'after) (> date-a date-b))))

;;;;; Effort

(cl-defmacro org-super-agenda--defeffort-group (name docstring &key section-name comparator)
  (declare (indent defun))
  `(org-super-agenda--defgroup ,(intern (concat "effort" (symbol-name name)))
     ,(concat docstring "\nArgument is a time-duration string, like \"5\" or \"0:05\" for 5 minutes.")
     :section-name (concat "Effort " ,(symbol-name name) " "
                           (s-join " or " args) " items")
     :let* ((effort-minutes (org-duration-string-to-minutes (car args))))
     :test (when-let ((item-effort (org-find-text-property-in-string 'effort item)))
             (,comparator (org-duration-string-to-minutes item-effort) effort-minutes))))

(org-super-agenda--defeffort-group <
  "Group items that are less than (or equal to) the given effort."
  :comparator <=)

(org-super-agenda--defeffort-group >
  "Group items that are higher than (or equal to) the given effort."
  :comparator >=)

;;;;; Misc

(org-super-agenda--defgroup anything
  "Select any item, no matter what.
This is a catch-all, probably most useful with the `:discard'
selector."
  :test t)

;; TODO: Rename this to something like :family-tree and make a new
;; one-level-deep-only :children matcher that will be much faster
(org-super-agenda--defgroup children
  "Select any item that has child entries.
Argument may be `t' to match if it has any children, `nil' to
match if it has no children, `todo' to match if it has children
with any to-do keywords, or a string to match if it has specific
to-do keywords."
  :section-name "Items with children"
  :let* ((case-fold-search t))
  :test (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
          (pcase (car args)
            ('todo ;; Match if entry has child to-dos
             (org-super-agenda--map-children
               :form (org-entry-is-todo-p)
               :any t))
            ((pred stringp)  ;; Match child to-do keywords
             (org-super-agenda--map-children
               :form (cl-member (org-get-todo-state) args :test #'string=)
               :any t))
            ('t  ;; Match if it has any children
             (org-goto-first-child))
            ('nil  ;; Match if it has no children
             (not (org-goto-first-child))))))

(with-eval-after-load 'org-habit
  (org-super-agenda--defgroup habit
    "Group habit items.
Habit items have a \"STYLE: habit\" Org property."
    :section-name "Habits"
    :test (org-is-habit-p (org-super-agenda--get-marker item))))

(org-super-agenda--defgroup log
  "Group items from log mode.
Note that these items may also be matched by the :time-grid
selector, so if you want these displayed in their own group, you
may need to select them in a group before a group containing the
:time-grid selector."
  :section-name "Log"
  ;; I don't know why the property's value is a string instead of a
  ;; symbol, because `org-agenda-log-mode-items' is a list of symbols.
  :test (cl-member (org-find-text-property-in-string 'type item)
                   '("closed" "clock" "state")
                   :test #'string=))

(org-super-agenda--defgroup heading-regexp
  "Group items whose headings match any of the given regular expressions.
Argument may be a string or list of strings, each of which should
be a regular expression.  You'll probably want to override the
section name for this group."
  :section-name (concat "Headings matching regexps: "
                        (s-join " OR "
                                (--map (s-wrap it "\"")
                                       args)))
  :let* ((case-fold-search t))
  :test (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
          (let ((heading (org-get-heading 'no-tags 'no-todo)))
            (cl-loop for regexp in args
                     thereis (string-match-p regexp heading)))))

(org-super-agenda--defgroup regexp
  "Group items that match any of the given regular expressions.
Argument may be a string or list of strings, each of which should
be a regular expression.  You'll probably want to override the
section name for this group."
  :section-name (concat "Items matching regexps: "
                        (s-join " OR "
                                (--map (s-wrap it "\"")
                                       args)))
  :let* ((case-fold-search t))
  :test (when-let ((entry (org-super-agenda--get-item-entry item)))
          (cl-loop for regexp in args
                   thereis (string-match-p regexp entry))))

(org-super-agenda--defgroup tag
  "Group items that match any of the given tags.
Argument may be a string or list of strings."
  :section-name (concat "Items tagged with: " (s-join " OR " args))
  :test (seq-intersection (org-super-agenda--get-tags item) args 'cl-equalp))

(org-super-agenda--defgroup todo
  "Group items that match any of the given TODO keywords.
Argument may be a string or list of strings, or `t' to match any
keyword, or `nil' to match only non-todo items."
  :section-name (pcase (car args)
                  ((pred stringp) ;; To-do keyword given
                   (concat (s-join " and " args) " items"))
                  ('t ;; Test for any to-do keyword
                   "Any TODO keyword")
                  ('nil ;; Test for not having a to-do keyword
                   "Non-todo items")
                  (_ ;; Oops
                   (user-error "Argument to `:todo' must be a string, list of strings, t, or nil")))
  :test (pcase (car args)
          ((pred stringp) ;; To-do keyword given
           (cl-member (org-find-text-property-in-string 'todo-state item) args :test 'string=))
          ('t ;; Test for any to-do keyword
           (org-find-text-property-in-string 'todo-state item))
          ('nil ;; Test for not having a to-do keyword
           (not (org-find-text-property-in-string 'todo-state item)))
          (_ ;; Oops
           (user-error "Argument to `:todo' must be a string, list of strings, t, or nil"))))

;;;;; Priority

(org-super-agenda--defgroup priority
  "Group items that match any of the given priorities.
Argument may be a string or list of strings, which should be,
e.g. \"A\" or (\"B\" \"C\")."
  :section-name (concat "Priority " (s-join " and " args) " items")
  :test (cl-member (org-super-agenda--get-priority-cookie item) args :test 'string=))

(cl-defmacro org-super-agenda--defpriority-group (name docstring &key section-name comparator)
  (declare (indent defun))
  `(org-super-agenda--defgroup ,(intern (concat "priority" (symbol-name name)))
     ,(concat docstring "\nArgument is a string; it may also be a list of
strings, in which case only the first will be used.
The string should be the priority cookie letter, e.g. \"A\".")
     :section-name (concat "Priority " ,(symbol-name name) " "
                           (s-join " or " args) " items")
     :let* ((priority-number (string-to-char (car args))))
     :test (let ((item-priority (org-super-agenda--get-priority-cookie item)))
             (when item-priority
               ;; Higher priority means lower number
               (,comparator (string-to-char item-priority) priority-number)))))

(org-super-agenda--defpriority-group >
  "Group items that are higher than the given priority."
  :comparator <)

(org-super-agenda--defpriority-group >=
  "Group items that are greater than or equal to the given priority."
  :comparator <=)

(org-super-agenda--defpriority-group <
  "Group items that are lower than the given priority."
  :comparator >)

(org-super-agenda--defpriority-group <=
  "Group items that are lower than or equal to the given priority."
  :comparator >=)

;;;; Grouping functions

;; TODO: cl-loop is great, but when it gets this big, it's rather ugly, and it
;; probably scares some people away.  This should probably be refactored.
(defun org-super-agenda--group-items (all-items)
  "Divide ALL-ITEMS into groups based on `org-super-agenda-groups'."
  (if (bound-and-true-p org-super-agenda-groups)
      ;; Transform groups
      (let ((org-super-agenda-groups (org-super-agenda--transform-groups org-super-agenda-groups)))
        ;; Collect and insert groups
        (cl-loop with section-name
                 for filter in org-super-agenda-groups
                 for custom-section-name = (plist-get filter :name)
                 for order = (or (plist-get filter :order) 0)  ; Lowest number first, 0 by default
                 for (auto-section-name non-matching matching) = (org-super-agenda--group-dispatch all-items filter)

                 ;; Auto category/group
                 if (cl-member auto-section-name '(:auto-group :auto-category))
                 do (setq section-name (or custom-section-name "Auto category/group"))
                 and append (cl-loop for group in matching
                                     collect (list :name (plist-get group :name)
                                                   :items (plist-get group :items)
                                                   :order order))
                 into sections
                 and do (setq all-items non-matching)

                 ;; Manual groups
                 else
                 do (setq section-name (or custom-section-name auto-section-name))
                 and collect (list :name section-name :items matching :order order) into sections
                 and do (setq all-items non-matching)

                 ;; Sort sections by :order then :name
                 finally do (setq non-matching (list :name "Other items"
                                                     :items non-matching
                                                     :order org-super-agenda-unmatched-order))
                 finally do (setq sections (--sort (let ((o-it (plist-get it :order))
                                                         (o-other (plist-get other :order)))
                                                     (cond ((and
                                                             ;; FIXME: This is now quite ugly.  I'm not sure that all of these tests
                                                             ;; are necessary, but at the moment it works, so I'm leaving it alone.
                                                             (equal o-it o-other)
                                                             (not (equal o-it 0))
                                                             (stringp (plist-get it :name))
                                                             (stringp (plist-get other :name)))
                                                            ;; Sort by string only for items with a set order
                                                            (string< (plist-get it :name)
                                                                     (plist-get other :name)))
                                                           ((and (numberp o-it)
                                                                 (numberp o-other))
                                                            (< o-it o-other))
                                                           (t nil)))
                                                   (push non-matching sections)))
                 ;; Insert sections
                 finally return (cl-loop for (_ name _ items) in sections
                                         when items
                                         collect (org-super-agenda--make-agenda-header name)
                                         and append items)))
    ;; No super-filters; return list unmodified
    all-items))

;;;;; Auto-grouping

;; TODO: Refactor these, because they are essentially the same thing,
;; like the regular groups do essentially the same thing.  But this
;; already works, so I'm going to go ahead and release it.

(defun org-super-agenda--auto-group-items (all-items &rest ignore)
  "Divide ALL-ITEMS into groups based on their AGENDA-GROUP property."
  (cl-loop with groups = (ht-create)
           for item in all-items
           for group = (org-entry-get (org-super-agenda--get-marker item)
                                      "agenda-group"
                                      org-super-agenda-properties-inherit)
           if group
           do (ht-set! groups group (cons item (ht-get groups group)))
           else collect item into non-matching
           finally return (list :auto-group
                                non-matching
                                (cl-loop for key in (sort (ht-keys groups) #'string<)
                                         for name = (concat "Group: " key)
                                         collect (list :name name
                                                       :items (nreverse (ht-get groups key)))))))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types
                                              :auto-group #'org-super-agenda--auto-group-items))

(defun org-super-agenda--auto-group-category (all-items &rest ignore)
  "Divide ALL-ITEMS into groups based on their org-category property."
  (cl-loop with categories = (ht-create)
           for item in all-items
           for category = (org-super-agenda--when-with-marker-buffer (org-super-agenda--get-marker item)
                            (org-get-category))
           if category
           do (ht-set! categories category (cons item (ht-get categories category)))
           else collect item into non-matching
           finally return (list :auto-category
                                non-matching
                                (cl-loop for key in (sort (ht-keys categories) #'string<)
                                         for name = (concat "Category: " key)
                                         collect (list :name name
                                                       :items (ht-get categories key))))))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types
                                              :auto-category #'org-super-agenda--auto-group-category))

;;;;; Dispatchers

(defun org-super-agenda--get-selector-fn (selector)
  "Return function for SELECTOR, or nil if special selector.
Raise error if invalid selector."
  (cond
   ((cl-member selector org-super-agenda-special-selectors)
    ;; Special selector, so no associated function; return nil
    nil)
   ;; Valid selector: return function
   ((plist-get org-super-agenda-group-types selector))
   ;; Invalid selector: raise error
   ((user-error "Invalid org-super-agenda-groups selector: %s" selector))))

(defun org-super-agenda--group-dispatch (items group)
  "Group ITEMS with the appropriate grouping functions for GROUP.
Grouping functions are listed in `org-super-agenda-group-types', which
see."
  (cl-loop for (selector args) on group by 'cddr  ; plist access
           for fn = (org-super-agenda--get-selector-fn selector)
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
           for name = (if (stringp (car names))
                          (s-join " and " (-non-nil names))
                        ;; Probably an :auto-group
                        (car names))
           finally return (list name items all-matches)))

;; TODO: This works, but it seems inelegant to basically copy the
;; group-dispatch function.  A more pure-functional approach might be
;; more DRY, but that would preclude using the loop macro, and might
;; be slower.  Decisions, decisions...

(defun org-super-agenda--group-dispatch-and (items group)
  "Group ITEMS that match all selectors in GROUP."
  ;; Used for the `:and' selector.
  (cl-loop with final-non-matches with final-matches
           with all-items = items  ; Save for later
           for (selector args) on group by 'cddr  ; plist access
           for fn = (org-super-agenda--get-selector-fn selector)
           ;; This double "when fn" is an ugly hack, but it lets us
           ;; use the destructuring-bind; otherwise we'd have to put
           ;; all the collection logic in a progn, or do the
           ;; destructuring ourselves, which would be uglier.
           when fn
           for (auto-section-name _ matching) = (funcall fn items args)
           when fn
           collect matching into all-matches
           and collect auto-section-name into names

           ;; Now for the AND
           finally do (setq final-matches (cl-reduce 'seq-intersection all-matches))
           finally do (setq final-non-matches (seq-difference all-items final-matches))
           finally return (list (s-join " AND " (-non-nil names))
                                final-non-matches
                                final-matches)))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types
                                              :and 'org-super-agenda--group-dispatch-and))

(defun org-super-agenda--group-dispatch-not (items group)
  "Group ITEMS that match no selectors in GROUP."
  ;; Used for the `:not' selector.
  ;; I think all I need to do is re-dispatch and reverse the results
  (-let (((name non-matching matching) (org-super-agenda--group-dispatch items group)))
    (list name matching non-matching)))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types
                                              :not 'org-super-agenda--group-dispatch-not))

(defun org-super-agenda--group-dispatch-discard (items group)
  "Discard items that match GROUP.
Any groups processed after this will not see these items."
  (cl-loop for (selector args) on group by 'cddr  ; plist access
           for fn = (org-super-agenda--get-selector-fn selector)
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
           finally return (list (s-join " and " (-non-nil names))
                                items
                                nil)))
(setq org-super-agenda-group-types (plist-put org-super-agenda-group-types
                                              :discard 'org-super-agenda--group-dispatch-discard))

;;;;; Transformers

(defun org-super-agenda--transform-groups (groups)
  "Transform GROUPS according to `org-super-agenda-group-transformers'."
  (cl-loop for group in groups
           for fn = (plist-get org-super-agenda-group-transformers (car group))
           if fn
           do (setq group (funcall fn (cadr group)))
           and append group
           else collect group))

(defun org-super-agenda--transform-group-order (groups)
  "Return GROUPS with their order set.
GROUPS is a list of groups, but the first element of the list is
actually the ORDER for the groups."
  (cl-loop with order = (pop groups)
           for group in groups
           collect (plist-put group :order order)))
(setq org-super-agenda-group-transformers (plist-put org-super-agenda-group-transformers
                                                     :order-multi 'org-super-agenda--transform-group-order))

;;;; Finalize filter

(defun org-super-agenda--filter-finalize-entries (string)
  "Filter the return of `org-agenda-finalize-entries' through `org-super-agenda--finalize-entries'."
  (mapconcat 'identity
             (org-super-agenda--group-items
              (split-string string "\n" t))
             "\n"))

;;;; Footer

(provide 'org-super-agenda)

;;; org-super-agenda.el ends here
