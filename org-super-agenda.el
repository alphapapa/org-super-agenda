;;; org-super-agenda.el --- Supercharge your agenda  -*- lexical-binding: t; -*-

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-super-agenda
;; Version: 0.1-pre
;; Package-Requires: ((emacs "25.1") (s "1.10.0") (dash "2.13") (org "9.0"))
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
(require 'org-agenda)
(require 'cl-lib)
(require 'dash)
(require 's)

;;;; Variables

(eval-when-compile
  ;; FIXME: Is this really necessary, or is this the best way to do
  ;; this?

  (defvar org-super-agenda-group-types nil
    "List of agenda grouping keywords and associated functions.
Populated automatically by `org-super-agenda--defgroup'.")

  (defvar org-super-agenda-group-transformers nil
    "List of agenda group transformers."))

(defvar org-super-agenda-function-overrides
  '((org-agenda-finalize-entries . org-super-agenda--finalize-entries))
  "List of alists mapping agenda functions to overriding
  functions.")

(defgroup org-super-agenda nil
  "Settings for `org-super-agenda'."
  :group 'org
  :link '(url-link "http://github.com/alphapapa/org-super-agenda"))

(defcustom org-super-agenda-groups nil
  "List of groups to apply to agenda views when `org-super-agenda-mode' is on.
See readme for information."
  :type 'list)

(defcustom org-super-agenda-unmatched-order 99
  "Default order setting for agenda section containing items unmatched by any filter."
  :type 'integer)

(defcustom org-super-agenda-fontify-whole-header-line nil
  "Fontify the whole line for section headers.
This is mostly useful if section headers have a highlight color,
making it stretch across the screen."
  :type 'boolean)

;;;; Support functions

(defsubst org-super-agenda--get-marker (s)
  "Return `org-marker' text properties of string S."
  (org-find-text-property-in-string 'org-marker s))

(defsubst org-super-agenda--get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defun org-super-agenda--make-agenda-header (s)
  "Return agenda header containing string S and a newline."
  (setq s (concat " " s))
  (org-add-props s nil 'face 'org-agenda-structure)
  (concat "\n" s))

(defsubst org-super-agenda--get-priority-cookie (s)
  "Return priority character for string S.
Matches `org-priority-regexp'."
  (when (string-match org-priority-regexp s)
    (match-string-no-properties 2 s)))

;;;; Minor mode

;;;###autoload
(define-minor-mode org-super-agenda-mode
  "Global minor mode to override standard Org agenda commands with modified versions that group according to `org-super-agenda-groups'.With prefix argument ARG, turn on if positive, otherwise off."
  :global t
  (let ((advice-function (if org-super-agenda-mode
                             (lambda (to fun)
                               ;; Enable mode
                               (advice-add to :override fun))
                           (lambda (from fun)
                             ;; Disable mode
                             (advice-remove from fun)))))
    (cl-loop for (target . override) in org-super-agenda-function-overrides
             do (funcall advice-function target override))
    ;; Display message
    (if org-super-agenda-mode
        (message "org-super-agenda-mode enabled.")
      (message "org-super-agenda-mode disabled."))))

;;;; Group selectors

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
  :test (when-let ((case-fold-search t)
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

(with-eval-after-load 'org-habit
  (org-super-agenda--defgroup habit
    "Group habit items.
Habit items have a \"STYLE: habit\" Org property."
    :section-name "Habits"
    :test (org-is-habit-p (org-super-agenda--get-marker item))))


;;;; Grouping functions

(defun org-super-agenda--group-items (all-items)
  "Divide ALL-ITEMS into groups based on `org-super-agenda-groups'."
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
                 finally return (cl-loop for (_ name _ items) in sections
                                         when items
                                         collect (org-super-agenda--make-agenda-header name)
                                         and append items)))
    ;; No super-filters; return list unmodified
    all-items))

;;;;; Dispatchers

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
           finally do (setq final-matches (cl-reduce 'seq-intersection all-matches))
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

;; TODO: Add example for :discard
(defun org-super-agenda--group-dispatch-discard (items group)
  "Discard items that match GROUP.
Any groups processed after this will not see these items."
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
           finally do (setq final-matches (cl-reduce 'seq-intersection all-matches))
           finally do (setq final-non-matches (seq-difference all-items final-matches))
           finally return (list (s-join " AND " (-non-nil names))
                                final-non-matches
                                nil)))
;; FIXME: This must be done but is this the way to do it?  Do I need eval-when-compile?
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
;; FIXME: Is there a better way to do this?  Maybe if I ever have any more transformers...
(setq org-super-agenda-group-transformers (plist-put org-super-agenda-group-transformers
                                                     :order-multi 'org-super-agenda--transform-group-order))

;;;; Finalize function

(defun org-super-agenda--finalize-entries (list &optional type)
  "Sort, limit and concatenate the LIST of agenda items.
The optional argument TYPE tells the agenda type."
  ;; This function is a copy of `org-agenda-finalize-entries', with
  ;; the only change being that it groups items with
  ;; `org-super-agenda--group-items' before it finally returns them.
  (let ((max-effort (cond ((listp org-agenda-max-effort)
			   (cdr (assoc type org-agenda-max-effort)))
			  (t org-agenda-max-effort)))
	(max-todo (cond ((listp org-agenda-max-todos)
			 (cdr (assoc type org-agenda-max-todos)))
			(t org-agenda-max-todos)))
	(max-tags (cond ((listp org-agenda-max-tags)
			 (cdr (assoc type org-agenda-max-tags)))
			(t org-agenda-max-tags)))
	(max-entries (cond ((listp org-agenda-max-entries)
			    (cdr (assoc type org-agenda-max-entries)))
			   (t org-agenda-max-entries))))
    (when org-agenda-before-sorting-filter-function
      (setq list
	    (delq nil
		  (mapcar
		   org-agenda-before-sorting-filter-function list))))
    (setq list (mapcar 'org-agenda-highlight-todo list)
	  list (mapcar 'identity (sort list 'org-entries-lessp)))
    (when max-effort
      (setq list (org-agenda-limit-entries
		  list 'effort-minutes max-effort
		  (lambda (e) (or e (if org-sort-agenda-noeffort-is-high
					32767 -1))))))
    (when max-todo
      (setq list (org-agenda-limit-entries list 'todo-state max-todo)))
    (when max-tags
      (setq list (org-agenda-limit-entries list 'tags max-tags)))
    (when max-entries
      (setq list (org-agenda-limit-entries list 'org-hd-marker max-entries)))

    ;; Filter with super-groups
    (setq list (org-super-agenda--group-items list))

    (mapconcat 'identity list "\n")))

;;;; Footer

(provide 'org-super-agenda)

;;; org-super-agenda.el ends here
