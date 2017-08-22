;;  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requirements

(require 'org-super-agenda)
(require 'ert)
(require 'ht)
(require 'f)

(load-file "diary-sunrise-sunset.el")

;;;; Variables

(defvar org-super-agenda--test-results (ht-create))
(defvar org-super-agenda--test-save-results nil)
(defvar org-super-agenda--test-show-results nil)

;;;; Commands

(defun org-super-agenda--test-update-all-tests ()
  (interactive)
  (when (equal (read-char "Update all test results?") ?y)
    (setq org-super-agenda--test-save-results t)
    (save-excursion
      (goto-char (point-min))
      (re-search-forward "^;;;; Tests")
      (while (re-search-forward "(org-super-agenda--test-run" nil t)
        (goto-char (match-beginning 0))
        (forward-sexp)
        (eval-last-sexp nil)))
    (setq org-super-agenda--test-save-results nil)))

(defun org-super-agenda--test-save-this-test ()
  (interactive)
  (let ((org-super-agenda--test-show-results nil)
        (org-super-agenda--test-save-results t))
    (org-super-agenda--test-run-this-test)))

(defun org-super-agenda--test-show-this-test ()
  (interactive)
  (let ((org-super-agenda--test-show-results t)
        (org-super-agenda--test-save-results nil))
    (org-super-agenda--test-run-this-test)))

(defun org-super-agenda--test-run-all ()
  (interactive)
  (ert-run-tests-interactively "^org-super-agenda--test-"))

(defun org-super-agenda--test-toggle-show-results ()
  (interactive)
  (setq org-super-agenda--test-show-results (not org-super-agenda--test-show-results))
  (message "Showing result: %s" org-super-agenda--test-show-results))

(defun org-super-agenda--test-toggle-save-results ()
  (interactive)
  (setq org-super-agenda--test-save-results (not org-super-agenda--test-save-results))
  (message "Saving results: %s" org-super-agenda--test-save-results))

;;;; Functions

(defun org-super-agenda--test-run-this-test ()
  (save-excursion
    (when (or (looking-at "(org-super-agenda--test-run")
              (re-search-backward "(org-super-agenda--test-run" nil t))
      (goto-char (match-beginning 0))
      (forward-sexp)
      (eval-last-sexp nil))))

(defun org-super-agenda--test-save-result (body-groups-hash result)
  (ht-set! org-super-agenda--test-results body-groups-hash result)
  (with-temp-file "results.el"
    (insert (format "%S" org-super-agenda--test-results)))
  (message "Saved result for: %s" body-groups-hash))

(defun org-super-agenda--test-load-results ()
  (setq org-super-agenda--test-results
        (read (f-read "results.el"))))

;;;; Macros

(defmacro org-super-agenda--test-with-org-today-date (date &rest body)
  "Run BODY with the `org-today' function set to return simply DATE.
  DATE should be a date-time string (both date and time must be included)."
  (declare (indent defun))
  `(let ((day (date-to-day ,date))
         (orig (symbol-function 'org-today)))
     (unwind-protect
         (progn
           (fset 'org-today (lambda () day))
           ,@body)
       (fset 'org-today orig))))

(cl-defmacro org-super-agenda--test-run
    (&key (body '(org-agenda-list nil))
          (groups nil groups-set)
          (span 'day)
          (date "2017-07-05 12:00")
          let*)
  "Test BODY with GROUPS and LET* binding.
When `org-super-agenda--test-save-results' is non-nil, save the
result to the results file.  When
`org-super-agenda--test-show-results' is non-nil, show the agenda
buffer and do not save the results."
  (declare (debug (form &optional listp sexp sexp stringp)))
  `(progn
     (unless org-super-agenda--test-results
       (org-super-agenda--test-load-results))
     (let ((body-groups-hash (sxhash (list ',body ,groups)))
           result format-time-string-orig)
       (unwind-protect
           (progn
             ;; Rebind `format-time-string' so it always returns the
             ;; same when no time is given, otherwise the "now" line
             ;; in the time-grid depends on the real time when the
             ;; test is run.  (For some reason, cl-labels/cl-flet
             ;; don't work, so we have to use `setf' and
             ;; `symbol-function'.  Might have something to do with
             ;; lexical-binding.)
             (setf (symbol-function 'format-time-string-orig)
                   (symbol-function 'format-time-string))
             (setf (symbol-function 'format-time-string)
                   (lambda (format-string &optional time zone)
                     (if time
                         (format-time-string-orig format-string time zone)
                       (concat (second (s-split " " ,date)) " "))))
             (org-super-agenda--test-with-org-today-date ,date
               (let* ((org-agenda-files (list "test.org"))
                      ,(if let*
                           let*
                         `(ignore nil))
                      ,(if groups-set
                           `(org-super-agenda-groups ,groups)
                         `(ignore nil))
                      ,(if span
                           `(org-agenda-span ',span)
                         `(ignore nil))
                      string)
                 ,body
                 ;; We ignore the text properties.  This should be the right
                 ;; thing to do, since we never modify them.  It also makes
                 ;; the results actually legible.
                 (setq result (buffer-substring-no-properties 1 (point-max)))
                 (unless org-super-agenda--test-show-results
                   (kill-buffer))))
             (when (and org-super-agenda--test-save-results
                        (not org-super-agenda--test-show-results))
               ;; Save test result
               (org-super-agenda--test-save-result body-groups-hash result))
             (unless org-super-agenda--test-show-results
               ;; Don't give real test result when showing result buffer
               (equal result (ht-get org-super-agenda--test-results body-groups-hash))))
         ;; Reset current-time function
         (setf (symbol-function 'format-time-string) (symbol-function 'format-time-string-orig))))))

;;;; Tests

(ert-deftest org-super-agenda--test-no-groups ()
  (should (org-super-agenda--test-run
           :groups nil)))

(ert-deftest org-super-agenda--test-time-grid-with-unprioritized-order-100 ()
  (should (org-super-agenda--test-run
           :groups '((:name "Today"
                            :time-grid t)
                     (:name "Unprioritized"
                            :not (:priority>= "C")
                            :order 100)))))

(ert-deftest org-super-agenda--test-priority>=B ()
  (should (org-super-agenda--test-run
           :groups '((:priority>= "B")))))

(ert-deftest org-super-agenda--test-priority<B-with-order-100 ()
  (should (org-super-agenda--test-run
           :groups '((:priority< "B" :order 100)))))

(ert-deftest org-super-agenda--test-heading-regexp ()
  (should (org-super-agenda--test-run
           :groups '((:heading-regexp "moon")))))

(ert-deftest org-super-agenda--test-main-example ()
  (should (org-super-agenda--test-run
           :groups '(;; Each group has an implicit boolean OR operator between its selectors.
                     (:name "Today" ; Optionally specify section name
                            :time-grid t ; Items that have a time associated
                            :todo "TODAY") ; Items that have this TODO keyword
                     (:name "Important"
                            ;; Single arguments given alone
                            :tag "bills"
                            :priority "A")
                     ;; Set order of multiple groups at once
                     (:order-multi (2 (:name "Shopping in town"
                                             ;; Boolean AND group matches items that match all subgroups
                                             :and (:tag "shopping" :tag "@town"))
                                      (:name "Food-related"
                                             ;; Multiple args given in list with implicit OR
                                             :tag ("food" "dinner"))
                                      (:name "Personal"
                                             :habit t
                                             :tag "personal")
                                      (:name "Space-related (non-moon-or-planet-related)"
                                             ;; Regexps match case-insensitively on the entire entry
                                             :and (:regexp ("space" "NASA")
                                                           ;; Boolean NOT also has implicit OR between selectors
                                                           :not (:regexp "moon" :tag "planet")))))
                     ;; Groups supply their own section names when none are given
                     (:todo "WAITING" :order 8) ; Set order of this section
                     (:todo ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
                            ;; Show this group at the end of the agenda (since it has the
                            ;; highest number). If you specified this group last, items
                            ;; with these todo keywords that e.g. have priority A would be
                            ;; displayed in that group instead, because items are grouped
                            ;; out in the order the groups are listed.
                            :order 9)
                     (:priority<= "B"
                                  ;; Show this section after "Today" and "Important", because
                                  ;; their order is unspecified, defaulting to 0. Sections
                                  ;; are displayed lowest-number-first.
                                  :order 1)
                     ;; After the last group, the agenda will display items that didn't
                     ;; match any of these groups, with the default order position of 99
                     ))))

(ert-deftest org-super-agenda--test-auto-groups ()
  (should (org-super-agenda--test-run
           :groups '((:auto-group t)))))

(ert-deftest org-super-agenda--test-discard-with-2-regexps ()
  (should (org-super-agenda--test-run
           :groups '((:discard (:regexp "pizza"
                                        :regexp "groceries"))))))

(ert-deftest org-super-agenda--test-agenda-with-grid-and-todo-with-children ()
  (should (org-super-agenda--test-run
           :let* (org-agenda-custom-commands
                  '(("u" "Super view"
                     ((agenda "" ((org-super-agenda-groups
                                   '((:name "Today"
                                            :time-grid t)))))
                      (todo "" ((org-super-agenda-groups
                                 '((:name "Projects"
                                          :children t)
                                   (:discard (:anything t))))))))))
           :body (org-agenda nil "u"))))

(ert-deftest org-super-agenda--test-forward-looking ()
  (should (org-super-agenda--test-run
           :groups '((:name "Schedule"
                            :time-grid t)
                     (:name "Today"
                            :scheduled today)
                     (:name "Habits"
                            :habit t)
                     (:name "Due today"
                            :deadline today)
                     (:name "Overdue"
                            :deadline past)
                     (:name "Due soon"
                            :deadline future)
                     (:name "Unimportant"
                            :todo ("SOMEDAY" "MAYBE" "CHECK" "TO-READ" "TO-WATCH")
                            :order 100)
                     (:name "Waiting..."
                            :todo "WAITING"
                            :order 98)
                     (:name "Scheduled earlier"
                            :scheduled past)))))

(ert-deftest org-super-agenda--test-someday-tags-view-Emacs ()
  (should (org-super-agenda--test-run
           :groups '((:todo "SOMEDAY"))
           :body (org-tags-view nil "Emacs"))))
