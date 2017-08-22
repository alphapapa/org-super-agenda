;;  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requirements

(require 'cl)
(require 'cus-edit)
(require 'org-habit)
(require 'org-super-agenda)
(require 'ert)

(require 'ht)
(require 'f)
(require 's)

(load-file "diary-sunrise-sunset.el")

;;;; Variables

(defconst org-super-agenda--test-date "2017-07-05 12:00")
(defvar org-super-agenda--test-results (ht-create))
(defvar org-super-agenda--test-save-results nil)
(defvar org-super-agenda--test-show-results nil)
(defvar org-super-agenda--test-results-file "results.el")

;;;; Commands

(cl-defun org-super-agenda--test-update-all-tests (&optional &key force)
  "Save the result of all tests to the results file."
  (interactive)
  (when (or force
            (equal (read-char "Update all test results? (y/n): ") ?y))
    (when (ht? org-super-agenda--test-results)
      (ht-clear! org-super-agenda--test-results))
    (let ((org-super-agenda--test-save-results t))
      (save-excursion
        (goto-char (point-min))
        (re-search-forward "^;;;; Tests")
        (while (re-search-forward "(org-super-agenda--test-run" nil t)
          (org-super-agenda--test-run-this-test))))))

(defun org-super-agenda--test-save-this-test ()
  "Save the result of this test."
  (interactive)
  (let ((org-super-agenda--test-show-results nil)
        (org-super-agenda--test-save-results t))
    (org-super-agenda--test-run-this-test)))

(defun org-super-agenda--test-show-this-test ()
  "Show the agenda buffer for this test."
  (interactive)
  (let ((org-super-agenda--test-show-results t)
        (org-super-agenda--test-save-results nil))
    (org-super-agenda--test-run-this-test)))

(defun org-super-agenda--test-run-all ()
  "Run all tests with ERT."
  (interactive)
  (ert-run-tests-interactively "^org-super-agenda--test-"))

(defun org-super-agenda--test-load-results ()
  "Load saved results from results file."
  (interactive)
  (setq org-super-agenda--test-results
        (read (f-read org-super-agenda--test-results-file)))
  (when (ht-empty? org-super-agenda--test-results)
    (error "Test results empty")))

;;;; Functions

(cl-defun org-super-agenda--test-process-output-as-string (process &optional &key args stdin ignore-status)
  "Return string of output of PROCESS called with ARGS and STDIN.
ARGS and STDIN are optional. ARGS may be a string or list of
strings. STDIN should be a string. If process returns non-zero
and IGNORE-STATUS is nil, raise `user-error' with STDERR
message."
  (declare (indent defun))
  (let* ((args (internal--listify args))
         status))
  (with-temp-buffer
    (when stdin
      (insert stdin))
    (setq status (apply #'call-process-region (point-min) (point-max)
                        process t '(t t) nil args))
    (unless (or ignore-status
                (= 0 status))
      (user-error (concat (concat process " failed: ")
                          (buffer-string))))
    (buffer-string)))

(defun org-super-agenda--test-diff-strings (a b)
  "Compare strings A and B using the \"diff\" utility."
  (let ((file-a (make-temp-file "argh"))
        (file-b (make-temp-file "argh")))
    (with-temp-file file-a
      (insert a))
    (with-temp-file file-b
      (insert b))
    (with-temp-buffer
      (insert (org-super-agenda--test-process-output-as-string "diff"
                :args (list "-u" file-a file-b)
                :ignore-status t))
      (f-delete file-a)
      (f-delete file-b)
      (goto-char (point-min))
      (forward-line 2)
      (buffer-substring (point) (point-max)))))

(defun org-super-agenda--test-save-result (body-groups-hash result)
  "Save RESULT to `org-super-agenda--test-results' with key BODY-GROUPS-HASH."
  (ht-set! org-super-agenda--test-results body-groups-hash result)
  (with-temp-file org-super-agenda--test-results-file
    (insert (format "%S" org-super-agenda--test-results)))
  (message "Saved result for: %s" body-groups-hash))

(defun org-super-agenda--test-get-custom-group-members (group)
  "Return a list of (VAR STANDARD-VALUE) forms for the customization group GROUP.
Sub-groups of GROUP are recursed into.  The resulting list is
suitable for splicing into a `let' binding form to temporarily
set every variable in GROUP to its standard, un-customized
value."
  (let* ((subgroups (custom-group-members group t))
         (vars (seq-difference (custom-group-members group nil) subgroups)))
    (append (cl-loop for (sg . type) in subgroups
                     append (org-super-agenda--test-get-custom-group-members sg))
            (cl-loop for (var . type) in vars
                     for sv = (car (get var 'standard-value))
                     when sv
                     collect (list var sv)))))

;;;; Macros

(cl-defmacro org-super-agenda--test-with-redefined-functions (fns &rest body)
  "Run BODY with functions redefined according to FNS.
FNS should be a list of (FUNCTION-NAME FUNCTION-BODY) lists.
This is helpful when, for whatever reason, `cl-flet' and
`cl-labels' don't work."
  (declare (indent defun))
  (let* ((set-forms (cl-loop for (fn def) in fns
                             for orig = (intern (concat (symbol-name fn) "-orig"))
                             collect `(setf (symbol-function ',orig) (symbol-function ',fn))
                             collect `(setf (symbol-function ',fn) ,def)))
         (unset-forms (cl-loop for (fn def) in fns
                               for orig = (intern (concat (symbol-name fn) "-orig"))
                               collect `(setf (symbol-function ',fn) (symbol-function ',orig)))))
    `(progn
       (unwind-protect
           (progn
             ,@set-forms
             ,@body)
         ,@unset-forms))))

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
          (date org-super-agenda--test-date)
          let*)
  "Test BODY with GROUPS and LET* binding.
When `org-super-agenda--test-save-results' is non-nil, save the
result to the results file.  When
`org-super-agenda--test-show-results' is non-nil, show the agenda
buffer and do not save the results."
  (declare (debug (form &optional listp sexp sexp stringp)))
  `(progn
     (org-super-agenda-mode 1)
     (let ((body-groups-hash (secure-hash 'md5 (format "%S" (list ',body ,groups))))
           result format-time-string-orig)

       ;; Redefine functions
       (org-super-agenda--test-with-redefined-functions
         ;; Rebind `format-time-string' so it always returns the
         ;; same when no time is given, otherwise the "now" line
         ;; in the time-grid depends on the real time when the
         ;; test is run.  (For some reason, cl-labels/cl-flet
         ;; don't work, so we have to use `setf' and
         ;; `symbol-function'.  Might have something to do with
         ;; lexical-binding.)
         ((format-time-string (lambda (format-string &optional time zone)
                                (if time
                                    (format-time-string-orig format-string time zone)
                                  (concat (second (s-split " " ,date)) " "))))
          (window-width (lambda ()
                          134)))

         ;; Run agenda
         (org-super-agenda--test-with-org-today-date ,date
           (let* (;; Set these vars so they are consistent between my config and the batch config
                  ,@(org-super-agenda--test-get-custom-group-members 'org-agenda)
                  ,@(org-super-agenda--test-get-custom-group-members 'org-habit)
                  (org-agenda-window-setup 'current-window)  ; The default breaks batch tests by trying to open a new frame
                  (org-agenda-files (list "test.org"))
                  ,@(if let*
                        let*
                      `((ignore nil)))
                  ,(if groups-set
                       `(org-super-agenda-groups ,groups)
                     `(ignore nil))
                  ,(if span
                       `(org-agenda-span ',span)
                     `(ignore nil))
                  string)
             ,body
             ;; We ignore the text properties.  This should be the right thing to do, since we
             ;; never modify them.  It also makes the results actually legible.  NOTE: We
             ;; could collapse the whitespace to avoid annoying discrepancies between my
             ;; config and the default org-agenda config used in batch mode.  This is probably
             ;; fine, because other than inserting group headers, we're not modifying any
             ;; whitespace.  BUT that means that, when a test fails, we won't be able to
             ;; easily see why, because there won't be any line-breaks for the diff.
             (setq result (buffer-substring-no-properties 1 (point-max)))
             (unless org-super-agenda--test-show-results
               (kill-buffer)))))

       ;; Save test results
       (when org-super-agenda--test-save-results
         (org-super-agenda--test-save-result body-groups-hash result))

       ;; Show test results
       (unless org-super-agenda--test-show-results
         ;; Don't give real test result when showing result buffer
         (or (equal result (ht-get org-super-agenda--test-results body-groups-hash))
             (error "Test failed: DIFF: %s"
                    (org-super-agenda--test-diff-strings (ht-get org-super-agenda--test-results body-groups-hash) result)))))))

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
           :let* ((org-agenda-custom-commands
                   '(("u" "Super view"
                      ((agenda "" ((org-super-agenda-groups
                                    '((:name "Today"
                                             :time-grid t)))))
                       (todo "" ((org-super-agenda-groups
                                  '((:name "Projects"
                                           :children t)
                                    (:discard (:anything t)))))))))))
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
