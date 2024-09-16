;;  -*- lexical-binding: t; -*-

;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'cus-edit)
(require 'org-habit)
(require 'org-super-agenda)
(require 'ert)
(require 'seq)

(require 'ht)
(require 'f)
(require 's)

;;;; Variables

(eval-and-compile
  ;; NOTE: Using eval-and-compile was not necessary in the past, but
  ;; now for some reason, without it, running the tests signals a
  ;; void-variable error.  It seems to make no sense, but this seems
  ;; to fix it.
  (defconst org-super-agenda-test-date "2017-07-05 12:00"))
(defvar org-super-agenda-test-results (ht-create))
(defvar org-super-agenda-test-save-results nil)
(defvar org-super-agenda-test-show-results nil)
(defvar org-super-agenda-test-results-file
  (expand-file-name "test/results.el" (locate-dominating-file default-directory ".git")))

;;;; Diary-sunset

(require 'solar)
(require 'cal-dst)

(defun org-super-agenda-test--diary-sunrise-sunset-split ()
  "Split `diary-sunrise-sunset' into sunrise, sunset, and daylight hours."
  (let* ((calendar-latitude 0)
         (calendar-longitude 0)
         (calendar-time-zone (if (dst-in-effect (date-to-day org-super-agenda-test-date))
                                 0
                               60))
         (string (diary-sunrise-sunset))
         (regexp (rx (group "Sunrise " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")") ", "
                     (group "sunset " (1+ (or digit ":")) (or "am" "pm")) " "
                     (group "(" (1+ alpha) ")")
                     (1+ anything)
                     "(" (group (1+ (or digit ":")))
                     ))
         (sunrise (progn
                    (string-match regexp string)
                    (match-string 1 string)) )
         (sunset (capitalize (match-string 3 string)))
         (daylight (format "%s of daylight" (match-string 5 string))))
    (list sunrise sunset daylight)))

(defun org-super-agenda-test--diary-sunrise ()
  (let ((s (org-super-agenda-test--diary-sunrise-sunset-split)))
    (format "%s (%s)" (cl-first s) (cl-third s))))

(defun org-super-agenda-test--diary-sunset ()
  (cl-second (org-super-agenda-test--diary-sunrise-sunset-split)))

;;;; Workarounds

;; Emacs 28.1 wraps ERT output and escapes newlines in it, which makes
;; the results of our test suite unreadable.  Since the relevant
;; variables are bound in the ERT function bodies, the only way to
;; override them is to redefine or advise the relevant ERT functions.
;; Of course, this may introduce problems if these functions change in
;; future Emacs versions, but we seem to have no choice but to copy
;; the function definitions and change the bindings.

(setq ert-batch-backtrace-right-margin nil)

(advice-add #'ert--pp-with-indentation-and-newline :override
            (lambda (object)
              "Pretty-print OBJECT, indenting it to the current column of point.
Ensures a final newline is inserted."
              (let ((begin (point))
                    (print-escape-newlines nil)
                    (pp-escape-newlines nil)
                    (print-escape-control-characters nil))
                (pp object (current-buffer))
                (unless (bolp) (insert "\n"))
                (save-excursion
                  (goto-char begin)
                  (indent-sexp)))))

(advice-add #'ert-reason-for-test-result :override
            (lambda (result)
              "Return the reason given for RESULT, as a string.

The reason is the argument given when invoking `ert-fail' or `ert-skip'.
It is output using `prin1' prefixed by two spaces.

If no reason was given, or for a successful RESULT, return the
empty string."
              (let ((reason
                     (and
                      (ert-test-result-with-condition-p result)
                      (cadr (ert-test-result-with-condition-condition result))))
                    (print-escape-newlines nil)
                    (print-level 6)
                    (print-length 10))
                (if reason (format "  %S" reason) ""))))

(advice-add #'ert-run-tests-batch :override
            (lambda (&optional selector)
              "Run the tests specified by SELECTOR, printing results to the terminal.

SELECTOR works as described in `ert-select-tests', except if
SELECTOR is nil, in which case all tests rather than none will be
run; this makes the command line \"emacs -batch -l my-tests.el -f
ert-run-tests-batch-and-exit\" useful.

Returns the stats object."
              (unless selector (setq selector 't))
              (ert-run-tests
               selector
               (lambda (event-type &rest event-args)
                 (cl-ecase event-type
                   (run-started
                    (unless ert-quiet
                      (cl-destructuring-bind (stats) event-args
                        (message "Running %s tests (%s, selector `%S')"
                                 (length (ert--stats-tests stats))
                                 (ert--format-time-iso8601 (ert--stats-start-time stats))
                                 selector))))
                   (run-ended
                    (cl-destructuring-bind (stats abortedp) event-args
                      (let ((unexpected (ert-stats-completed-unexpected stats))
                            (skipped (ert-stats-skipped stats))
		            (expected-failures (ert--stats-failed-expected stats)))
                        (message "\n%sRan %s tests, %s results as expected, %s unexpected%s (%s, %f sec)%s\n"
                                 (if (not abortedp)
                                     ""
                                   "Aborted: ")
                                 (ert-stats-total stats)
                                 (ert-stats-completed-expected stats)
                                 unexpected
                                 (if (zerop skipped)
                                     ""
                                   (format ", %s skipped" skipped))
                                 (ert--format-time-iso8601 (ert--stats-end-time stats))
                                 (float-time
                                  (time-subtract
                                   (ert--stats-end-time stats)
                                   (ert--stats-start-time stats)))
                                 (if (zerop expected-failures)
                                     ""
                                   (format "\n%s expected failures" expected-failures)))
                        (unless (zerop unexpected)
                          (message "%s unexpected results:" unexpected)
                          (cl-loop for test across (ert--stats-tests stats)
                                   for result = (ert-test-most-recent-result test) do
                                   (when (not (ert-test-result-expected-p test result))
                                     (message "%9s  %S%s"
                                              (ert-string-for-test-result result nil)
                                              (ert-test-name test)
                                              (if (getenv "EMACS_TEST_VERBOSE")
                                                  (ert-reason-for-test-result result)
                                                ""))))
                          (message "%s" ""))
                        (unless (zerop skipped)
                          (message "%s skipped results:" skipped)
                          (cl-loop for test across (ert--stats-tests stats)
                                   for result = (ert-test-most-recent-result test) do
                                   (when (ert-test-result-type-p result :skipped)
                                     (message "%9s  %S%s"
                                              (ert-string-for-test-result result nil)
                                              (ert-test-name test)
                                              (if (getenv "EMACS_TEST_VERBOSE")
                                                  (ert-reason-for-test-result result)
                                                ""))))
                          (message "%s" "")))))
                   (test-started
                    )
                   (test-ended
                    (cl-destructuring-bind (stats test result) event-args
                      (unless (ert-test-result-expected-p test result)
                        (cl-etypecase result
                          (ert-test-passed
                           (message "Test %S passed unexpectedly" (ert-test-name test)))
                          (ert-test-result-with-condition
                           (message "Test %S backtrace:" (ert-test-name test))
                           (with-temp-buffer
                             (insert (backtrace-to-string
                                      (ert-test-result-with-condition-backtrace result)))
                             (if (not ert-batch-backtrace-right-margin)
                                 (message "%s"
                                          (buffer-substring-no-properties (point-min)
                                                                          (point-max)))
                               (goto-char (point-min))
                               (while (not (eobp))
                                 (let ((start (point))
                                       (end (line-end-position)))
                                   (setq end (min end
                                                  (+ start
                                                     ert-batch-backtrace-right-margin)))
                                   (message "%s" (buffer-substring-no-properties
                                                  start end)))
                                 (forward-line 1))))
                           (with-temp-buffer
                             (ert--insert-infos result)
                             (insert "    ")
                             (let ((print-escape-newlines nil)
                                   (print-level 5)
                                   (print-length 10))
                               (ert--pp-with-indentation-and-newline
                                (ert-test-result-with-condition-condition result)))
                             (goto-char (1- (point-max)))
                             (cl-assert (looking-at "\n"))
                             (delete-char 1)
                             (message "Test %S condition:" (ert-test-name test))
                             (message "%s" (buffer-string))))
                          (ert-test-aborted-with-non-local-exit
                           (message "Test %S aborted with non-local exit"
                                    (ert-test-name test)))
                          (ert-test-quit
                           (message "Quit during %S" (ert-test-name test)))))
                      (unless ert-quiet
                        (let* ((max (prin1-to-string (length (ert--stats-tests stats))))
                               (format-string (concat "%9s  %"
                                                      (prin1-to-string (length max))
                                                      "s/" max "  %S (%f sec)")))
                          (message format-string
                                   (ert-string-for-test-result result
                                                               (ert-test-result-expected-p
                                                                test result))
                                   (1+ (ert--stats-test-pos stats test))
                                   (ert-test-name test)
                                   (ert-test-result-duration result))))))))
               nil)))

;;;; Commands

(cl-defun org-super-agenda-test--update-all ()
  "Save the result of all tests to the results file.
Saves results of already-defined tests."
  (interactive)
  (when (ht? org-super-agenda-test-results)
    (ht-clear! org-super-agenda-test-results))
  ;; Don't fsync every time the result file is written, which is slow.
  (let ((write-region-inhibit-fsync t)
        (org-super-agenda-test-save-results t))
    (ert-run-tests-batch "^org-super-agenda-test--")))

(defun org-super-agenda-test--save-this-result ()
  "Save the result of this test."
  (interactive)
  (let ((org-super-agenda-test-show-results nil)
        (org-super-agenda-test-save-results t))
    (org-super-agenda-test--run-this-test)
    ;; Re-eval the ERT test
    (org-super-agenda-test--ert-def-this-test)))

(defun org-super-agenda-test--show-this-result ()
  "Show the agenda buffer for this test."
  (interactive)
  (let ((org-super-agenda-test-show-results t)
        (org-super-agenda-test-save-results nil))
    (org-super-agenda-test--run-this-test)))

(defun org-super-agenda-test--run-this-test ()
  (save-excursion
    (unless (bolp)
      (beginning-of-defun))
    (let ((eod (save-excursion
                 (end-of-defun)
                 (point))))
      (while (re-search-forward "(org-super-agenda-test--run" nil t)
        (save-excursion
          (unless (> (point) eod)
            (goto-char (match-beginning 0))
            (forward-sexp)
            (eval-last-sexp nil)))))))

(defun org-super-agenda-test--ert-def-this-test ()
  (save-excursion
    (unless (bolp)
      (beginning-of-defun))
    (forward-char 1)
    (eval-defun nil)))

(defun org-super-agenda-test--run-all ()
  "Run all tests with ERT."
  (interactive)
  (when (ht-empty? org-super-agenda-test-results)
    (message "Loading test results...")
    (org-super-agenda-test--load-results))
  (ert-run-tests-interactively "^org-super-agenda-test--"))

(defun org-super-agenda-test--load-results ()
  "Load saved results from results file."
  (interactive)
  (setq org-super-agenda-test-results
        (read (f-read org-super-agenda-test-results-file)))
  (when (ht-empty? org-super-agenda-test-results)
    (error "Test results empty"))
  (message "Test results loaded"))

;;;; Functions

(cl-defun org-super-agenda-test--process-output-as-string (process &optional &key args stdin ignore-status)
  "Return string of output of PROCESS called with ARGS and STDIN.
ARGS and STDIN are optional. ARGS may be a string or list of
strings. STDIN should be a string. If process returns non-zero
and IGNORE-STATUS is nil, raise `user-error' with STDERR
message."
  (declare (indent defun))
  (let* ((args (cl-typecase args
                 (list args)
                 (otherwise (list args))))
         status)
    (with-temp-buffer
      (when stdin
        (insert stdin))
      (setq status (apply #'call-process-region (point-min) (point-max)
                          process t '(t t) nil args))
      (unless (or ignore-status
                  (= 0 status))
        (user-error (concat (concat process " failed: ")
                            (buffer-string))))
      (buffer-string))))

(defun org-super-agenda-test--diff-strings (a b)
  "Compare strings A and B using the \"diff\" utility."
  (cl-loop for s in (list a b)
           unless (stringp s)
           do (error "Unable to diff non-string: %s is: %s" (symbol-name s) s))
  (let* ((write-region-inhibit-fsync t)
         (file-a (make-temp-file "argh"))
         (file-b (make-temp-file "argh")))
    (with-temp-file file-a
      (insert a))
    (with-temp-file file-b
      (insert b))
    (with-temp-buffer
      (insert (org-super-agenda-test--process-output-as-string "diff"
                :args (list "-u" file-a file-b)
                :ignore-status t))
      (f-delete file-a)
      (f-delete file-b)
      (goto-char (point-min))
      (forward-line 2)
      (buffer-substring (point) (point-max)))))

(defun org-super-agenda-test--save-result (body-groups-hash result)
  "Save RESULT to `org-super-agenda-test-results' with key BODY-GROUPS-HASH."
  (ht-set! org-super-agenda-test-results body-groups-hash result)
  (let ((print-level 999999)
        (print-length 999999))
    ;; Set these, otherwise the formatted hash table will be truncated
    (with-temp-file org-super-agenda-test-results-file
      (insert (format "%S" org-super-agenda-test-results))))
  (message "Saved result for: %s" body-groups-hash))

(eval-and-compile
  ;; NOTE: Similarly to the variable defined earlier, a void-function
  ;; error is now signaled unless this definition is wrapped in
  ;; eval-and-compile.  I can't explain why.
  (defun org-super-agenda-test--get-custom-group-members (group)
    "Return a list of (VAR STANDARD-VALUE) forms for the customization group GROUP.
Sub-groups of GROUP are recursed into.  The resulting list is
suitable for splicing into a `let' binding form to temporarily
set every variable in GROUP to its standard, un-customized
value."
    (let* ((subgroups (custom-group-members group t))
           (vars (seq-difference (custom-group-members group nil) subgroups)))
      (append (cl-loop for (sg . type) in subgroups
                       append (org-super-agenda-test--get-custom-group-members sg))
              (cl-loop for (var . type) in vars
                       for sv = (car (get var 'standard-value))
                       when sv
                       collect (list var sv))))))

;;;; Macros

(cl-defmacro org-super-agenda-test--with-mock-functions (fns &rest body)
  "Run BODY with functions redefined according to FNS.
FNS should be a list of (FUNCTION-NAME FUNCTION-BODY) lists,
where FUNCTION-BODY is a lambda form."
  (declare (indent defun))
  `(cl-letf* ,(cl-loop for (fn def) in fns
                       collect `((symbol-function ',fn)
                                 ,def))
     ,@body))

(defmacro org-super-agenda-test--with-org-today-date (date &rest body)
  "Run BODY with the `org-today' function set to return simply DATE.
  DATE should be a date-time string (both date and time must be included)."
  (declare (indent defun))
  `(org-super-agenda-test--with-mock-functions
     ((org-today (lambda ()
                   ,(date-to-day date))))
     ,@body))

(cl-defmacro org-super-agenda-test--run
    (&key (body '(org-agenda-list))
          (groups nil groups-set)
          (span 'day)
          (date org-super-agenda-test-date)
          (data-file "test/test.org")
          (skip-lines nil)
          let*)
  "Test BODY with GROUPS and LET* binding.
When `org-super-agenda-test-save-results' is non-nil, save the
new-result to the results file.  When
`org-super-agenda-test-show-results' is non-nil, show the agenda
buffer and do not save the results.  Load test results if not
already loaded."
  (declare (debug (form &optional listp sexp sexp stringp)))
  `(progn
     (unless (> (ht-size org-super-agenda-test-results) 0)
       ;; This allows `ert-run-tests-batch-and-exit' to work automatically.
       (org-super-agenda-test--load-results))
     (org-super-agenda-mode 1)
     (let ((body-groups-hash (secure-hash 'md5 (format "%S" (list ',body ,groups))))
           new-result)

       ;; Redefine functions
       (org-super-agenda-test--with-mock-functions
         ((frame-width (lambda (&rest _ignore)
                         134))
          (window-width (lambda (&rest _ignore)
                          134))
          ;; Org after 2017-08-08 uses `window-text-width'
          (window-text-width (lambda (&rest _ignore)
                               134))
          ;; Org after 81a2fe4f0b6d5bb91e96fc91f8550f2dce8d1185 uses `window-max-chars-per-line'.
          (window-max-chars-per-line (lambda (&rest _ignore) 134)))

         ;; Run agenda
         (org-super-agenda-test--with-org-today-date ,date
           (let* (;; Set these vars so they are consistent between my config and the batch config
                  ,@(org-super-agenda-test--get-custom-group-members 'org-agenda)
                  ,@(org-super-agenda-test--get-custom-group-members 'org-habit)
                  (org-agenda-window-setup 'current-window) ; The default breaks batch tests by trying to open a new frame
                  (org-agenda-start-with-log-mode nil) ; Set this by default, in case it's set to t in my running Emacs instance
                  (org-agenda-current-time-string "now - - - - - - - - - - - - - - - - - - - - - - - - -")
                  (org-agenda-block-separator ?=)
                  ;; HACK: Look for test.org in either dir, so it works interactively and
                  ;; in batch tests.  This is ugly, but I don't know how else to do it.
                  (org-agenda-files (list (if (file-exists-p ,data-file)
                                              (expand-file-name ,data-file)
                                            (expand-file-name ,data-file))))
                  ,@(if let*
                        let*
                      `((_ignore nil)))
                  ,(if groups-set
                       `(org-super-agenda-groups ,groups)
                     `(_ignore nil))
                  ,(if span
                       `(org-agenda-span ',span)
                     `(_ignore nil))
                  _string)
             ;; Fix org-agenda variables whose value is different on
             ;; graphical terminals in later Org versions.
             (setf (nth 2 org-agenda-time-grid) "......"
                   (nth 3 org-agenda-time-grid) "----------------")
             (unwind-protect
                 (progn
                   ;; Advise `format-time-string' so it always returns the same when no time is given,
                   ;; otherwise the "now" line in the time-grid depends on the real time when the test
                   ;; is run.  NOTE: Due to ERT's calling `format-time-string' when a test fails, for
                   ;; some reason, using `cl-letf*' to rebind `format-time-string' prevents ERT from
                   ;; using it to log a timestamp, so we use advice here, which seems to work.
                   (advice-add #'format-time-string :around
                               (defun org-super-agenda-test/format-time-string (orig-fn format-string &optional time zone)
                                 (if time
                                     (funcall orig-fn format-string time zone)
                                   (concat (cl-second (s-split " " ,date)) " "))))
                   ,body)
               (advice-remove #'format-time-string 'org-super-agenda-test/format-time-string))
             ;; We ignore the text properties.  This should be the right thing to do, since we
             ;; never modify them.  It also makes the results actually legible.  NOTE: We
             ;; could collapse the whitespace to avoid annoying discrepancies between my
             ;; config and the default org-agenda config used in batch mode.  This is probably
             ;; fine, because other than inserting group headers, we're not modifying any
             ;; whitespace.  BUT that means that, when a test fails, we won't be able to
             ;; easily see why, because there won't be any line-breaks for the diff.
             (setq new-result (buffer-substring-no-properties
                               (if ,skip-lines
                                   (save-excursion
                                     (forward-line ,skip-lines)
                                     (point))
                                 1)
                               (point-max)))
             (unless org-super-agenda-test-show-results
               (kill-buffer)))))

       ;; Save test results
       (when org-super-agenda-test-save-results
         (org-super-agenda-test--save-result body-groups-hash new-result))

       ;; Show test results
       (unless org-super-agenda-test-show-results
         ;; Don't give real test result when showing new-result buffer
         (let ((stored-result (ht-get org-super-agenda-test-results body-groups-hash)))
           (or (equal stored-result new-result)
               (unless (and stored-result new-result)
                 (error "Empty result for body:%s\nSTORED-RESULT:%s\nNEW-RESULT:%s"
                        body-groups-hash stored-result new-result))
               (error "Test failed for body:%s\nDIFF:\n %s" body-groups-hash
                      (org-super-agenda-test--diff-strings stored-result new-result))))))))

;;;; Tests

;;;;; Complex

(ert-deftest org-super-agenda-test--no-groups ()
  (should (org-super-agenda-test--run
           :groups nil)))

(ert-deftest org-super-agenda-test--time-grid-with-unprioritized-order-100 ()
  (should (org-super-agenda-test--run
           :groups '((:name "Today"
                            :time-grid t)
                     (:name "Unprioritized"
                            :not (:priority>= "C")
                            :order 100)))))

(ert-deftest org-super-agenda-test--priority>=B ()
  (should (org-super-agenda-test--run
           :groups '((:priority>= "B")))))

(ert-deftest org-super-agenda-test--priority<B-with-order-100 ()
  (should (org-super-agenda-test--run
           :groups '((:priority< "B" :order 100)))))

(ert-deftest org-super-agenda-test--heading-regexp ()
  (should (org-super-agenda-test--run
           :groups '((:heading-regexp "moon")))))

(ert-deftest org-super-agenda-test--main-example ()
  (should (org-super-agenda-test--run
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

(ert-deftest org-super-agenda-test--auto-groups ()
  (should (org-super-agenda-test--run
           :groups '((:auto-group t)))))

(ert-deftest org-super-agenda-test--auto-property ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-property "agenda-group")))))

(ert-deftest org-super-agenda-test--auto-outline-path ()
  ;; TODO: This is really useful.  It should be an example in the docs.
  (should (org-super-agenda-test--run
           :groups '((:auto-outline-path t)))))

(ert-deftest org-super-agenda-test--auto-parent ()
  ;; DONE: Works.
  ;; TODO: This is really useful.  It should be an example in the docs.
  (should (org-super-agenda-test--run
           :groups '((:auto-parent t)))))

(ert-deftest org-super-agenda-test--auto-dir-name ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-dir-name t)))))

(ert-deftest org-super-agenda-test--discard-with-2-regexps ()
  (should (org-super-agenda-test--run
           :groups '((:discard (:regexp "pizza"
                                        :regexp "groceries"))))))

(ert-deftest org-super-agenda-test--agenda-with-grid-and-todo-with-children ()
  (should (org-super-agenda-test--run
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

(ert-deftest org-super-agenda-test--forward-looking ()
  (should (org-super-agenda-test--run
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

(ert-deftest org-super-agenda-test--someday-tags-view-Emacs ()
  (should (org-super-agenda-test--run
           :skip-lines 3
           :groups '((:todo "SOMEDAY"))
           :body (org-tags-view nil "Emacs"))))

;;;;; Single-selector

(ert-deftest org-super-agenda-test--:category ()
  (should (org-super-agenda-test--run
           :groups '((:category "ideas")))))
(ert-deftest org-super-agenda-test--:category-multi ()
  (should (org-super-agenda-test--run
           :groups '((:category ("ambition" "ideas"))))))

(ert-deftest org-super-agenda-test--:children-nil ()
  (should (org-super-agenda-test--run
           ;; DONE: Works.
           :groups '((:children nil)))))

(ert-deftest org-super-agenda-test--:children-t ()
  ;; DONE Works.
  (should (org-super-agenda-test--run
           :groups '((:children t)))))

(ert-deftest org-super-agenda-test--:children-todo ()
  (should (org-super-agenda-test--run
           ;; DONE: Works.
           ;; FIXME: This test sort of works, but it passed even with a bug present in the code.
           ;; See <https://github.com/alphapapa/org-super-agenda/issues/75#issuecomment-519839287>.
           :groups '((:children todo)))))

(ert-deftest org-super-agenda-test--:children-string ()
  (should (org-super-agenda-test--run
           ;; DONE: Works.  (This groups items that have child tasks
           ;; with the "WAITING" keyword, so the "WAITING" task is not
           ;; in the group, but its parent is.)
           :groups '((:children "WAITING")))))

(ert-deftest org-super-agenda-test--:date ()
  (should (org-super-agenda-test--run
           ;; FIXME: Note that `ts-date' property is unset with,
           ;; e.g. `org-todo-list', so that selector won't have any
           ;; effect then.

           ;; TODO: Come up with a way to test :date, because in the
           ;; daily/weekly agenda, every item has a date, so it's
           ;; redundant.  I already know that it works to test the
           ;; `ts-date' property, but I'd like a more meaningful test.
           :groups '((:date t)))))

(ert-deftest org-super-agenda-test--:deadline-t ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline t)))))
(ert-deftest org-super-agenda-test--:deadline-nil ()
  ;; DONE: Works.  I don't remember why I have this one using
  ;; `org-todo-list', but I'll leave it.
  (should (org-super-agenda-test--run
           :skip-lines 4
           :groups '((:deadline nil))
           :body (org-todo-list))))
(ert-deftest org-super-agenda-test--:deadline-nil-agenda ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline nil)))))
(ert-deftest org-super-agenda-test--:deadline-past ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline past))
           :date "2017-07-06 12:00")))
(ert-deftest org-super-agenda-test--:deadline-today ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline today)))))
(ert-deftest org-super-agenda-test--:deadline-future ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline future)))))
(ert-deftest org-super-agenda-test--:deadline-before ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline (before "2017-07-10"))))))
(ert-deftest org-super-agenda-test--:deadline-after ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:deadline (after "2017-07-10"))))))

(ert-deftest org-super-agenda-test--:effort< ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:effort< "5")))))
(ert-deftest org-super-agenda-test--:effort> ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:effort> "5")))))

(ert-deftest org-super-agenda-test--:file-path ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:file-path "/test/")))))

(ert-deftest org-super-agenda-test--:habit ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:habit t)))))

(ert-deftest org-super-agenda-test--:heading-regexp ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:heading-regexp "moon")))))

(ert-deftest org-super-agenda-test--:log ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:log t))
           :let* ((org-agenda-show-log t)))))

(ert-deftest org-super-agenda-test--:pred ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:pred (lambda (item)
                              (s-contains? "moon" item)))))))

(ert-deftest org-super-agenda-test--:property-only ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:property "Effort")))))

(ert-deftest org-super-agenda-test--:property-list-without-value ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:property ("Effort"))))))

(ert-deftest org-super-agenda-test--:property-list-with-value ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:property ("Effort" "5"))))))

(ert-deftest org-super-agenda-test--:property-list-with-pred ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:property ("Effort" (lambda (v)
					    (when v
					      (<= (string-to-number v) 5)))))))))

(ert-deftest org-super-agenda-test--:property-list-with-symbol ()
  ;; DONE: Works.
  (let ((debug-on-error nil))
    (should-error (org-super-agenda-test--run
                   :groups '((:property ("Effort" 'sym-not-allowed-here)))))))

(ert-deftest org-super-agenda-test--:priority ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:priority "A")))))
(ert-deftest org-super-agenda-test--:priority> ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:priority> "B")))))
(ert-deftest org-super-agenda-test--:priority>= ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:priority>= "B")))))
(ert-deftest org-super-agenda-test--:priority< ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:priority< "A")))))
(ert-deftest org-super-agenda-test--:priority<= ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:priority<= "B")))))

(ert-deftest org-super-agenda-test--:regexp ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:regexp "take over")))))

(ert-deftest org-super-agenda-test--:scheduled-t ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled t)))))
(ert-deftest org-super-agenda-test--:scheduled-nil ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled nil)))))
(ert-deftest org-super-agenda-test--:scheduled-past ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled past)))))
(ert-deftest org-super-agenda-test--:scheduled-today ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled today)))))
(ert-deftest org-super-agenda-test--:scheduled-future ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :date "2017-07-04 12:00"
           :span 2
           :groups '((:scheduled future)))))
(ert-deftest org-super-agenda-test--:scheduled-before ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled (before "2017-07-05"))))))
(ert-deftest org-super-agenda-test--:scheduled-after ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:scheduled (after "2017-07-04"))))))

(ert-deftest org-super-agenda-test--:tag ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:tag "space")))))

(ert-deftest org-super-agenda-test--:tag-with-inheritance ()
  (should (org-super-agenda-test--run
           :let* ((org-agenda-use-tag-inheritance t))
           :groups '((:tag "universe")))))

(ert-deftest org-super-agenda-test--:time-grid ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:time-grid t)))))

(ert-deftest org-super-agenda-test--:todo ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:todo "WAITING")))))

;;;;;; Special selectors

(ert-deftest org-super-agenda-test--:and ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:and (:regexp "Take over" :todo "TODO"))))))

(ert-deftest org-super-agenda-test--:anything ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:anything t)))))

(ert-deftest org-super-agenda-test--:auto-category ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-category t)))))

(ert-deftest org-super-agenda-test--:auto-planning ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-planning t)))))

(ert-deftest org-super-agenda-test--:auto-group ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-group t)))))

(ert-deftest org-super-agenda-test--:auto-map ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-map (lambda (item)
				  (when-let* ((pos (text-property-not-all 0 (length item)
									  'face nil item)))
                                    (format "Face: %s"
					    (get-text-property pos 'face item)))))))))

(ert-deftest org-super-agenda-test--:auto-tags ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-tags t)))))

(ert-deftest org-super-agenda-test--:auto-ts ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:auto-ts t)))))

(ert-deftest org-super-agenda-test--:discard ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:discard (:anything t))))))

(ert-deftest org-super-agenda-test--:not ()
  ;; FIXME: The :not selector causes auto-generated group names to be incorrect.
  ;; DONE: Works otherwise.
  (should (org-super-agenda-test--run
           :groups '((:not (:todo t))))))

(ert-deftest org-super-agenda-test--with-retained-sorting ()
  ;; FIXME: This seems to be slightly broken on Org 9.6.6.  Maybe <https://github.com/alphapapa/org-super-agenda/issues/207>?
  (should (org-super-agenda-test--run
           :groups '((:name "Ambitions vs Bills with retained sorting"
                            :and (:todo "TODO" :priority>= "B" :tag "ambition")
                            :and (:todo "TODO" :priority>= "B" :tag "bills")
                            :discard (:anything)))
           :let* ((org-agenda-sorting-strategy '(priority-down tag-down))
                  (org-super-agenda-keep-order t)))))

(ert-deftest org-super-agenda-test--without-retained-sorting ()
  (should (org-super-agenda-test--run
           :groups '((:name "Ambitions vs Bills without retained sorting"
                            :and (:todo "TODO" :priority>= "B" :tag "ambition")
                            :and (:todo "TODO" :priority>= "B" :tag "bills")
                            :discard (:anything)))
           :let* ((org-agenda-sorting-strategy '(priority-down tag-down))
                  (org-super-agenda-keep-order nil)))))

(ert-deftest org-super-agenda-test--:order ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:name "Last"
                            :order 100
                            :todo "WAITING")))))

(ert-deftest org-super-agenda-test--:order-multi ()
  ;; DONE: Works.
  (should (org-super-agenda-test--run
           :groups '((:order-multi (100
                                    (:todo "WAITING")
                                    (:name "Not TODOs"
                                           :not (:todo t))))))))

(ert-deftest org-super-agenda-test--issue-264 ()
  ;; See <https://github.com/alphapapa/org-super-agenda/issues/264>.
  ;; DONE: Works.
  (should (org-super-agenda-test--run
	   :body (org-todo-list)
           :data-file "test/264.org"
           :skip-lines 3
           :groups '((:name "time-grid" :time-grid t)
                     (:name "Todo" :todo "TODO")
                     (:name "catch-all" :anything t)))))
