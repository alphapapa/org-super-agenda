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

(defconst org-super-agenda-test-date "2017-07-05 12:00")
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
    (ert-run-tests-batch "^org-super-agenda--")))

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
  (let* ((args (internal--listify args))
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
                     collect (list var sv)))))

;;;; Macros

(cl-defmacro org-super-agenda-test--with-mock-functions (fns &rest body)
  "Run BODY with functions redefined according to FNS.
FNS should be a list of (FUNCTION-NAME FUNCTION-BODY) lists,
where FUNCTION-BODY is a lambda form."
  (declare (indent defun))
  `(cl-letf ,(cl-loop for (fn def) in fns
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
         ;; Rebind `format-time-string' so it always returns the
         ;; same when no time is given, otherwise the "now" line
         ;; in the time-grid depends on the real time when the
         ;; test is run.  (For some reason, cl-labels/cl-flet
         ;; don't work, so we have to use `setf' and
         ;; `symbol-function'.  Might have something to do with
         ;; lexical-binding.)
         ((format-time-string-orig (symbol-function 'format-time-string))
          (format-time-string (lambda (format-string &optional time zone)
                                (if time
                                    (format-time-string-orig format-string time zone)
                                  (concat (cl-second (s-split " " ,date)) " "))))
          (frame-width (lambda ()
                         134))
          (window-width (lambda ()
                          134))
          ;; Org after 2017-08-08 uses `window-text-width'
          (window-text-width (lambda ()
                               134)))

         ;; Run agenda
         (org-super-agenda-test--with-org-today-date ,date
           (let* ( ;; Set these vars so they are consistent between my config and the batch config
                  ,@(org-super-agenda-test--get-custom-group-members 'org-agenda)
                  ,@(org-super-agenda-test--get-custom-group-members 'org-habit)
                  (org-agenda-window-setup 'current-window) ; The default breaks batch tests by trying to open a new frame
                  (org-agenda-start-with-log-mode nil) ; Set this by default, in case it's set to t in my running Emacs instance
                  ;; HACK: Look for test.org in either dir, so it works interactively and
                  ;; in batch tests.  This is ugly, but I don't know how else to do it.
                  (org-agenda-files (list (if (file-exists-p "test/test.org")
                                              "test/test.org"
                                            "test.org")))
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
             (setq new-result (buffer-substring-no-properties 1 (point-max)))
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
