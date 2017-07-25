;;; Commentary:

;; To try this out, evaluate this file and run this code to open an example:

;; (let ((org-agenda-custom-commands (list (quote ("u" "SUPER Agenda"
;;                                                 org-super-agenda ""
;;                                                 ((super-filters '(osa/separate-by-time
;;                                                                   (:fn osa/separate-by-any-tag :args ("bills"))
;;                                                                   osa/separate-by-habit
;;                                                                   (:fn osa/separate-by-todo-keyword :args "WAITING")
;;                                                                   (:fn osa/separate-by-todo-keyword
;;                                                                        :args ("SOMEDAY" "TO-READ" "CHECK" "TO-WATCH" "WATCHING")
;;                                                                        :last t)
;;                                                                   (:fn osa/separate-by-priority :args "A")
;;                                                                   (:fn osa/separate-by-priority :args "B")
;;                                                                   (:fn osa/separate-by-priority :args "C")))
;;                                                  (org-agenda-span 'day)))))))
;;   (org-agenda nil "u"))

;; You can adjust the `super-filters' to create as many different sections as you like.

;;; Code:

;;;; Requirements

(require 'subr-x)
(require 'org)
(require 'cl-lib)
(require 's)

;;;; Filter macro and functions

(cl-defmacro osa/def-separator (name docstring &key section-name test)
  (declare (indent defun))
  (let ((function-name (intern (concat "osa/separate-by-" (symbol-name name)))))
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

(osa/def-separator time
  "Separate agenda items that have a time associated."
  :section-name "Schedule"  ; Note: this does not mean the item has a "SCHEDULED:" line
  :test (when-let ((time (org-find-text-property-in-string 'dotime item)))
          (not (eql (org-find-text-property-in-string 'dotime item) 'time))))

(osa/def-separator any-tag
  "Separate agenda ITEMS into two lists, putting items that contain any of TAGS into the second list.
  Returns list like (SECTION-NAME NON-MATCHING MATCHING)."
  :section-name (concat "Items tagged with: " (s-join " OR " args))
  :test (seq-intersection (osa/get-tags item) args))

(osa/def-separator habit
  "Separate habits into separate list.
  Returns (\"Habits\" NON-HABITS HABITS)."
  :section-name "Habits"
  :test (org-is-habit-p (org-find-text-property-in-string 'org-marker item)))

(osa/def-separator todo-keyword
  "Separate items by TODO-KEYWORD.
    Returns (SECTION-NAME NON-MATCHING MATCHING)."
  :section-name (concat (s-join " and " args) " items")
  :test (cl-member (org-find-text-property-in-string 'todo-state item) args :test 'string=))

(osa/def-separator priority
  "Separate items by PRIORITIES.
      PRIORITIES may be a string or a list of strings which match the
      letter in an Org priority cookie, e.g. \"A\", \"B\", etc.
      Returns (SECTION-NAME NON-MATCHING MATCHING)."
  :section-name (concat "Priority " (s-join " and " args) " items")
  :test (cl-member (osa/get-priority-cookie item) args :test 'string=))

;;;; Agenda command

(cl-defun org-super-agenda (&optional arg start-day span with-hour)
  "SUPER-FILTERS should be a list like (FILTER-FN ARG), e.g.:

  '(osa/separate-by-any-tag (\"bills\"))"
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
            ;; Diary
            (let ((org-agenda-search-headline-for-time t))
              (require 'diary-lib)
              (setq rtn (org-get-entries-from-diary date))
              (setq rtnall (append rtnall rtn))))

        (if (or rtnall org-agenda-show-all-dates)
            ;; Insert results
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

              ;; Actually insert results
              (when rtnall
                ;; Insert each filtered sublist
                (cl-loop with filter-fn
                         with args
                         for filter in super-filters
                         if (functionp filter) do (setq filter-fn filter
                                                        args nil
                                                        last nil)
                         else do (setq filter-fn (plist-get filter :fn)
                                       args (plist-get filter :args)
                                       last (plist-get filter :last))
                         for (section-name non-matching matching) = (funcall filter-fn rtnall args)

                         ;; FIXME: This repetition is kind of ugly, but I guess cl-loop is worth it...
                         if last collect (cons section-name matching) into last-sections
                         and do (setq rtnall non-matching)
                         else collect (cons section-name matching) into sections
                         and do (setq rtnall non-matching)

                         finally do (progn
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
                                                            "\n\n"))))))


              (put-text-property s (1- (point)) 'day d)
              (put-text-property s (1- (point)) 'org-day-cnt day-cnt))))

      (when (and org-agenda-clockreport-mode clocktable-start)
        ;; Clocktable
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

      ;; Window stuff
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

      ;; Add text properties to entire buffer
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

(defun osa/get-tags (s)
  "Return list of tags in agenda item string S."
  (org-find-text-property-in-string 'tags s))

(defun osa/insert-agenda-header (s)
  "Insert agenda header into current buffer containing string S and a newline."
  (insert (org-add-props s nil 'face 'org-agenda-structure) "\n"))

(defun osa/get-priority-cookie (item)
  "Return priority character for item."
  (when (string-match org-priority-regexp item)
    (match-string-no-properties 2 item)))
