;; https://github.com/mpereira/.emacs.d/blob/739db8e3dbab01b5e50f7e31c2200ca6540b639e/configuration.org#change-font-m-x-x-select-font

(let ((org-super-agenda-groups
       '((:name "Done today"
                :closed today))))
  (org-ql-agenda "~/org/main.org"
    (done)))

(let ((org-super-agenda-groups
       '((:name "Done in September"
                :closed (after "september 1 this year"))
         (:name "Done this week"
                :closed (after "sunday"))
         (:name "Done last week"
                :and (:closed (after "last sunday")
                              :closed (before "this sunday"))))))
  (org-ql-agenda "~/org/main.org"
    (done)))

(let ((org-super-agenda-groups
       '((:name "Done in September"
                :closed (between "september 1" "september 30"))
         (:name "Done this week"
                :closed (between "sunday" "saturday"))
         (:name "Done last week"
                :closed (between "last sunday" "last saturday"))
         )))
  (org-ql-agenda "~/org/main.org"
    (closed)))

(let ((org-super-agenda-groups
       '((:name "Due this week"
                :deadline (between "sunday" "saturday"))
         (:name "Due last week"
                :deadline (between "last sunday" "last saturday"))
         (:name "Due next week"
                :deadline (between "next sunday" "next saturday"))
         (:name "Due December"
                :deadline (between "Dec 1" "Dec 30"))
         )))
  (org-ql-agenda "~/org/main.org"
    (deadline)
    :sort (date)))

(org-super-agenda--test-with-org-today-date "2017-07-05 00:00"
  (let ((org-agenda-files (list "~/src/emacs/org-super-agenda/test/test.org"))
        (org-agenda-span 'day)
        (org-super-agenda-groups
         '((:name "Due this week"
                  ;; NOTE: GNU date doesn't support relative dates from absolute ones like this.
                  :deadline (between "this sunday july 5 2017" "this saturday july 5 2017"))
           (:name "Due last week"
                  :deadline (between "last sunday" "last saturday"))
           (:name "Due next week"
                  :deadline (between "next sunday" "next saturday"))
           (:name "Due July"
                  :deadline (between "July 1 2017" "July 31 2017"))
           )))
    (org-ql-agenda (org-agenda-files)
      (deadline)
      :sort (date))))

(cl-defun org-super-agenda--gnu-date-to-timestamp (s &key (zero-hour t))
  "Return Unix timestamp for date S, converted with GNU \"date\"."
  (shell-command-to-string (format "date --rfc-3339=seconds --date \"%s\""
                                   (if zero-hour
                                       (concat s " 00:00")
                                     s))))


(org-time-string-to-absolute (org-super-agenda--gnu-date-to-timestamp "yesterday"))

(let ((org-super-agenda-groups
       '((:name "Done last week"
                :closed (between "last sunday" "last saturday"))
         (:name "Done this week"
                :closed (between "this sunday" "this saturday")))))
  (org-ql-agenda (org-agenda-files)
    (closed)
    :sort (date)))
