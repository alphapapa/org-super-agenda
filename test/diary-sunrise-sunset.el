(require 'cl-lib)
(require 'solar)
(require 'cal-dst)

(defun org-super-agenda--test-diary-sunrise-sunset-split ()
  "Split `diary-sunrise-sunset' into sunrise, sunset, and daylight hours."
  (let* ((calendar-latitude 0)
         (calendar-longitude 0)
         (calendar-time-zone (if (dst-in-effect (date-to-day org-super-agenda--test-date))
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

(defun org-super-agenda--test-diary-sunrise ()
  (let ((s (org-super-agenda--test-diary-sunrise-sunset-split)))
    (format "%s (%s)" (first s) (third s))))

(defun org-super-agenda--test-diary-sunset ()
  (cl-second (org-super-agenda--test-diary-sunrise-sunset-split)))
