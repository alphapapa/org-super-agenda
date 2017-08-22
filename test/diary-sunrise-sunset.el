(require 'cl-lib)
(require 'solar)

(defun org-super-agenda--test-solar-setup ()
  "Set ‘calendar-longitude’, ‘calendar-latitude’, ‘calendar-time-zone’, using GeoClue and `current-time-zone'."
  (dolist (var '(calendar-latitude calendar-longitude))
    (unless (symbol-value var)
      (setq var 0)))
  (unless calendar-time-zone
    (setq calendar-time-zone 0)))

(org-super-agenda--test-solar-setup)

(defun org-super-agenda--test-diary-sunrise-sunset-split ()
  "Split `diary-sunrise-sunset' into sunrise, sunset, and daylight hours."
  (let* ((string (diary-sunrise-sunset))
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
  (let ((s (diary-sunrise-sunset-split)))
    (format "%s (%s)" (first s) (third s))))

(defun org-super-agenda--test-diary-sunset ()
  (cl-second (diary-sunrise-sunset-split)))
