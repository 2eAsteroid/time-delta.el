(defun time-delta (time-one time-two)
  "Calculates the time delta between two times.
Takes in two times t-1 and t-2, both of format HH:MM:SS, and with the
assumption that t-1 <= t-2."
  (let* ((time-one-list (reverse (split-string time-one (rx ":"))))
         (time-two-list (reverse (split-string time-two (rx ":"))))
         (delta-list nil)
         (carry nil))
    (dotimes (index 3)
      (let ((difference (- (string-to-number (nth index time-two-list)) (string-to-number (nth index time-one-list)))))
        (if (and (not (null carry)) (not (< difference 0)))
                (setq difference (1- difference)))
        (if (< difference 0)
            (progn (setq difference (+ 60 difference))
                   (setq carry t)))
        (setq delta-list (cons difference delta-list))))
    (format "%s:%s:%s "
            (car delta-list)
            (cadr delta-list)
            (caddr delta-list))))
