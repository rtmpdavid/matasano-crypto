(in-package :cl-user)
(defpackage matasano
  (:use :cl)
  (:export))
(in-package :matasano)

(defun detect-xor (filename)
  (with-open-file (f filename)
    (loop for i = (read-line f nil nil)
       while i
       for average-freq = (/ (reduce #'+ (score-hex i) :key #'cdr)
                             (length (score-hex i)))
       when  (>= average-freq 0.05)
         collect i)))
