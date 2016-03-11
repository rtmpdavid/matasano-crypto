(in-package :cl-user)
(defpackage matasano
  (:use :cl :iter)
  (:export))
(in-package :matasano)

(defvar +input-string+ (map 'string #'code-char (read-hex "6-hex.txt")))
(defvar +input-string-hex+ ( "6-hex.txt"))

(defun calc-hamming-distance (string1 string2)
  (loop
     for a across string1
     for b across string2
     summing (logcount (logxor (char-code a)
                               (char-code b)))))


(defun guess-keysize (string)
  (iter (for keysize from 2 to (min (/ (length string) 2) 40))
        (format t "~a ~a~%" (floor (/ (calc-hamming-distance
                                       (subseq string 0 keysize)
                                       (subseq string keysize (* keysize 2)))
                                      keysize))
                keysize)
        (finding keysize minimizing (/ (calc-hamming-distance
                                        (subseq string 0 keysize)
                                        (subseq string keysize (* keysize 2)))
                                       keysize))))

(defun break-string (string count)
  (iter (for end from count to (length string) by count)
        (for begin previous end initially 0)
        (collecting (subseq string begin end))))


(defun transpose-string (strings)
  (apply #'map 'list
         #'(lambda (&rest chars)
             (coerce chars 'string))
         (car strings) (cdr strings)))

;;111
