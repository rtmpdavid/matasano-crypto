(in-package :cl-user)
(defpackage matasano
  (:use :cl)
  (:export))
(in-package :matasano)

(defparameter *sample-filename* "catcher.txt")
(defparameter *sample-scores* (with-open-file (s sample-filename)
                                (score-stream s)))

(defun make-score ()
  (loop for c from 0 to 255
     collect (cons (code-char c) 0)))

(defun score-hex (hex)
  (score-bytes (hex-decode hex)))

(defun score-bytes (bytes)
  (with-input-from-string (s (map 'string
                                  #'code-char
                                  bytes))
    (score-stream s)))


(defun score-string (string)
  (with-input-from-string (s string)
    (score-stream s)))

(defun score-stream (stream)
  (let ((str-len 0))
    (loop
       for char = (read-char stream nil nil)
       with char-table = (make-score)
       while char
       do (incf str-len)
       do (incf (cdr (elt char-table (char-code char))))
       finally (return (remove-if #'zerop
                                  (sort (map 'list (lambda (a) (cons (car a)
                                                                     (float (/ (cdr a) str-len))))
                                             char-table)
                                        #'> :key #'cdr)
                                  :key #'cdr)))))

(defun match (score1 score2)
  (let ((results (loop for i in score1
                    for matches = (remove-if-not #'(lambda (a) (<= (abs (- (cdr i) a)) 0.02)) score2 :key #'cdr)
                    if matches
                    collect (map 'list #'(lambda (a)
                                           (logxor (char-code (car i))
                                                   (char-code (car a))))
                                 matches))))
    (if results (subseq results
                        0 (min (1- (length results)) 255))
        nil)))


(defun count-matches (matches)
  (loop for i in matches
     with table = (make-hash-table)
     do (mapcar #'(lambda (a)
                    (incf (gethash a table 0)))
                i)
     finally (return (sort (alexandria:hash-table-alist table) #'> :key #'cdr))))

(defun single-byte-xor (string byte)
  (map 'string
       #'code-char
       (single-byte-xor-bytes (hex-decode string) byte)))

(defun single-byte-xor-string (string byte)
  (map 'string
       #'(lambda (a)
           (code-char (logxor (char-code a)
                              byte)))
       string))

(defun single-byte-xor-bytes (bytes byte)
  (map 'vector
       #'(lambda (a)
           (logxor a byte))
       bytes))

(defun unxor-hex (hex &optional (variants-to-show 1))
  (let ((match-scores (count-matches (match (score-hex hex) *sample-scores*))))
    (mapcar #'(lambda (a)
                (single-byte-xor hex (car a)))
            (subseq match-scores 0 (min (1- (length match-scores)) variants-to-show)))))

(defun unxor (string &optional (variants-to-show 2))
  (let ((match-scores (count-matches (match (score-string string) *sample-scores*))))
    (mapcar #'(lambda (a)
                (print (car a))
                (single-byte-xor (hex-encode (map 'vector #'char-code string)) (car a)))
            (subseq match-scores 0 (min (1- (length match-scores)) variants-to-show)))))
