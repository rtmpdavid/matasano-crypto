
(in-package :cl-user)
(defpackage matasano
  (:use :cl)
  (:export :fixed-xor
           :fixed-xor-bytes))
(in-package :matasano)

(defun fixed-xor (str1 str2)
  (hex-encode (fixed-xor-bytes (hex-decode str1) (hex-decode str2))))

(defun fixed-xor-bytes (bytes1 bytes2)
  (map '(simple-array unsigned-byte (*))
       #'logxor
       bytes1
       bytes2))
