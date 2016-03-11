(in-package :cl-user)
(defpackage matasano
  (:use :cl)
  (:export))
(in-package :matasano)

(defun repeating-key-xor (string key)
  (hex-encode (coerce (loop for char across string
                         with key-len = (length key)
                         for i from 0
                         collect (logxor (char-code (aref key (mod i key-len)))
                                         (char-code char)))
                      'vector)))
