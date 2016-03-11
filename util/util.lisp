(in-package :cl-user)
(defpackage matasano
  (:use :cl)
  (:export :char-nibble
           :hex-decode
           :hex-decode-stream
           :swap-nibbles))
(in-package :matasano)

(defun char-nibble (char)
  (let ((char-val (char-int (char-downcase char))))
    (if (<= char-val 57)
        (- char-val 48)
        (- char-val 87))))

(defun hex-decode (string)
  (with-input-from-string (stream string)
    (hex-decode-stream stream (/ (length string) 2))))

(defun hex-decode-stream (stream &optional (initial-n-elts 1))
  (let ((decoded (make-array initial-n-elts :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (loop
       for nibble-h = (read-char stream nil nil)
       while nibble-h
       for nibble-l = (read-char stream nil nil)
       do (vector-push-extend (logior (ash (char-nibble nibble-h) 4)
                                      (char-nibble nibble-l))
                              decoded)
       finally (return decoded))))

(defun nibble-char (nibble)
  (declare (type (integer 0 15) nibble))
  (code-char (if (> nibble 9) (+ nibble 87)
                 (+ nibble 48))))

(defun hex-encode (bytes)
  (let ((encoded (make-array (* (length bytes) 2)  :element-type 'character :adjustable nil :fill-pointer 0)))
    (loop for byte across bytes
       for nibble-h = (ldb (byte 4 4) byte)
       for nibble-l = (ldb (byte 4 0) byte)
       do (vector-push (nibble-char nibble-h) encoded)
       do (vector-push (nibble-char nibble-l) encoded))
    encoded))

(defun swap-nibbles (byte)
  (logior (ash (ldb (byte 4 0) byte) 4)
          (ldb (byte 4 4) byte)))

(defun read-hex (filename)
  (with-open-file (f filename)
    (hex-decode-stream f)))
