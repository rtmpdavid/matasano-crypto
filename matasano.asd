(in-package :cl-user)
(defpackage matasano
  (:use :cl :asdf))
(in-package :matasano)

(defsystem matasano
    :version "0.0"
    :author ""
    :license ""
    :depends-on (:alexandria)
    :components ((:module "set-1"
                          :components
                          ((:file "1-hex-to-base64")
                           (:file "2-fixed-xor")
                           (:file "3-single-byte-xor-cypher")
                           (:file "4-detect-xor")
                           (:file "5-repeating-xor")
                           (:file "6-break-repeating-xor"))
                          :depends-on ("util"))
                 (:module "util"
                          :components
                          ((:file "util"))))
    :description "")
