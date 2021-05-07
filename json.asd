(defpackage :json-asd
  (:use :cl :asdf))

(in-package :json-asd)

(defsystem :json
  :name "json"
  :version "1.1"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "JSON encoding and decoding for Common Lisp."
  :serial t
  :components ((:file "json")
               (:file "decode")
               (:file "encode")))
