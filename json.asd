(defpackage :json-asd
  (:use :cl :asdf))

(in-package :json-asd)

(defsystem :json
  :name "json"
  :version "1.0"
  :author "Jeffrey Massung"
  :license "Apache 2.0"
  :description "JSON encoding and decoding for LispWorks."
  :serial t
  :components ((:file "json")
               (:file "encode")
               (:file "decode"))
  :depends-on ("lexer"))
