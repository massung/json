;;;; JSON parser for LispWorks
;;;;
;;;; Copyright (c) 2012 by Jeffrey Massung
;;;;
;;;; This file is provided to you under the Apache License,
;;;; Version 2.0 (the "License"); you may not use this file
;;;; except in compliance with the License.  You may obtain
;;;; a copy of the License at
;;;;
;;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;;
;;;; Unless required by applicable law or agreed to in writing,
;;;; software distributed under the License is distributed on an
;;;; "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
;;;; KIND, either express or implied.  See the License for the
;;;; specific language governing permissions and limitations
;;;; under the License.
;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "parsergen"))

(defpackage :json
  (:use :cl :hcl :parsergen :lexer)
  (:export
   #:json-decode
   #:json-decode-file))

(in-package :json)

(deflexer json-lexer (:multi-line t)
  ("[%s%n]+"                          :next-token)
  ("{"                                :object)
  ("}"                                :end-object)
  ("%["                               :array)
  ("%]"                               :end-array)
  (":"                                :colon)
  (","                                :comma)

  ;; strings use a different lexer
  ("\""                               (prog1
                                          :quote
                                        (setf *lexer* #'string-lexer)))

  ;; numbers
  ("[+-]?%d+%.%d+(?[eE][+-]?%d+)?"    (values :float (parse-float $$)))
  ("[+-]?%d+(?[eE][+-]?%d+)?"         (values :int (truncate (parse-float $$))))

  ;; identifiers
  ("%a%w*"                            (cond
                                       ((string= $$ "true") (values :const :true))
                                       ((string= $$ "false") (values :const :false))
                                       ((string= $$ "null") (values :const :null))
                                       (t
                                        (error "Unknown identifier ~s" $$)))))

(deflexer string-lexer (:multi-line t)
  ("\""                               (prog1
                                          :quote
                                        (setf *lexer* #'json-lexer)))

  ;; escaped characters
  ("\\n"                              (values :char #\newline))
  ("\\t"                              (values :char #\tab))
  ("\\f"                              (values :char #\formfeed))
  ("\\b"                              (values :char #\backspace))
  ("\\r"                              (values :char #\return))

  ;; unicode characters
  ("\\u(%x%x%x%x)"                    (let ((n (parse-integer $1 :radix 16)))
                                        (values :char (code-char n))))

  ;; all other characters
  ("\\."                              (values :char (char $$ 1)))
  ("."                                (values :char (char $$ 0)))

  ;; don't reach the end of file or line
  ("$"                                (error "Unterminated string")))

(defparser json-parser
  ((start value) $1)
  
  ;; single json value
  ((value :const) $1)
  ((value :float) $1)
  ((value :int) $1)
  ((value string) $1)
  ((value array) $1)
  ((value object) $1)

  ;; unparsable values
  ((value :error)
   (error "JSON syntax error"))

  ;; quoted string
  ((string :quote chars)
   (coerce $2 'lw:text-string))

  ;; string characters
  ((chars :char chars)
   `(,$1 ,@$2))
  ((chars :quote)
   `())
 
  ;; objects
  ((object :object :end-object) ())
  ((object :object members) $2)

  ;; members of an object
  ((members string :colon value :comma members)
   `((,$1 ,$3) ,@$5))
  ((members string :colon value :end-object)
   `((,$1 ,$3)))
  
  ;; arrays
  ((array :array :end-array) ())
  ((array :array elements) $2)

  ;; elements of an array
  ((elements value :comma elements)
   `(,$1 ,@$3))
  ((elements value :end-array)
   `(,$1)))

(defun json-decode (string &optional source)
  "Convert a JSON string into a Lisp object."
  (with-lexbuf (string source)
    (parse #'json-parser #'json-lexer)))

(defun json-decode-file (pathname)
  "Load a JSON source file and decode it."
  (with-open-file (f pathname)
    (let ((s (make-array (file-length f) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer s) (read-sequence s f))
      (json-decode s pathname))))