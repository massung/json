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
   #:json-decode))

(in-package :json)

(deflexer string-lexer ()
  ("\""             (values nil t))
  ("\\n"            (values :char #\newline))
  ("\\t"            (values :char #\tab))
  ("\\f"            (values :char #\formfeed))
  ("\\b"            (values :char #\backspace))
  ("\\r"            (values :char #\return))
  ("\\u(%x%x%x%x)"  (values :char (code-char (parse-integer $1 :radix 16))))
  ("\\."            (values :char (char $$ 1)))
  ("."              (values :char (char $$ 0))))

(defun parse-json-string ()
  "Join a list of characters together to create a string."
  (map 'lw:text-string #'token-value (tokenize #'string-lexer)))

(deflexer json-lexer (:multi-line t)
  ("[%s%n]+")
  ("{"                                :object)
  ("}"                                :end-object)
  ("%["                               :array)
  ("%]"                               :end-array)
  (":"                                :colon)
  (","                                :comma)
  ("\""                               (values :string (parse-json-string)))
  ("[+-]?%d+%.%d+(?[eE][+-]?%d+)?"    (values :float (parse-float $$)))
  ("[+-]?%d+(?[eE][+-]?%d+)?"         (values :int (truncate (parse-float $$))))
  ("%a%w*"                            (cond
                                       ((string= $$ "true") (values :const :true))
                                       ((string= $$ "false") (values :const :false))
                                       ((string= $$ "null") (values :const :null))
                                       (t :unknown-identifier))))

(defparser json-parser
  ((start value) $1)
  
  ;; single json value
  ((value :const) (token-value $1))
  ((value :string) (token-value $1))
  ((value :float) (token-value $1))
  ((value :int) (token-value $1))
  ((value array) (coerce $1 'vector))
  ((value object) $1)

  ;; unparsable values
  ((value :unknown-identifier)
   (error "Unknown JSON identifier"))
  ((value :error)
   (error "JSON syntax error"))
 
  ;; objects
  ((object :object :end-object) ())
  ((object :object members) $2)

  ;; members of an object
  ((members :string :colon value :comma members)
   `((,(token-value $1) . ,$3) ,@$5))
  ((members :string :colon value :end-object)
   `((,(token-value $1) . ,$3)))
  
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
    (json-parser #'json-lexer)))
