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
  (:use :cl :lw :hcl :parsergen :re :lexer)
  (:export
   #:*json-null*

   ;; decoding functions
   #:json-decode
   #:json-decode-into

   ;; encoding functions
   #:json-encode))

(in-package :json)

(defvar *json-null* :null
  "The value used to decode JSON null to.")
(defvar *json-indent* 0
  "The indentation level when encoding JSON.")

(deflexer json-lexer
  ("[%s%n]+"                  :next-token)
  ("{"                        :object)
  ("}"                        :end-object)
  ("%["                       :array)
  ("%]"                       :end-array)
  (":"                        :colon)
  (","                        :comma)

  ;; strings use a different lexer
  ("\""                       (push-lexer #'string-lexer :string))

  ;; number constants
  ("[+%-]?%d+%.%d+e[+%-]?%d+" (values :float (parse-float $$)))
  ("[+%-]?%d+%.%d+"           (values :float (parse-float $$)))
  ("[+%-]?%d+e[+%-]?%d+"      (values :int (truncate (parse-float $$))))
  ("[+%-]?%d+"                (values :int (parse-integer $$)))

  ;; identifier constants
  ("%a%w*"                    (cond
                               ((string= $$ "true") (values :bool t))
                               ((string= $$ "false") (values :bool nil))
                               ((string= $$ "null") (values :null))
                               (t
                                (error "Unknown identifier ~s" $$)))))

(deflexer string-lexer
  ("\""                       (pop-lexer :end-string))

  ;; escaped characters
  ("\\n"                      (values :chars #\newline))
  ("\\t"                      (values :chars #\tab))
  ("\\f"                      (values :chars #\formfeed))
  ("\\b"                      (values :chars #\backspace))
  ("\\r"                      (values :chars #\return))

  ;; unicode characters
  ("\\u(%x%x%x%x)"            (let ((n (parse-integer $1 :radix 16)))
                                (values :chars (code-char n))))

  ;; all other characters
  ("\\(.)"                    (values :chars $1))
  ("[^\\\"]+"                 (values :chars $$))

  ;; don't reach the end of file or line
  ("$"                        (error "Unterminated string")))

(defparser json-parser
  ((start value) $1)

  ;; numerics, strings, arrays, and objects
  ((value :null) *json-null*)
  ((value :bool) $1)
  ((value :float) $1)
  ((value :int) $1)
  ((value string) $1)
  ((value array) $1)
  ((value object) $1)

  ;; unparsable values
  ((value :error)
   (error "JSON syntax error"))

  ;; quoted string
  ((string :string chars)
   (with-output-to-string (s nil :element-type 'character)
     (loop :for c :in $2 :do (princ c s))))

  ;; character sequences
  ((chars :chars chars)
   `(,$1 ,@$2))
  ((chars :end-string)
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
  ((array :array elements)
   (coerce $2 'vector))
  ((array :array :end-array)
   #())

  ;; elements of an array
  ((elements value :comma elements)
   `(,$1 ,@$3))
  ((elements value :end-array)
   `(,$1)))

(defparser string-parser
  ((start string) $1)

  ;; convert a list of characters to a string
  ((string chars)
   (coerce $1 'lw:text-string))

  ;; collect a list of characters
  ((chars :char chars)
   `(,$1 ,@$2))
  ((chars)
   `()))

(defun json-decode (string &optional source)
  "Convert a JSON string into a Lisp object."
  (let ((tokens (tokenize #'json-lexer string source)))
    (parse #'json-parser tokens)))

(defun json-decode-into (class string &optional source)
  "Create an instance of class and assoc all slots."
  (labels ((decode-into (class json)
             (if (vectorp json)
                 (map 'vector #'(lambda (i) (decode-into class i)) json)
               (let ((p (funcall (if (subtypep class 'condition) #'make-condition #'make-instance) class)))
                 (prog1
                     p
                   (loop :for slot :in (class-slots (class-of p))
                         :for slot-name := (slot-definition-name slot)
                         :for slot-type := (slot-definition-type slot)
                         :do (let ((value (assoc slot-name json :test #'string=)))
                               (when value
                                 (setf (slot-value p slot-name)
                                       (if (subtypep slot-type 'standard-object)
                                           (decode-into slot-type (second value))
                                         (second value)))))))))))
    (let ((json (json-decode string source)))
      (when json
        (decode-into class json)))))

(defun json-indent (stream &optional object colon-p at-sign-p)
  "Indents with whitespace."
  (declare (ignore object colon-p at-sign-p))
  (terpri stream)
  (loop :for i :below (* *json-indent* 2) :do (princ #\space stream)))

(defmethod json-encode ((value symbol))
  "Encode the T constant as JSON true."
  (cond
   ((eq value *json-null*) "null")
   ((eq value nil)         "false")
   ((eq value t)           "true")
   (t
    (json-encode (symbol-name value)))))

(defmethod json-encode ((value number))
  "Encode a number as a JSON value."
  (write-to-string value))

(defmethod json-encode ((value ratio))
  "Encode a ratio as a JSON value."
  (write-to-string (float value)))

(defmethod json-encode ((value string))
  "Encode a string to a string."
  (flet ((encode-char (c)
           (cond
            ((char= c #\\) "\\\\")
            ((char= c #\") "\\\"")
            ((char= c #\newline) "\\n")
            ((char= c #\tab) "\\t")
            ((char= c #\formfeed) "\\f")
            ((char= c #\backspace) "\\b")
            ((char= c #\return) "\\r")
            ((char> c #\~)
             (format nil "\\u~16,4,'0r" (char-code c)))
            (t
             (string c)))))
    (format nil "\"~{~a~}\"" (map 'list #'encode-char value))))

(defmethod json-encode ((value sequence))
  "Encode an array to a string."
  (if (zerop (length value))
      "[ ]"
    (flet ((encode (item)
             (let ((*json-indent* (1+ *json-indent*)))
               (json-encode item))))
      (let ((*json-indent* (1+ *json-indent*)))
        (let ((items (map 'list #'encode value)))
          (format nil "~/json::json-indent/~:*[ ~{~a~/json::json-indent/~^~:*~^, ~}]" items))))))

(defmethod json-encode ((value standard-object))
  "Encode any class with slots to a string."
  (let ((slots (class-slots (class-of value))))
    (flet ((encode-slot (slot)
             (let ((*json-indent* (1+ *json-indent*)))
               (let ((name (slot-definition-name slot)))
                 (list (json-encode name) (json-encode (slot-value value name)))))))
      (format nil "{ ~{~a:~a~/json::json-indent/~^~:*~^, ~}}" (mapcan #'encode-slot slots)))))