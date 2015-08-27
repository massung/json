;;;; JSON parser for Common Lisp
;;;;
;;;; Copyright (c) Jeffrey Massung
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

(defpackage :json
  (:use :cl :re :lexer :parse)
  (:export
   #:json-decode
   #:json-encode

   ;; parsed json objects
   #:json-object
   #:json-object-members

   ;; json object member accessor
   #:json-getf
   #:json-setf))

(in-package :json)

;;; ----------------------------------------------------

(defclass json-object ()
  ((members :initform nil
            :initarg :members
            :accessor json-object-members))
  (:documentation "An associative list of key/value pairs."))

;;; ----------------------------------------------------

(defmethod print-object ((obj json-object) stream)
  "Output a JSON object to a stream in readable form."
  (print-unreadable-object (obj stream :type t)
    (let ((*print-level* 1))
      (json-encode obj stream))))

;;; ----------------------------------------------------

(defun json-getf (object key &optional value)
  "Find an member's value in a JSON object."
  (let ((place (assoc key (json-object-members object) :test 'string=)))
    (if (null place)
        value
      (values (second place) t))))

;;; ----------------------------------------------------

(defun json-setf (object key value)
  "Assign a value to a key in a JSON object."
  (let ((place (assoc key (json-object-members object) :test 'string=)))
    (prog1 value
      (if (null place)
          (push (list key value) (json-object-members object))
        (rplacd place (list value))))))

;;; ----------------------------------------------------

(defsetf json-getf json-setf)

;;; ----------------------------------------------------

(define-lexer json-lexer (s)
  ("[%s%n]+" :next-token)
  ("{"       :object)
  ("}"       :end-object)
  ("%["      :array)
  ("%]"      :end-array)
  (":"       :key)
  (","       :comma)

  ;; strings use a different lexer
  ("\""      (push-lexer s #'string-lexer :string))

  ;; numeric constants
  ("[+-]?%d+(?%.%d+)?(?e[+-]?%d+)?"
   (values :constant (read-from-string $$)))

  ;; identifier constants
  ("%a%w*"   (cond ((string= $$ "true")  (values :constant t))
                   ((string= $$ "false") (values :constant nil))
                   ((string= $$ "null")  (values :constant nil))

                   ;; all other identifiers are invalid
                   (t (error "Unknown JSON identifier ~s" $$)))))

;;; ----------------------------------------------------

(define-lexer string-lexer (s)
  ("\""            (pop-lexer s :string))

  ;; escaped characters
  ("\\n"           (values :chars #\newline))
  ("\\t"           (values :chars #\tab))
  ("\\f"           (values :chars #\formfeed))
  ("\\b"           (values :chars #\backspace))
  ("\\r"           (values :chars #\return))

  ;; unicode characters
  ("\\u(%x%x%x%x)" (let ((n (parse-integer $1 :radix 16)))
                     (values :chars (code-char n))))

  ;; all other characters
  ("\\(.)"         (values :chars $1))
  ("[^\\\"]+"      (values :chars $$))

  ;; don't reach the end of file or line
  ("$"             (error "Unterminated string")))

;;; ----------------------------------------------------

(define-parser json-value
  "Parse a single JSON value."
  (.or  'json-constant
        'json-string
        'json-array
        'json-object))

;;; ----------------------------------------------------

(define-parser json-constant
  "Parse a literal JSON value."
  (.is :constant))

;;; ----------------------------------------------------

(define-parser json-string
  "Parse a quoted string."
  (.let (cs (.between (.is :string) (.is :string) (.many (.is :chars))))
    (.ret (format nil "~{~a~}" cs))))

;;; ----------------------------------------------------

(define-parser json-array
  "Parse a list of values."
  (.between (.is :array) (.is :end-array) 'json-values))

;;; ----------------------------------------------------

(define-parser json-values
  "Parse values separated by commas."
  (.sep-by 'json-value (.is :comma)))

;;; ----------------------------------------------------

(define-parser json-object
  "Parse a set of key/value pairs."
  (.let (ms (.between (.is :object) (.is :end-object) 'json-members))
    (.ret (make-instance 'json-object :members ms))))

;;; ----------------------------------------------------

(define-parser json-members
  "Key/value pairs separated by commas."
  (.sep-by 'json-kv-pair (.is :comma)))

;;; ----------------------------------------------------

(define-parser json-kv-pair
  "A single key value pair."
  (.let* ((k 'json-string)
          (v (.do (.is :key) 'json-value)))
    (.ret (list k v))))

;;; ----------------------------------------------------

(defun json-decode (string &optional source)
  "Convert a JSON string into a Lisp object."
  (with-lexer (lexer 'json-lexer string :source source)
    (with-token-reader (next-token lexer)
      (parse 'json-value next-token))))

;;; ----------------------------------------------------

(defun json-encode (value &optional stream)
  "Encodes a Lisp value into a stream."
  (json-write value stream))
