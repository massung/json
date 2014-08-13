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
   #:json-object-members

   ;; decoding and encoding functions
   #:json-decode
   #:json-decode-into
   #:json-encode
   #:json-encode-into))

(in-package :json)

(defclass json-object ()
  ((members :initform nil :initarg :members :accessor json-object-members))
  (:documentation "A differentiation between a list and an object."))

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
                               ((string= $$ "true") :true)
                               ((string= $$ "false") :false)
                               ((string= $$ "null") :null)
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
  ((value :true) t)
  ((value :false) nil)
  ((value :null) nil)
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
  ((object :object :end-object)
   (make-instance 'json-object))
  ((object :object members) 
   (make-instance 'json-object :members $2))

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
  (labels ((decode-into (class value)
             (cond
              ((listp value)
               (mapcar #'(lambda (i) (decode-into class i)) value))
              ((subtypep class 'keyword)
               (intern (string-upcase value) :keyword))
              ((null value)
               ())
              ((typep value 'json-object)
               (cond ((subtypep class 'cl:list)
                      (loop :for (k v) :in (json-object-members value)
                            :collect (if (typep v 'json-object)
                                         (list k (decode-into 'cl:list v))
                                       (list k v))))

                     ;; construct a hash table from the object members
                     ((subtypep class 'cl:hash-table)
                      (loop :with ht := (make-hash-table :test 'equal)
                            
                            ;; loop over each member in the object
                            :for (k v) :in (json-object-members value)
                            
                            ;; add a k/v pair to the hash table
                            :do (setf (gethash k ht) (if (typep v 'json-object)
                                                         (decode-into 'cl:hash-table v)
                                                       v))
                            
                            ;; return the hash table
                            :finally (return ht)))
                     
                     ;; the class is a CLOS class that needs decoded into
                     (t (let ((object (make-instance class)))
                          (loop :for slot :in (class-slots (class-of object))
                                
                                ;; use the name and type of the slot
                                :for slot-name := (slot-definition-name slot)
                                :for slot-type := (slot-definition-type slot)
                                
                                ;; return the object when done decoding
                                :finally (return object)
                                
                                ;; decode into the slot from the member values
                                :do (let ((prop (assoc slot-name (json-object-members value) :test #'string=)))
                                      (if prop
                                          (setf (slot-value object slot-name)
                                                (if (or (subtypep slot-type 'standard-object)
                                                        (subtypep slot-type 'keyword)
                                                        (subtypep slot-type 'hash-table)
                                                        (subtypep slot-type 'list))
                                                    (decode-into slot-type (second prop))
                                                  (second prop)))
                                        (when (subtypep slot-type 'standard-object)
                                          (setf (slot-value object slot-name)
                                                (make-instance slot-type))))))))))

              ;; type error (e.g. decoding "true" into a hash table)
              (t (error "~a is not of type ~a" value class)))))
    (when-let (json (json-decode string source))
      (decode-into class json))))

(defun json-encode (value)
  "Encodes a Lisp value into a string."
  (with-output-to-string (s)
    (json-encode-into value s)))

(defmethod json-encode-into ((value (eql t)) &optional (*standard-output* *standard-output*))
  "Encode the true value."
  (declare (ignore value))
  (format t "~<true~>"))

(defmethod json-encode-into ((value (eql nil)) &optional (*standard-output* *standard-output*))
  "Encode the null constant."
  (declare (ignore value))
  (format t "~<null~>"))

(defmethod json-encode-into ((value symbol) &optional (*standard-output* *standard-output*))
  "Encode a symbol to a stream."
  (json-encode-into (symbol-name value)))

(defmethod json-encode-into ((value number) &optional (*standard-output* *standard-output*))
  "Encode a number to a stream."
  (format t "~<~a~>" value))

(defmethod json-encode-into ((value ratio) &optional (*standard-output* *standard-output*))
  "Encode a ratio to a stream."
  (format t "~<~a~>" (float value)))

(defmethod json-encode-into ((value string) &optional (*standard-output* *standard-output*))
  "Encode a string as a stream."
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
    (format t "~<\"~{~a~}\"~>" (map 'list #'encode-char value))))

(defmethod json-encode-into ((value pathname) &optional (*standard-output* *standard-output*))
  "Encode a pathname as a stream."
  (json-encode-into (namestring value) *standard-output*))

(defmethod json-encode-into ((value vector) &optional (*standard-output* *standard-output*))
  "Encode an array to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (*standard-output* nil :prefix "[" :suffix "]")
      (when (plusp (length value))
        (json-encode-into (aref value 0)))
      (loop :for i :from 1 :below (length value)
            :do (progn
                  (write-char #\,)
                  (pprint-newline :fill)
                  (pprint-indent :block 0)
                  (json-encode-into (aref value i)))))))

(defmethod json-encode-into ((value list) &optional (*standard-output* *standard-output*))
  "Encode a list to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (*standard-output* value :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop (progn
              (json-encode-into (pprint-pop))
              (pprint-exit-if-list-exhausted)
              (write-char #\,)
              (pprint-newline :fill)
              (pprint-indent :block 0))))))

(defmethod json-encode-into ((value hash-table) &optional (*standard-output* *standard-output*))
  "Encode a hash-table to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (let ((keys (loop :for key :being :each hash-keys :in value :collect key)))
      (pprint-logical-block (*standard-output* keys :prefix "{" :suffix "}")
        (pprint-exit-if-list-exhausted)
        (loop (let ((key (pprint-pop)))
                (json-encode-into (string key))
                (write-char #\:)
                (json-encode-into (gethash key value))
                (pprint-exit-if-list-exhausted)
                (write-char #\,)
                (pprint-newline :mandatory)
                (pprint-indent :current 0)))))))

(defmethod json-encode-into ((value standard-object) &optional (*standard-output* *standard-output*))
  "Encode any class with slots to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (flet ((bound (slot)
             (slot-boundp value (slot-definition-name slot))))
      (let ((bound-slots (remove-if-not #'bound (class-slots (class-of value)))))
        (pprint-logical-block (*standard-output* bound-slots  :prefix "{" :suffix "}")
          (pprint-exit-if-list-exhausted)
          (loop (let ((name (slot-definition-name (pprint-pop))))
                  (json-encode-into name)
                  (write-char #\:)
                  (json-encode-into (when (slot-boundp value name) (slot-value value name)))
                  (pprint-exit-if-list-exhausted)
                  (write-char #\,)
                  (pprint-newline :mandatory)
                  (pprint-indent :current 0))))))))
