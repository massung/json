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

(in-package :json)

;;; ----------------------------------------------------

(defmethod json-write ((value (eql t)) &optional stream)
  "Encode the true value."
  (declare (ignore value))
  (format stream "~<true~>"))

;;; ----------------------------------------------------

(defmethod json-write ((value (eql nil)) &optional stream)
  "Encode the null constant."
  (declare (ignore value))
  (format stream "~<null~>"))

;;; ----------------------------------------------------

(defmethod json-write ((value symbol) &optional stream)
  "Encode a symbol to a stream."
  (json-write (symbol-name value) stream))

;;; ----------------------------------------------------

(defmethod json-write ((value number) &optional stream)
  "Encode a number to a stream."
  (format stream "~<~a~>" value))

;;; ----------------------------------------------------

(defmethod json-write ((value ratio) &optional stream)
  "Encode a ratio to a stream."
  (format stream "~<~a~>" (float value)))

;;; ----------------------------------------------------

(defmethod json-write ((value string) &optional stream)
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
    (format stream "~<\"~{~a~}\"~>" (map 'list #'encode-char value))))

;;; ----------------------------------------------------

(defmethod json-write ((value pathname) &optional stream)
  "Encode a pathname as a stream."
  (json-write (namestring value) stream))

;;; ----------------------------------------------------

(defmethod json-write ((value vector) &optional stream)
  "Encode an array to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream nil :prefix "[" :suffix "]")
      (when (plusp (length value))
        (json-write (aref value 0)))
      (loop
         for i from 1 below (length value)
         do (progn
              (write-char #\, stream)
              (pprint-newline :fill)
              (pprint-indent :block 0)
              (json-write (aref value i) stream))))))

;;; ----------------------------------------------------

(defmethod json-write ((value list) &optional stream)
  "Encode a list to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream value :prefix "[" :suffix "]")
      (pprint-exit-if-list-exhausted)
      (loop
         (json-write (pprint-pop) stream)
         (pprint-exit-if-list-exhausted)
         (write-char #\, stream)
         (pprint-newline :fill)
         (pprint-indent :block 0)))))

;;; ----------------------------------------------------

(defmethod json-write ((value hash-table) &optional stream)
  "Encode a hash-table to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (let ((keys (loop for key being each hash-keys in value collect key)))
      (pprint-logical-block (stream keys :prefix "{" :suffix "}")
        (pprint-exit-if-list-exhausted)
        (loop
           (let ((key (pprint-pop)))
             (if (not (stringp key))
                 (progn
                   (warn "~s is not a valid JSON key; skipping...~%" key)
                   (pprint-exit-if-list-exhausted))
               (progn
                 (json-write key stream)
                 (write-char #\: stream)
                 (json-write (gethash key value) stream)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\, stream)
                 (pprint-newline :mandatory)
                 (pprint-indent :current 0)))))))))

;;; ----------------------------------------------------

(defmethod json-write ((value json-object) &optional stream)
  "Encode a JSON object with an associative list of members to a stream."
  (let ((*print-pretty* t)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (stream (json-object-members value)
                                  :prefix "{"
                                  :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop
         (let ((kv-pair (pprint-pop)))
           (destructuring-bind (k v)
               kv-pair
             (if (not (stringp k))
                 (progn
                   (warn "~s is not a valid JSON key; skipping...~%" k)
                   (pprint-exit-if-list-exhausted))
               (progn
                 (json-write k stream)
                 (write-char #\: stream)
                 (json-write v stream)
                 (pprint-exit-if-list-exhausted)
                 (write-char #\, stream)
                 (pprint-newline :mandatory)
                 (pprint-indent :current 0)))))))))
