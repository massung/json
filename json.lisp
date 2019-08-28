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
  (:use :cl)
  (:export
   #:json-decode
   #:json-encode

   ;; decode from a stream
   #:json-read

   ;; enable json-object-reader macro
   #:json-enable-reader-macro

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
          (let ((k (if (stringp key)
                       key
                     (princ-to-string key))))
            (push (list k value) (json-object-members object)))
        (rplacd place (list value))))))

;;; ----------------------------------------------------

(defsetf json-getf json-setf)

;;; ----------------------------------------------------

(defun json-decode (string &key (start 0) end)
  "Convert a JSON string into a Lisp object."
  (with-input-from-string (stream string :start start :end end)
    (values (json-read stream)
            (file-position stream))))

;;; ----------------------------------------------------

(defun json-encode (value &optional stream)
  "Encodes a Lisp value into a stream."
  (json-write value stream))

;;; ----------------------------------------------------

(defun json-enable-reader-macro ()
  "Set the #{ dispatch macro character for reading JSON objects."
  (flet ((json-object-reader (stream char n)
           (declare (ignorable char n))
           (let ((xs (read-delimited-list #\} stream t)))
             (loop
                for key = (pop xs)
                for value = (pop xs)

                ;; stop when nothing is left
                unless (or xs key value)
                return (make-instance 'json-object :members pairs)

                ;; build associative list of key/value pairs
                collect (list (princ-to-string key) value)
                into pairs))))
    (set-dispatch-macro-character #\# #\{ #'json-object-reader)
    (set-macro-character #\} (get-macro-character #\) nil))))
