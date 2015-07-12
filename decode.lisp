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

(defmethod json-decode-object-into (class value)
  "Only success if value is a type of class."
  (prog1
      value
    (unless (typep value class)
      (warn "~s is not of type ~a" value class))))

;;; ----------------------------------------------------

(defmethod json-decode-object-into (class (value cl:list))
  "An array of values, decode them all."
  (loop for i in value collect (json-decode-object-into class i)))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql 'cl:pathname)) (value string))
  "A string should be decoded into a pathname."
  (pathname value))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql 'cl:keyword)) (value string))
  "Decode a JSON string into a keyword."
  (intern (string-upcase value) :keyword))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql 'cl:list)) (value json-object))
  "Return the members from a JSON object in an associative list."
  (loop for (k v) in (json-object-members value) collect (list k v)))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql 'cl:hash-table)) (value (eql nil)))
  "Return an empty hash table."
  (make-hash-table :test 'equal))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql 'cl:hash-table)) (value json-object))
  "Return the members of a JSON object in a hash table."
  (loop with ht = (make-hash-table :test 'equal)
                            
        ;; loop over each member in the object
        for (k v) in (json-object-members value)
        
        ;; add a k/v pair to the hash table
        do (setf (gethash k ht) v)
        
        ;; return the hash table
        finally (return ht)))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((object standard-object) (value json-object))
  "Decode the members of a JSON object into the slots of a CLOS object."
  (loop for slot in (class-slots (class-of object))
                                
        ;; use the name and type of the slot
        for slot-name = (slot-definition-name slot)
        for slot-type = (slot-definition-type slot)
                                
        ;; return the object when done decoding
        finally (return object)
        
        ;; decode into the slot from the member values
        do (let ((prop (assoc slot-name (json-object-members value) :test #'string=)))
             (if prop
                 (setf (slot-value object slot-name)
                       (json-decode-object-into slot-type (second prop)))
               (when (subtypep slot-type 'standard-object)
                 (setf (slot-value object slot-name)
                       (make-instance slot-type)))))))

;;; ----------------------------------------------------

(defmethod json-decode-object-into (class (value json-object))
  "Decode a JSON object into a new instance of class."
  (if (subtypep class 'standard-object)
      (json-decode-object-into (make-instance class) value)
    (json-decode-object-into class value)))

;;; ----------------------------------------------------

(defmethod json-decode-object-into ((class (eql t)) (value json-object))
  "Identity."
  (progn value))
