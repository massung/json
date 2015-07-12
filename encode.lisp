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

(defmethod json-encode-into ((value (eql t)) &optional (*standard-output* *standard-output*))
  "Encode the true value."
  (declare (ignore value))
  (format t "~<true~>"))

;;; ----------------------------------------------------

(defmethod json-encode-into ((value (eql nil)) &optional (*standard-output* *standard-output*))
  "Encode the null constant."
  (declare (ignore value))
  (format t "~<null~>"))

;;; ----------------------------------------------------

(defmethod json-encode-into ((value symbol) &optional (*standard-output* *standard-output*))
  "Encode a symbol to a stream."
  (json-encode-into (symbol-name value)))

;;; ----------------------------------------------------

(defmethod json-encode-into ((value number) &optional (*standard-output* *standard-output*))
  "Encode a number to a stream."
  (format t "~<~a~>" value))

;;; ----------------------------------------------------

(defmethod json-encode-into ((value ratio) &optional (*standard-output* *standard-output*))
  "Encode a ratio to a stream."
  (format t "~<~a~>" (float value)))

;;; ----------------------------------------------------

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

;;; ----------------------------------------------------

(defmethod json-encode-into ((value pathname) &optional (*standard-output* *standard-output*))
  "Encode a pathname as a stream."
  (json-encode-into (namestring value) *standard-output*))

;;; ----------------------------------------------------

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
      (loop for i from 1 below (length value)
            do (progn
                 (write-char #\,)
                 (pprint-newline :fill)
                 (pprint-indent :block 0)
                 (json-encode-into (aref value i)))))))

;;; ----------------------------------------------------

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

;;; ----------------------------------------------------

(defmethod json-encode-into ((value hash-table) &optional (*standard-output* *standard-output*))
  "Encode a hash-table to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (let ((keys (loop for key being each hash-keys in value collect key)))
      (pprint-logical-block (*standard-output* keys :prefix "{" :suffix "}")
        (pprint-exit-if-list-exhausted)
        (loop (let ((key (pprint-pop)))
                (if (not (stringp key))
                    (progn
                      (warn "~s is not a valid JSON key; skipping...~%" key)
                      (pprint-exit-if-list-exhausted))
                  (progn
                    (json-encode-into key)
                    (write-char #\:)
                    (json-encode-into (gethash key value))
                    (pprint-exit-if-list-exhausted)
                    (write-char #\,)
                    (pprint-newline :mandatory)
                    (pprint-indent :current 0)))))))))

;;; ----------------------------------------------------

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
        (pprint-logical-block (*standard-output* bound-slots :prefix "{" :suffix "}")
          (pprint-exit-if-list-exhausted)
          (loop (let ((name (slot-definition-name (pprint-pop))))
                  (json-encode-into name)
                  (write-char #\:)
                  (json-encode-into (when (slot-boundp value name) (slot-value value name)))
                  (pprint-exit-if-list-exhausted)
                  (write-char #\,)
                  (pprint-newline :mandatory)
                  (pprint-indent :current 0))))))))

;;; ----------------------------------------------------

(defmethod json-encode-into ((value json-object) &optional (*standard-output* *standard-output*))
  "Encode a JSON object with an associative list of members to a stream."
  (let ((*print-pretty* t)
        (*print-level* nil)
        (*print-length* nil)
        (*print-lines* nil)
        (*print-right-margin* 72))
    (pprint-logical-block (*standard-output* (json-object-members value) :prefix "{" :suffix "}")
      (pprint-exit-if-list-exhausted)
      (loop (let ((kv-pair (pprint-pop)))
              (destructuring-bind (k v)
                  kv-pair
                (if (not (stringp k))
                    (progn
                      (warn "~s is not a valid JSON key; skipping...~%" k)
                      (pprint-exit-if-list-exhausted))
                  (progn
                    (json-encode-into k)
                    (write-char #\:)
                    (json-encode-into v)
                    (pprint-exit-if-list-exhausted)
                    (write-char #\,)
                    (pprint-newline :mandatory)
                    (pprint-indent :current 0)))))))))
