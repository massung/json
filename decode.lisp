;;;; JSON parser for Common Lisp
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

(defun json-read (stream &optional (eof-error-p t) eof-value)
  "Read a JSON object from a stream."
  (let ((c (peek-char t stream eof-error-p :eof)))
    (case c
      (:eof eof-value)

      ;; constants, objects, lists, and strings
      (#\t (json-read-true stream))
      (#\f (json-read-false stream))
      (#\n (json-read-null stream))
      (#\{ (json-read-object stream))
      (#\[ (json-read-list stream))
      (#\" (json-read-string stream))

      ;; must be a number
      (otherwise (json-read-number stream)))))

;;; ----------------------------------------------------

(defun json-read-char (stream expected &key skip-ws)
  "Read the next, expected character in the stream."
  (declare (optimize (speed 3) (debug 0)))
  (if (json-peek-char stream expected :skip-ws skip-ws)
      t
    (error "JSON error: unexpected ~s" (read-char stream))))

;;; ----------------------------------------------------

(defun json-peek-char (stream expected &key skip-ws)
  "Peek at the next character or token and optionally error if unexpected."
  (declare (optimize (speed 3) (debug 0)))
  (when (equal (peek-char skip-ws stream) expected)
    (read-char stream)))

;;; ----------------------------------------------------

(defun json-read-true (stream)
  "Read true from a JSON stream."
  (json-read-char stream #\t :skip-ws t)
  (json-read-char stream #\r)
  (json-read-char stream #\u)
  (json-read-char stream #\e))

;;; ----------------------------------------------------

(defun json-read-false (stream)
  "Read false from a JSON stream."
  (prog1 nil
    (json-read-char stream #\f :skip-ws t)
    (json-read-char stream #\a)
    (json-read-char stream #\l)
    (json-read-char stream #\s)
    (json-read-char stream #\e)))

;;; ----------------------------------------------------

(defun json-read-null (stream)
  "Read null from a JSON stream."
  (prog1 nil
    (json-read-char stream #\n :skip-ws t)
    (json-read-char stream #\u)
    (json-read-char stream #\l)
    (json-read-char stream #\l)))

;;; ----------------------------------------------------

(defun json-read-number (stream)
  "Read a number from a JSON stream."
  (declare (optimize (speed 3) (debug 0)))
  (let ((s (with-output-to-string (s)
             (when (equal (peek-char t stream) #\-)
               (write-char (read-char stream) s))

             ;; read base-10 digits, fraction, and exponent
             (labels ((read-digits ()
                        (let ((c (read-char stream)))
                          (unless (digit-char-p c)
                            (error "JSON error: unexpected ~s" c))

                          ;; write the digits
                          (loop
                             (write-char c s)

                             ;; next digit, test for eof
                             (unless (setf c (read-char stream nil))
                               (return))

                             ;; ensure digit
                             (unless (digit-char-p c)
                               (return (unread-char c stream))))))

                      ;; fractional component
                      (read-fraction ()
                        (when (equal (peek-char nil stream nil) #\.)
                          (write-char (read-char stream) s)
                          (read-digits)))

                      ;; exponent
                      (read-exponent ()
                        (when (equalp (peek-char nil stream nil) #\e)
                          (write-char (read-char stream) s)

                          ;; optional sign
                          (case (peek-char nil stream)
                            (#\- (write-char (read-char stream) s))
                            (#\+ (write-char (read-char stream) s)))

                          ;; exponent
                          (read-digits))))

               ;; read each component; numbers beginning with 0 are a special case
               (if (equalp (peek-char nil stream) #\0)
                   (write-char (read-char stream) s)
                 (read-digits))
               (read-fraction)
               (read-exponent)))))
    (prog1
      (read-from-string s))))

;;; ----------------------------------------------------

(defun json-read-string (stream)
  "Read a string from a JSON stream."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected quote
  (json-read-char stream #\" :skip-ws t)

  ;; read into an output buffer
  (with-output-to-string (s)
    (loop
       for c = (read-char stream)

       ;; stop at closing quote
       until (char= c #\")

       ;; write character to output
       do (if (char/= c #\\)
              (write-char c s)
            (let ((c (case (read-char stream)
                       (#\n #\newline)
                       (#\t #\tab)
                       (#\f #\formfeed)
                       (#\b #\backspace)
                       (#\r #\return)

                       ;; read unicode character
                       (#\u (let ((x1 (digit-char-p (read-char stream) 16))
                                  (x2 (digit-char-p (read-char stream) 16))
                                  (x3 (digit-char-p (read-char stream) 16))
                                  (x4 (digit-char-p (read-char stream) 16)))
                              (code-char (logior (ash x1 12)
                                                 (ash x2  8)
                                                 (ash x3  4)
                                                 (ash x4  0)))))

                       ;; verbatim character
                       (otherwise c))))
              (write-char c s))))))

;;; ----------------------------------------------------

(defun json-read-list (stream)
  "Read a list of JSON values."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected open bracket
  (json-read-char stream #\[ :skip-ws t)

  ;; check for an empty list
  (if (json-peek-char stream #\] :skip-ws t)
      nil
    (loop
       for x = (json-read stream)
       collect x
       into xs

       ;; check for another element
       while (json-peek-char stream #\, :skip-ws t)

       ;; return the final list
       finally (return (prog1 xs
                         (json-read-char stream #\] :skip-ws t))))))

;;; ----------------------------------------------------

(defun json-read-object (stream)
  "Read an associative list of key/value pairs into a JSON object."
  (declare (optimize (speed 3) (debug 0)))

  ;; read the expected open brace
  (json-read-char stream #\{ :skip-ws t)

  ;; check for an empty object
  (if (json-peek-char stream #\} :skip-ws t)
      (make-instance 'json-object)
    (loop
       for key = (json-read-string stream)
       for value = (progn
                     (json-read-char stream #\: :skip-ws t)
                     (json-read stream))

       ;; build the associative list of members
       collect (list key value)
       into xs

       ;; check for another element
       while (json-peek-char stream #\, :skip-ws t)

       ;; return the final list
       finally (return (prog1 (make-instance 'json-object :members xs)
                         (json-read-char stream #\} :skip-ws t))))))
