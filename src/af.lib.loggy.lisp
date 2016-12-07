;; Ahungry Fleece - A JSON (and friends) utility library.
;; Copyright (C) 2016 Matthew Carter <m@ahungry.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.
;;
;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;;; af.lib.loggy.lisp

(in-package #:cl-user)

(defpackage af.lib.loggy
  (:use :cl
        :cl-json)
  (:export
   :log->
   :flog
   :*loggy*
   ))

(in-package #:af.lib.loggy)

(defclass Loggy ()
  (
   (Level
    :accessor Level
    :initarg :level
    :initform 'debug)
   (Output
    :accessor Output
    :initarg :output
    :initform 'print)
   ))

(defun get-level (level)
  "Get a numeric level from LEVEL."
  (case level
    (debug 9)
    (warn 7)
    (info 5)
    (crit 3)
    (t 0)))

(defgeneric log-> (object level &rest message)
  (:documentation "Write a log level message."))

(defmethod log-> ((logger Loggy) level &rest message)
  "Produce a log level message.

Sample call:

  (log-> *loggy* 'debug \"Hello World\")"
  (when (>= (get-level (Level logger))
            (get-level level))
    (apply (Output logger) message)))

(defparameter *loggy* (make-instance 'Loggy))

(defun flog (level &rest message)
  "Log to the singleton."
  (apply #'log-> *loggy* level message))

;;; "af.lib.loggy" goes here. Hacks and glory await!
