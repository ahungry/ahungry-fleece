;;;; af.lib.loggy.lisp

(in-package #:cl-user)

(defpackage af.lib.loggy
  (:use :cl
        :cl-json)
  (:export
   :warn
   :critical
   :debug))

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
    (funcall (Output logger) message)))

(defparameter *loggy* (make-instance 'Loggy))

;;; "af.lib.loggy" goes here. Hacks and glory await!
