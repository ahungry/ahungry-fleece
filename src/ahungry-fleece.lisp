;;;; ahungry-fleece.lisp

(in-package #:cl-user)

(defpackage ahungry-fleece
  (:use :cl
        :cl-json
        :af.lib.hashy)
  (:export :main))

(in-package #:ahungry-fleece)

(defun main ()
  "Well...guess we can print the version here."
  (print "0.0.1"))

;;; "ahungry-fleece" goes here. Hacks and glory await!
