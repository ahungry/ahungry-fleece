;; Ahungry Fleece - A utility library.
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

;;;; af.lib.coverage.lisp

;; Code based off of SBCL source code:
;; https://github.com/sbcl/sbcl/blob/622c9daf9bb41ef9ad4b8a063c62c4baf59a1c1a/contrib/sb-cover/cover.lisp

;; Original copyright note:

;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

;;; This module includes a modified version of the source path parsing
;;; routines from Swank. That code was written by Helmut Eller, and
;;; was placed under Public Domain

(in-package #:cl-user)

(defpackage af.lib.coverage
  (:use :cl)
  (:export :report-json
           :with-coverage
           ))

(in-package #:af.lib.coverage)

(defun report-json (ok)
  "stub"
  nil)

;; Newly contributed GPLv3 code goes down here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-coverage (package &rest body)
    "Run BODY with coverage enabled."
    `(progn
       (declaim (optimize sb-cover:store-coverage-data))
       (with-open-stream (*error-output* (make-broadcast-stream))
         (with-open-stream (*standard-output* (make-broadcast-stream))
           (asdf:oos 'asdf:load-op ,package :force t)
           ))
       ,@body
       (declaim (optimize (sb-cover:store-coverage-data 0)))))
  )
