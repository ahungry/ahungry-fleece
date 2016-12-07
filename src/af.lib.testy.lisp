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

;;;; af.lib.testy.lisp

(in-package #:cl-user)

(defpackage af.lib.testy
  (:use :cl
        :cl-json)
  (:export
   :warn
   :critical
   :debug))

(in-package #:af.lib.testy)

(defmacro desc (desc &rest body)
  "Describe a set of test."
  `(progn (format t "~a~%" ,desc)
          ,@body))

(defmacro it (desc &rest body)
  "Assert the body evaluates as expected."
  `(let ((result ,@body))
     (format t "  ~a ~a~%"
             (if result "+" "-")
             ,desc)))

;;; "af.lib.testy" goes here. Hacks and glory await!
