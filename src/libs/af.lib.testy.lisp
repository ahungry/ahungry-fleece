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

;;;; af.lib.testy.lisp

(in-package #:cl-user)

(defpackage af.lib.testy
  (:use :cl
        :cl-json
        :af.lib.ansi-colors)
  (:export
   :suite
   :desc
   :it))

(in-package #:af.lib.testy)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro suite (desc &rest results)
    "Describe a suite of tests."
    `(progn
       (af.lib.ansi-colors:with-color :blue
         (format t "~%~a~%" ,desc))
       (let ((results (list ,@results)))
         (let ((color (if (eq 0 (count nil results)) :light-green :light-red)))
           (with-color color
             (format t "~%~a tests, ~a failures~%"
                     (length results)
                     (count nil results))))
         (eq 0 (count nil results)))))

  (defmacro desc (desc &rest results)
    "Describe a set of test."
    `(progn
       (af.lib.ansi-colors:with-color :cyan
         (format t "~%  ~a~%~%" ,desc))
       (let ((results (list ,@results)))
         (let ((color (if (eq 0 (count nil results)) :light-green :light-red)))
           (with-color color
             (format t "~%  ~a assertions, ~a failures~%~%"
                     (length results)
                     (count nil results))))
         (eq 0 (count nil results))))))

(defun it (desc result)
  "Assert the body evaluates as expected."
  (let ((color (if result :green :red)))
    (af.lib.ansi-colors:with-color color
      (format t "    ~a ~a~%"
              (if result "+" "-")
              desc)))
  result)

;;; "af.lib.testy" goes here. Hacks and glory await!
