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
  (defmacro test-wrapper (format-title format-summary  desc &rest results)
    "Describe a suite of tests."
    `(progn
       (af.lib.ansi-colors:with-color :blue
         (format t ,format-title ,desc))
       (let ((results (list ,@results)))
         (let ((color (if (zerop (count nil results)) :light-green :light-red)))
           (with-color color
             (format t ,format-summary
                     (length results)
                     (count nil results))))
         (zerop (count nil results)))))

  (defmacro suite (desc &rest results)
    "Describe a suite of tests."
    `(test-wrapper "~%~a~%" "~%~a tests, ~a failures~%" ,desc ,@results))

  (defmacro desc (desc &rest results)
    "Describe a set of test."
    `(test-wrapper "~%~%  ~a~%~%" "~%  ~a assertions, ~a failures~%" ,desc ,@results))
  ) ;; eval-when

(defun it (desc result)
  "Assert the body evaluates as expected."
  (let ((color (if result :green :red)))
    (af.lib.ansi-colors:with-color color
      (format t "    ~a ~a~%"
              (if result "+" "-")
              desc)))
  result)

;;; "af.lib.testy" goes here. Hacks and glory await!
