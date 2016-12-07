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

;;;; af.run.tests.lisp

(in-package #:cl-user)

(defpackage af.run.tests
  (:use :cl
        :af.lib.loggy
        :af.lib.testy)
  (:export :main))

(in-package #:af.run.tests)

(defun main ()
  "Begin the tests!"
  (desc "af.lib.loggy:"

   (it "Should print to stdout when default level is set"
       (progn
       (eq "Hello" (log-> *loggy* 'debug "Hello"))))

   (it "Should not print debug when set to warn"
       (progn
       (setf (af.lib.loggy:Level *loggy*) 'warn)
       (not (eq "Hello" (log-> *loggy* 'debug "Hello")))))

   (it "Should break"
       (eq 2 1))
   ))

;;; "af.run.tests" goes here. Hacks and glory await!