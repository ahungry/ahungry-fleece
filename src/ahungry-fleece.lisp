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

;;;; ahungry-fleece.lisp

(in-package #:cl-user)

(defpackage ahungry-fleece
  (:use :cl
        :cl-json
        :quickproject
        :af.lib.clone
        :af.lib.hashy)
  (:export :main
           :make-skeleton-project
           ))

(in-package #:ahungry-fleece)

(defparameter *base-directory* (asdf:system-source-directory :ahungry-fleece))

(defun make-skeleton-project (directory)
  "Create a new project in DIREcTORY with name equal to the directory.

Similar to quickproject:make-project, but also ensures the
created files closely match what is available in this own project's
directory structure setup (with CLI based unit test etc.)."
  (quickproject:make-project directory)
  (clone-project
   (merge-pathnames #P"skel" *base-directory*)
   directory
   "skeleton"
   (pathname-name directory)))

(defun main ()
  "Well...guess we can print the version here."
  (print "0.0.2"))

;;; "ahungry-fleece" goes here. Hacks and glory await!
