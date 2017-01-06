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

;;;; af.lib.clone.lisp

(in-package #:cl-user)

(defpackage af.lib.clone
  (:use :cl
        :af.lib.io
        :cl-json)
  (:export
   :clone-project
   ))

(in-package #:af.lib.clone)

(defun clone-project (from-path to-path from-name to-name)
  "Recursively copy FROM-PATH to TO-PATH, while replacing all
occurences of FROM-NAME to TO-NAME."
  (let ((nodes (directory-tree from-path)))
    nodes))

;;; "af.lib.clone" goes here. Hacks and glory await!