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

;;;; af.lib.io.lisp

(in-package #:cl-user)

(defpackage af.lib.io
  (:use :cl
        :cl-json)
  (:export
   :file-get-contents
   ))

(in-package #:af.lib.io)

(defun file-get-contents (filename)
  "Read in FILENAME and return as a single string."
  (let ((lines
         (with-open-file
             (stream filename
                     :direction :input
                     :if-does-not-exist :error)
           (when stream
             (loop for line = (read-line stream nil 'eof)
                until (eq line 'eof)
                collect line)))))
    (format nil "~{~a~%~}" lines)))

;;; "af.lib.io" goes here. Hacks and glory await!
