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

;;;; af.lib.io.lisp

(in-package #:cl-user)

(defpackage af.lib.io
  (:use :cl
        :cl-json)
  (:export
   :directory-p
   :directory-tree
   :file-p
   :file-get-contents
   ))

(in-package #:af.lib.io)

(defun directory-p (path)
  "Check if PATH is a directory (a directory will have the trailing slash)."
  (let ((dir (directory path)))
    (when dir
      (not (pathname-name (car dir))))))

(defun directory-tree (directory &optional (tree '()))
  "Recursively get all files in a directory."
  (let* ((path (pathname directory))
         (directories (format nil "~a/*.*" path)))
    (append tree directories)))

(defun file-p (path)
  "Check if PATH is a file (a directory will have the trailing slash)."
  (let ((dir (directory path)))
    (when dir
      (when (pathname-name (car dir)) t))))

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
    (format nil "~{~a~^~%~}" lines)))

(defun file-put-contents (filename content)
  "Write to FILENAME the CONTENT string sent in.

If the file exists, will append to the file.

If the file does not exist, will create it."
  (let ((lines (split-sequence:split-sequence #\Newline content)))
         (with-open-file
             (stream filename
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
           (loop for line in lines
                do (write-line line stream )))))

;;; "af.lib.io" goes here. Hacks and glory await!
