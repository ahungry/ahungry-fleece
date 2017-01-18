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
   :file-put-contents
   :file-get-binary-contents
   :file-put-binary-contents
   :pathname-to-string
   ))

(in-package #:af.lib.io)

(defun directory-p (path)
  "Check if PATH is a directory (a directory will have the trailing slash)."
  (let ((dir (directory path)))
    (when dir
      (not (pathname-name (car dir))))))

(defun directory-tree (directory)
  "Recursively get all files in a DIRECTORY."
  (let* ((path (pathname directory))
         (nodes (directory (format nil "~a/*.*" path))))
    ;; Recursively grab the directories
    (loop for node in nodes
       when (directory-p node)
       do (setq nodes (append nodes (directory-tree node))))
    nodes))

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

(defun file-put-contents (filename content &key (overwrite nil))
  "Write to FILENAME the CONTENT string sent in.

If the file exists, will append to the file.

If the file does not exist, will create it."
  (let ((lines (split-sequence:split-sequence #\Newline content)))
    (ensure-directories-exist filename)
    (with-open-file
        (stream filename
                :direction :output
                :if-exists (if overwrite :supersede :append)
                :if-does-not-exist :create)
      (loop for line in lines
         do (write-line line stream)))))

(defun file-get-binary-contents (filename)
  "Read in FILENAME and return as a byte list."
  (let ((bytes
         (with-open-file
             (stream filename
                     :direction :input
                     :if-does-not-exist :error
                     :element-type '(unsigned-byte 8)
                     :external-format :utf-8)
           (when stream
             (loop for byte = (read-byte stream nil 'eof)
                until (eq byte 'eof)
                collect byte)))))
    bytes))

(defun file-put-binary-contents (filename content &key (overwrite nil))
  "Write to FILENAME the CONTENT bytes sent in.

If the file exists, will append to the file.

If the file does not exist, will create it."
  (let ((bytes content))
    (ensure-directories-exist filename)
    (with-open-file
        (stream filename
                :direction :output
                :if-exists (if overwrite :supersede :append)
                :if-does-not-exist :create
                :element-type '(unsigned-byte 8)
                :external-format :utf-8)
      (loop for byte in bytes
         do (write-byte byte stream)))))

(defun pathname-to-string (pathname)
  "Change PATHNAME into a string. @todo Add test"
  (format nil "~a" pathname))

(defun directory-structure-helper (directory)
  "List the directories (recursively)."
  (let ((dirs (directory (format nil "~a/*/" directory))))
    ;; Recursively grab the directories
    (loop for dir in dirs
       do (setq dirs (append dirs (directory-structure dir))))
    dirs))

(defun directory-structure (directory)
  "Directory structure (including the root dir)."
  (append (list (pathname directory)) (directory-structure-helper directory)))

(defun find-file (path name)
  "get all files matching a portion of name. @todo use threading here"
  (let ((dirs (directory-structure path))
        (matches '()))
    (loop for dir in dirs
       for files = (append (directory (format nil "~a/*.*" dir))
                           (directory (format nil "~a/*" dir)))
       do (loop for file in files
             when (search name (pathname-to-string file))
             do (push file matches)))
    matches))

;;; "af.lib.io" goes here. Hacks and glory await!
