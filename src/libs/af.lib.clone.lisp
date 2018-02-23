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
        :cl-ppcre
        :cl-json)
  (:export
   :clone-project
   :rename-path
   ))

(in-package #:af.lib.clone)

(defun rename-path (from-path to-path file-name from-name to-name)
  "Rename a file in a path, as such:

Given a FILE-NAME /one/two/sub/three with a FROM-PATH of /one/two
and a TO-PATH of /uno/dos/sub/, with a FROM-NAME three and TO-NAME tres,
produce a path of /uno/dos/sub/tres.

@todo Add test
"
  (let* ((file-segment (subseq (af.lib.io:pathname-to-string file-name)
                               (1+ (length (af.lib.io:pathname-to-string from-path)))))
         (file-renamed (cl-ppcre:regex-replace-all from-name file-segment to-name)))
    (merge-pathnames file-renamed to-path)))

(defun clone-project (from-path to-path from-name to-name)
  "Recursively copy FROM-PATH to TO-PATH, while replacing all
occurences of FROM-NAME to TO-NAME."
  (let ((nodes (directory-tree from-path))
        (to-path (format nil "~a/" to-path)))
    (loop for node in nodes
       when (file-p node)
       collect
         (let ((out-file (rename-path from-path to-path node from-name to-name)))
           (handler-case
               (let* ((in-content (file-get-contents node))
                      (out-content (cl-ppcre:regex-replace-all from-name in-content to-name)))
                 (file-put-contents out-file out-content :overwrite t)
                 out-file)
             (sb-int:stream-decoding-error ()
               ;; UTF8 binary issue with characters most likely...
               (progn
                 (file-put-binary-contents out-file (file-get-binary-contents node) :overwrite t)
                 out-file))
             (error (err)
               (print err)))))))

;;; "af.lib.clone" goes here. Hacks and glory await!
