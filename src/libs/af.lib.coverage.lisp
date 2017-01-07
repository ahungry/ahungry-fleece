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

;;;; af.lib.coverage.lisp

;; Code based off of SBCL source code:
;; https://github.com/sbcl/sbcl/blob/622c9daf9bb41ef9ad4b8a063c62c4baf59a1c1a/contrib/sb-cover/cover.lisp

;; Original copyright note:

;;; A frontend for the SBCL code coverage facility. Written by Juho
;;; Snellman, and placed under public domain.

;;; This module includes a modified version of the source path parsing
;;; routines from Swank. That code was written by Helmut Eller, and
;;; was placed under Public Domain

(in-package #:cl-user)

(defpackage af.lib.coverage
  (:use :cl :sb-c :sb-cover)
  (:export :report-cli
           :with-coverage
           ))

(in-package #:af.lib.coverage)

(defun report-cli (directory)
  "Print a code coverage report of all instrumented files into DIRECTORY.
If DIRECTORY does not exist, it will be created. The main report will be
printed to the file cover-index.html. The external format of the source
files can be specified with the EXTERNAL-FORMAT parameter.

If the keyword argument FORM-MODE has the value :CAR, the annotations in
the coverage report will be placed on the CARs of any cons-forms, while if
it has the value :WHOLE the whole form will be annotated (the default).
The former mode shows explicitly which forms were instrumented, while the
latter mode is generally easier to read."
  (let* ((paths)
         (directory (sb-cover::pathname-as-directory directory))
         (*default-pathname-defaults* (translate-logical-pathname directory)))
    (ensure-directories-exist *default-pathname-defaults*)
    (maphash (lambda (k v)
               (declare (ignore v))
               (let* ((pk (translate-logical-pathname k))
                      (n (format nil "~(~{~2,'0X~}~)"
                                 (coerce (md5:md5sum-string
                                          (sb-ext:native-namestring pk))
                                         'list)))
                      (path (make-pathname :name n :type "html" :defaults directory)))
                 (when (probe-file k)
                   (ensure-directories-exist pk)
                   (with-open-file (stream path
                                           :direction :output
                                           :if-exists :supersede
                                           :if-does-not-exist :create)
                     (push (list* k n (sb-cover::report-file k stream :default))
                           paths)))))
             *code-coverage-info*)
    (let ((report-file (make-pathname :name "cover-index" :type "html" :defaults directory)))
      (with-open-stream (stream *standard-output*)
        ;;(sb-cover::write-styles stream)
        (unless paths
          (warn "No coverage data found for any file, producing an empty report. Maybe you~%forgot to (DECLAIM (OPTIMIZE SB-COVER:STORE-COVERAGE-DATA))?")
          (format stream "<h3>No code coverage data found.</h3>")
          (close stream)
          (return-from report-cli))

        ;; @todo Get format under 80 char width
        (format stream "                                        EXPRESSION                          BRANCH~%")
        (format stream "----------------------------------------------------------------------------------------------------~%")
        (format stream "Source File                    ~{~12A~}~%"
                (list
                 "Covered" "Total" "      %"
                 "Covered" "Total" "      %"))
        (format stream "----------------------------------------------------------------------------------------------------~%")
        (setf paths (sort paths #'string< :key #'car))
        (loop for prev = nil then source-file
           for (source-file report-file expression branch) in paths
           for even = nil then (not even)
           do (when (or (null prev)
                        (not (equal (pathname-directory (pathname source-file))
                                    (pathname-directory (pathname prev)))))
                (format stream "~%~%~A"
                        (namestring (make-pathname :directory (pathname-directory (pathname source-file)))))
                )
           do (format stream "~%    ~30a ~{~:[-~;~:*  ~8,a~] ~:[-~;~:*~8a~]~:[       -~;~:*~8,1f~]          ~}"
                      (enough-namestring (pathname source-file)
                                         (pathname source-file))
                      (list (sb-cover::ok-of expression)
                            (sb-cover::all-of expression)
                            (sb-cover::percent expression)
                            (sb-cover::ok-of branch)
                            (sb-cover::all-of branch)
                            (sb-cover::percent branch))))
        (format stream "~%~%"))
      report-file)))

;; Newly contributed GPLv3 code goes down here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-coverage (package &rest body)
    "Run BODY with coverage enabled."
    `(progn
       (declaim (optimize sb-cover:store-coverage-data))
       (with-open-stream (*error-output* (make-broadcast-stream))
         (with-open-stream (*standard-output* (make-broadcast-stream))
           (asdf:oos 'asdf:load-op ,package :force t)
           ))
       ,@body
       (declaim (optimize (sb-cover:store-coverage-data 0)))))
  )
