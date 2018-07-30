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
  (:use :cl
        :sb-c
        :af.contrib.sb-cover
        :af.lib.ansi-colors
        :af.lib.io
        :af.lib.hashy)
  (:export :report-cli
           :report-json
           :with-coverage
           ))

(in-package #:af.lib.coverage)

(defmacro code-coverage-hashtable () `(car sb-c::*code-coverage-info*))

(defun report-json (directory)
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
         (directory (af.contrib.sb-cover::pathname-as-directory directory))
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
                     (push (list* k n (af.contrib.sb-cover::report-file k stream :default))
                           paths)))))
             (code-coverage-hashtable))
    (let ((report-file (make-pathname :name "coverage" :type "json" :defaults directory))
          (directory-node "")
          (json (make-hash-table :test #'equal)))
      (with-open-file (stream report-file
                              :direction :output :if-exists :supersede
                              :if-does-not-exist :create)
        ;;(af.contrib.sb-cover::write-styles stream)
        (unless paths
          (warn "No coverage data found for any file, producing an empty report. Maybe you~%forgot to (DECLAIM (OPTIMIZE AF.CONTRIB.SB-COVER:STORE-COVERAGE-DATA))?")
          (format stream "<h3>No code coverage data found.</h3>")
          (return-from report-json))

        (setf paths (sort paths #'string< :key #'car))
        (loop for prev = nil then source-file
           for (source-file report-file expression branch) in paths
           for even = nil then (not even)
           do (when (or (null prev)
                        (not (equal (pathname-directory (pathname source-file))
                                    (pathname-directory (pathname prev)))))
                ;; Create a new directory key for the JSON
                (setf directory-node (namestring (make-pathname :directory (pathname-directory (pathname source-file)))))
                (setf (gethash directory-node json) (list))
                )
           do (let ((json-file-coverage (make-hash-table :test #'equal))
                    (json-line-coverage (make-hash-table :test #'equal))
                    (json-branch-coverage (make-hash-table :test #'equal)))

                ;; Now build the branches
                (setf (gethash "covered" json-line-coverage)
                      (af.contrib.sb-cover::ok-of expression))
                (setf (gethash "total" json-line-coverage)
                      (af.contrib.sb-cover::all-of expression))
                (setf (gethash "percent" json-line-coverage)
                      (if (af.contrib.sb-cover::percent expression)
                          (float (af.contrib.sb-cover::percent expression))
                          nil))

                (setf (gethash "covered" json-branch-coverage)
                      (af.contrib.sb-cover::ok-of branch))
                (setf (gethash "total" json-branch-coverage)
                      (af.contrib.sb-cover::all-of branch))
                (setf (gethash "percent" json-branch-coverage)
                      (if (af.contrib.sb-cover::percent branch)
                          (float (af.contrib.sb-cover::percent branch))
                          nil))

                (setf (gethash "name" json-file-coverage)
                      (format nil "~a" (enough-namestring (pathname source-file) (pathname source-file))))
                (setf (gethash "line" json-file-coverage) json-line-coverage)
                (setf (gethash "branch" json-file-coverage) json-branch-coverage)

                (push json-file-coverage (gethash directory-node json))
                ))
        (format stream (dump json))
        ))))

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
         (directory (af.contrib.sb-cover::pathname-as-directory directory))
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
                     (push (list* k n (af.contrib.sb-cover::report-file k stream :default))
                           paths)))))
             (code-coverage-hashtable))
    (let ((report-file (make-pathname :name "cover-index" :type "html" :defaults directory)))
      ;;(af.contrib.sb-cover::write-styles t)
      (unless paths
        (warn "No coverage data found for any file, producing an empty report. Maybe you~%forgot to (DECLAIM (OPTIMIZE AF.CONTRIB.SB-COVER:STORE-COVERAGE-DATA))?")
        (format t "<h3>No code coverage data found.</h3>")
        (return-from report-cli))

      ;; @todo Get format under 80 char width
      (format t "                                        EXPRESSION                          BRANCH~%")
      (format t "----------------------------------------------------------------------------------------------------~%")
      (format t "Source File                    ~{~12A~}~%"
              (list
               "Covered" "Total" "      %"
               "Covered" "Total" "      %"))
      (format t "----------------------------------------------------------------------------------------------------~%")
      (setf paths (sort paths #'string< :key #'car))
      (loop for prev = nil then source-file
         for (source-file report-file expression branch) in paths
         for even = nil then (not even)
         do (when (or (null prev)
                      (not (equal (pathname-directory (pathname source-file))
                                  (pathname-directory (pathname prev)))))
              (format t "~%~%~A"
                      (namestring (make-pathname :directory (pathname-directory (pathname source-file)))))
              )
         do (format t "~%    ~30a ~{~:[-~;~:*  ~8,a~] ~:[-~;~:*~8a~]~:[       -~;~:*~8,1f~]          ~}"
                    (enough-namestring (pathname source-file)
                                       (pathname source-file))
                    (list (af.contrib.sb-cover::ok-of expression)
                          (af.contrib.sb-cover::all-of expression)
                          (af.contrib.sb-cover::percent expression)
                          (af.contrib.sb-cover::ok-of branch)
                          (af.contrib.sb-cover::all-of branch)
                          (af.contrib.sb-cover::percent branch))))
      (format t "~%~%")
      report-file)))

;; Newly contributed GPLv3 code goes down here
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defmacro with-coverage (package &rest body)
    "Run BODY with coverage enabled."
    `(progn
       (declaim (optimize af.contrib.sb-cover:store-coverage-data))
       (with-open-stream (*error-output* (make-broadcast-stream))
         (with-open-stream (*standard-output* (make-broadcast-stream))
           ;; (asdf:oos 'asdf:load-op ,package :force t)
           (print "Well, this would be a load, but whatever...")
           ))
       ,@body
       (declaim (optimize (af.contrib.sb-cover:store-coverage-data 0)))))
  )
