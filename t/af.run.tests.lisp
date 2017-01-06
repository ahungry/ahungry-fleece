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
        :af.lib.io
        :af.lib.ansi-colors
        :af.lib.loggy
        :af.lib.hashy
        :af.lib.testy)
  (:export :main))

(in-package #:af.run.tests)

(defparameter *base-directory* (asdf:system-source-directory :ahungry-fleece))
(defparameter *yml* nil "The yml file for testing.")

(defun main ()
  "Begin the tests!"
  ;; See if we're in the shell environment or not (SLIME will use 'dumb' here)
  (unless (and (sb-ext:posix-getenv "AF_LIB_TESTY_COLORIZE")
               (> (length (sb-ext:posix-getenv "AF_LIB_TESTY_COLORIZE")) 0))
      (setf af.lib.ansi-colors:*colorize-p* nil))

  (if (suite
       "af.lib"

       (desc
        "af.lib.io"

        (it "Should be able to tell a directory"
            (eq t (af.lib.io:directory-p (user-homedir-pathname))))

        (it "Should be able to tell a file"
            (eq t (af.lib.io:file-p "/dev/null")))
        )

       (desc
        "af.lib.loggy"

        (it "Should send to proper stream when levels are set"
            (progn
              (setf (af.lib.loggy:Level *loggy*) 'debug)
              (setf (af.lib.loggy:Output-Stream *loggy*) nil)
              (eq "Hello" (log-> *loggy* 'debug "Hello"))))

        (it "Should not print debug when set to warn"
            (progn
              (setf (af.lib.loggy:Level *loggy*) 'warn)
              (not (eq "Hello" (log-> *loggy* 'debug "Hello")))))
        )

       (desc
        "af.lib.hashy"

        (it "Should let me easily access a yml property"
            (and
             (setf *yml* (hash-from-yaml-file
                          (format nil "~a/t/fixtures/pets.yml"
                                  (asdf:system-source-directory :ahungry-fleece))))
             (equal "object" (af.lib.hashy:ref "#/definitions/Pet/type" *yml*))))

        (it "Should parse alist to hash properly"
            (equal 1 (ref "#/one/ione"
                          (alist-to-hash
                           '((:one . ((:ione . 1))) (:two . 2))))))

        (it "Should undo cl-json key mangling"
            (equal "DogTime" (stringify :*dog-time)))

        (it "Should return same results from yaml or json"
            (let* ((fpath (format nil "~a/t/fixtures/"
                                  (asdf:system-source-directory :ahungry-fleece)))
                   (yamlobj (hash-from-yaml-file (format nil "~a~a"
                                                         fpath
                                                         "pets.yml")))
                   (jsonobj (hash-from-json-file (format nil "~a~a"
                                                         fpath
                                                         "pets.json"))))
              (equal (dump yamlobj)
                     (dump jsonobj))))

        (it "Should allow setting of nested hash tables"
            (let ((hash1 (make-hash-table :test #'equal))
                  (hash2 (make-hash-table :test #'equal)))
              (setf (gethash "findme" hash2) 42)
              (setf (gethash "hash2" hash1) hash2)
              (and
               (equal 42 (gethash "findme" (gethash "hash2" hash1)))
               (progn
                 (setf (gethash "findme" hash2) 33)
                 (equal 33 (gethash "findme" (gethash "hash2" hash1)))
                 )
               (equal 33 (ref "#/hash2/findme" hash1))
               (progn
                 (ref "#/hash2/findme" hash1 99)
                 (equal 99 (ref "#/hash2/findme" hash1)))
               t)
              ))
        )

       ) ;; end suite
      (setf sb-ext:*exit-hooks* (list (lambda () (sb-ext:exit :code 0))))
      (setf sb-ext:*exit-hooks* (list (lambda () (sb-ext:exit :code 1))))))

;;; "af.run.tests" goes here. Hacks and glory await!
