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

;;;; af.lib.hashy.lisp

(in-package #:cl-user)

(defpackage af.lib.hashy
  (:use :cl
        :cl-json
        :cl-yaml
        :split-sequence
        :af.lib.loggy
        :af.lib.io)
  (:export
   :alist-to-hash
   :hash-from-json-file
   :hash-from-yaml-file
   :ref
   ))

(in-package #:af.lib.hashy)

(defun dotp (pair)
  "T if PAIR is a dotted pair and not a list."
  (not (listp (cdr pair))))

(defun alist-to-hash (alist)
  "Convert ALIST into a nested hash."
  (let ((hash (make-hash-table :test #'equal)))
    (cond
      ;; We receive something like '(:this . "that")
      ((dotp alist)
       (setf (gethash (string-downcase (string (car alist))) hash)
             (cdr alist)))

      ;; We receive something like '(1 2 3)
      ((not (listp (car alist)))
       alist)

      ;; We receive something like '((:list . "ofStuff"))
      ((listp (car (car alist)))
       (mapcar (lambda (item)
                 (alist-to-hash item))
               alist)
       )

      ;; We receive something like '(:here . ((:we . "go")))
      ;; Convert each item in the alist to a hash key
      (t
       (loop for item in alist
          do (if
              (listp (car item))
              (progn
                (print "WTF")
                (list (alist-to-hash item))
                ;;(setf (gethash "??" hash) (alist-to-hash item))
                )
              (let ((key (string-downcase (string (car item))))
                   (item (cdr item)))
               (if (listp item) ;; We know it isn't an atom
                   (setf (gethash key hash) (alist-to-hash item))
                   (setf (gethash key hash) item)))))
       hash))))

(defun alist-from-json-file (filename)
  "Read in FILENAME and create an alist."
  (let ((json (cl-json:decode-json-from-string
               (file-get-contents filename))))
    json))

(defun hash-from-json-file (filename)
  "Read in FILENAME and create a hash."
  (let ((json (alist-from-json-file filename)))
    (alist-to-hash json)))

(defun hash-from-yaml-file (filename)
  "Read in FILENAME and create a hash."
  (yaml:parse (file-get-contents filename)))

(defun dump (object)
  "Return the object as json."
  (yaml:emit-to-string object))

(defun ref (path object)
  "Query a reference to a JSON or YML object.

For example, calling on a JSON object like:

  {\"definitions\": {\"Pet\": {\"name\": \"Fido\"}}}

or YML object like:

  definitions:
    Pet:
      name: Fido

as such:

  (ref \"#/definitions/Pet/name\" obj)

Will return the setf'able value \"Fido\"."
  (let ((args (cdr (split-sequence:split-sequence #\/ path)))
        (chain '("#")))
    (reduce
     (lambda (acc arg)
       (push arg chain)
       (cond
         ;; If we failed to get a node, keep returning nil.
         ((eq nil acc)
          (flog 'warn
                (format nil "#'ref missed node: ~{~a~^/~}" (reverse chain)))
          nil)

         ;; When it's a hash table, get via hash.
         ((hash-table-p acc) (gethash arg acc))

         ;; When it's a list, grab the nth value.
         ((listp acc) (nth (parse-integer arg :junk-allowed t) acc))

         ;; Return nothing if nothing was found.
         (t nil)))
     (push object args))))

;;; "af.lib.hashy" goes here. Hacks and glory await!
