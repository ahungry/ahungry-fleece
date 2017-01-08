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
   :dump
   :hash-from-json-file
   :hash-from-json-string
   :hash-from-yaml-file
   :hash-from-yaml-string
   :ref
   :stringify
   :dotp
   :with-hashy
   ))

(in-package #:af.lib.hashy)

(defun stringify (symbol)
  "Convert a cl-json symbol back into original representation."
  (let* ((decoded (cl-json:encode-json-to-string `((,symbol . ""))))
         (start-pos 2) ;; after the {"
         (end-pos (position #\" (subseq decoded start-pos))))
    (subseq decoded start-pos (+ start-pos end-pos))))

(defun dotp (pair)
  "T if PAIR is a dotted pair and not a list."
  (not (listp (cdr pair))))

(defun alist-to-hash (alist)
  "Convert ALIST into a nested hash."
  (let ((hash (make-hash-table :test #'equal)))
    (cond
      ;; We receive something like '(:this . "that")
      ((dotp alist)
       (setf (gethash (stringify (car alist)) hash)
             (cdr alist)))

      ;; We receive something like '(1 2 3)
      ((not (listp (car alist)))
       alist)

      ;; We receive something like '((:list . "ofStuff"))
      ((listp (caar alist))
       (mapcar (lambda (item)
                 (alist-to-hash item))
               alist))

      ;; We receive something like '(:here . ((:we . "go")))
      ;; Convert each item in the alist to a hash key
      (t
       (loop for item in alist
          do (let ((key (stringify (car item)))
                   (item (cdr item)))
               (if (listp item) ;; We know it isn't an atom
                   (setf (gethash key hash) (alist-to-hash item))
                   (setf (gethash key hash) item))))
       hash))))

(defun hash-from-json-string (json-string)
  "Create a hash from the JSON-STRING."
  (alist-to-hash (cl-json:decode-json-from-string json-string)))

(defun hash-from-json-file (filename)
  "Read in FILENAME and create a hash."
  (hash-from-json-string (file-get-contents filename)))

(defun hash-from-yaml-string (yaml-string)
  "Read in YAML-STRING and create a hash."
  (yaml:parse yaml-string))

(defun hash-from-yaml-file (filename)
  "Read in FILENAME and create a hash."
  (hash-from-yaml-string (file-get-contents filename)))

(defmacro with-hashy ((target &key (type :json)) &body body)
  "Create and bind to HASHY object (hash-table) from the TARGET.

- If TARGET is a pathname, will read in the file and decode.
- If TARGET is a string, will decode the contents.
- todo: If TARGET is a URI, will retrieve remote contents and decode."
  `(progn
     (let (fn)
     ,(cond
        ;; Handle json pathname
        ((and (eq :json type)
              (pathnamep target))
         `(progn
            (setf fn #'hash-from-json-file)))

        ;; Handle json strings
        ((and (eq :json type)
              (stringp target))
         `(progn
            (setf fn #'hash-from-json-string)))

        ;; Handle yaml pathname
        ((and (eq :yaml type)
              (pathnamep target))
         `(progn
            (setf fn #'hash-from-yaml-file)))

        ;; Handle yaml strings
        ((and (eq :yaml type)
              (stringp target))
         `(progn
            (setf fn #'hash-from-yaml-string)))

        )
     (let ((hashy (funcall fn ,target)))
       ,@body))))

(defun dump (object)
  "Return the object as json."
  (yaml:emit-to-string object))

(defun ref (path object &optional value (split-char #\/))
  "Query a reference to a json or yaml object.

PATH is a path such as #/key/anotherKey/anotherKey/node.

OBJECT is a hash table (optionally made up of nested ones or lists).

VALUE is an optional value to change the node to.

For example, calling on a JSON object like:

  {\"definitions\": {\"Pet\": {\"name\": \"Fido\"}}}

or YML object like:

  definitions:
    Pet:
      name: Fido

as such:

  (ref \"#/definitions/Pet/name\" obj)

Will return the setf'able value \"Fido\"."
  (let ((args (cdr (split-sequence:split-sequence split-char path)))
        (chain '("#"))
        (iter 0))
    (reduce
     (lambda (acc arg)
       (push arg chain)
       (incf iter)
       (cond
         ;; If we failed to get a node, keep returning nil.
         ((eq nil acc)
          (flog 'warn
                (format nil "#'ref missed node: ~{~a~^/~}" (reverse chain)))
          nil)

         ;; When it's a hash table, get via hash.
         ((hash-table-p acc)
          (when (and value (eq (1+ iter) (length args)))
            (setf (gethash arg acc) value))
          (gethash arg acc))

         ;; When it's a list, grab the nth value.
         ((listp acc) (nth (parse-integer arg :junk-allowed t) acc))

         ;; Return nothing if nothing was found.
         (t nil)))
     (push object args))))

;;; "af.lib.hashy" goes here. Hacks and glory await!
