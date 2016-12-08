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
        :af.lib.io)
  (:export
   :json-to-hash
   :ref
   ))

(in-package #:af.lib.hashy)

(defun json-to-hash (json)
  "Convert a json string into a hash table."
  json)

(defun hash-from-yaml-file (filename)
  "Read in FILENAME and create a hash."
  (yaml:parse (file-get-contents filename)))

;; @todo Make this work
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
  (let ((args (split-sequence:split-sequence #\/ path)))
    (reduce (lambda (acc arg)
              (cond
                ;; When it's a hash table, get the hash or skip if missing.
                ((hash-table-p acc)
                 (or (gethash arg acc) acc))

                ;; When it's a list, grab the nth value or skip if missing.
                ((listp acc)
                 (or (nth (parse-integer arg :junk-allowed t) acc) acc))

                ;; When it is neither, just return accumulator, we are
                ;; very generous about accepting poor paths/data...
                (t acc)))
            (push object args))))

;;; "af.lib.hashy" goes here. Hacks and glory await!
