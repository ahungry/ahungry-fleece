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

;;;;; ahungry-fleece.asd

(in-package :cl-user)

(defpackage ahungry-fleece-asd
  (:use :cl :asdf))

(in-package :ahungry-fleece-asd)

(asdf:defsystem #:ahungry-fleece
  :version "0.0.1"
  :description "A JSON library."
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (#:cl-json)
  :serial t
  :components
  (
   (:module "src"
            :components
            ((:file "ahungry-fleece" :depends-on ("af.lib.hashy"))
             (:file "af.lib.hashy" :depends-on ("af.lib.loggy"))
             (:file "af.lib.loggy")))
   )
  )

;;:in-order-to ((test-op (load-op alluring-allegory-test))))
