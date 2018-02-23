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
  :version "0.3.0"
  :description "A general utility library of convenience functions and features."
  :author "Matthew Carter <m@ahungry.com>"
  :license "GPLv3"
  :depends-on (#:cl-json
               #:cl-yaml
               #:md5
               #:chipz
               #:archive
               #:split-sequence)
  :serial t
  :components
  (
   ;; Contrib module from SBCL source (Public Domain)
   (:module "af.contrib.compiler"
            :pathname "contrib/sb-rotate-byte"
            :components
            ((:file "package")
             (:file "compiler" :depends-on ("package"))))

   ;; Contrib module from SBCL source (Public Domain)
   (:module "af.contrib.vm"
            :depends-on ("af.contrib.compiler")
            :pathname "contrib/sb-rotate-byte"
            :components
            ((:file "arm-vm" :if-feature :arm)
             (:file "arm64-vm" :if-feature :arm64)
             (:file "x86-vm" :if-feature :x86)
             (:file "x86-64-vm" :if-feature :x86-64)
             (:file "ppc-vm" :if-feature :ppc)))

   ;; Contrib module from SBCL source (Public Domain)
   (:module "af.contrib.sb-rotate-byte"
            :depends-on ("af.contrib.vm")
            :pathname "contrib/sb-rotate-byte"
            :components
            ((:file "rotate-byte")))

   ;; Contrib module from SBCL source (Public Domain)
   (:module "af.contrib.sb-md5"
            :depends-on ("af.contrib.sb-rotate-byte")
            :pathname "contrib/sb-md5"
            :components
            ((:file "md5")))

   ;; Contrib module from SBCL source (Public Domain)
   (:module "af.contrib.sb-cover"
            :depends-on ("af.contrib.sb-md5")
            :pathname "contrib/sb-cover"
            :components
            ((:file "cover")))

   ;; The lib modules
   (:module "libs"
            :depends-on ("af.contrib.sb-cover")
            :pathname "src/libs"
            :components
            ((:file "af.lib.coverage"
                    :depends-on ("af.lib.hashy"
                                 "af.lib.ansi-colors"
                                 ))
             (:file "af.lib.hashy"
                    :depends-on ("af.lib.loggy"
                                 "af.lib.io"))
             (:file "af.lib.testy"
                    :depends-on ("af.lib.ansi-colors"
                                 "af.lib.coverage"))
             (:file "af.lib.loggy"
                    :depends-on ("af.lib.io"))
             (:file "af.lib.ansi-colors")
             (:file "af.lib.clone"
                    :depends-on ("af.lib.io"))
             (:file "af.lib.io")))

   ;; The main module
   (:module "ahungry-fleece"
            :pathname "src"
            :depends-on ("libs")
            :components
            ((:file "ahungry-fleece")))

   ;; The testing module (@todo Look at Injection for separate asd for testing)...
   (:module "t"
            :pathname "t"
            :depends-on ("libs" "ahungry-fleece")
            :components
            ((:file "af.run.tests")))
   )
  )

;;:in-order-to ((test-op (load-op alluring-allegory-test))))
