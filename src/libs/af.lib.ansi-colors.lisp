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

(defpackage af.lib.ansi-colors
  (:use :cl
        :split-sequence)
  (:export
   :colorize
   :with-color
   :*colorize-p*
   ))

(in-package #:af.lib.ansi-colors)

(defparameter *colors*
  (list :black "0;30"
        :blue "0;34"
        :green "0;32"
        :cyan "0;36"
        :red "0;31"
        :purple "0;35"
        :brown "0;33"
        :light-gray "0;37"
        :dark-gray "1;30"
        :light-blue "1;34"
        :light-green "1;32"
        :light-cyan "1;36"
        :light-red "1;31"
        :light-purple "1;35"
        :yellow "1;33"
        :white "1;37"))

(defparameter *colorize-p* t)

(defun colorize (color)
  "Set the active color of the terminal."
  (when *colorize-p*
    (format t "~c[~am"
            #\Esc
            (or (getf *colors* color)
                (getf *colors* :white)))))

(defmacro with-color (color &rest body)
  "Activate COLOR and execute BODY (while reseting back to base color)."
  `(progn
     (colorize ,color)
     ,@body
     (colorize :light-gray)))

;;; "af.lib.hashy" goes here. Hacks and glory await!
