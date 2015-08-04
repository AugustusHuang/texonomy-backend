;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

;;;; General package information.
(in-package :cl-user)

;;; General utilites package, will contain basic type and routines.
(defpackage :texonomy-util
  (:nicknames :tex-util)
  (:use :cl)
  ;; We have to export accessors, constructor, type predicate and the name of
  ;; the structure to export them as a whole...
  (:export
   ;; sparse vector routines...
   :sparse-vector
   :sparse-vector-p
   :make-sparse-vector
   :sparse-vector-values
   :sparse-vector-index
   :sparse-vector-len
   :make-sparse-vector-from-vector
   :make-vector-from-sparse-vector
   :aref-sparse-vector
   :negate-sparse-vector
   :sparse-vector-+
   :sparse-vector--
   :matrix-*-sparse-vector
   :sparse-inner-product
   :sparse-vector-abs
   ;; general routines...
   :list-to-array
   :1d-array-to-list
   :2d-array-to-vector
   :vector-to-2d-array
   :random-array
   :random-matrix
   :vector-abs
   :vector-max
   :norm
   :with-gensyms
   :dovec
   :matrix-invert
   :matrix-conjugate
   :matrix-transpose
   :matrix-*-vector
   :identity-matrix
   :matrix-multiply
   :m+
   :m-
   :m*
   :m/
   :.+
   :.-
   :.*
   :./
   :qr-solve
   :mask-matrix))

;;; Compressed sensing package, will contain everything about the underlying
;;; core tech CS.
(defpackage :texonomy-cs
  (:nicknames :tex-cs)
  (:use :cl
	:texonomy-util)
  (:export
   :erf
   :erfc
   :fdrthresh
   :hardthresh
   :softthresh
   :stagewise-omp))

;;; Core recognition package, will contain interface and wrappers of the
;;; front-end, and a recognition system based on CS.
(defpackage :texonomy-recognition
  (:nicknames :tex-rec)
  (:use :cl
	:texonomy-util
	:texonomy-cs
	:texonomy-graphic)
  (:export))

;;; Core graphic manipulation package, will contain all things do with tex
;;; font/symbol graph, like resize, resample, random sample etc.
(defpackage :texonomy-graphic
  (:nicknames :tex-graph)
  (:use :cl
	:texonomy-util)
  (:export))

;;; if we want to realise server on Lisp level, it will be present,
;;; but if we want to have a native-C socket level instead, it will
;;; never be here.
(defpackage :texonomy-server
  (:nicknames :tex-server)
  (:use :cl
	:texonomy-util)
  (:export))
