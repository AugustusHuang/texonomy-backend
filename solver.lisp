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

;;;; Compressed Sensing solver routines.

(in-package :texonomy-cs)

;;; Some helper functions... Perhaps we shall move them into util package?
(defun sort-index (seq predicate)
  )

(defun erf (vec)
  )

(defun elementwise-* (vec1 vec2)
  "Element-wise multiplication of two vectors, if one of them is a scalar, expand it to a constant vector."
  (declare (type (or number vector) vec1 vec2))
  (assert (= (length vec1)
	     (length vec2))
	  (vec1 vec2)
	  "Size mismatch, two vectors of size ~D and ~D."
	  (length vec1)
	  (length vec2))
  (let* ((len (length vec1))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (* (aref vec1 i) (aref vec2 i))))
    out))

(defun elementwise-/ (vec1 vec2)
  "Element-wise division of two vectors, if one of them is a scalar, expand it to a constant vector."
  (declare (type (or number vector) vec1 vec2))
  (assert (= (length vec1)
	     (length vec2))
	  (vec1 vec2)
	  "Size mismatch, two vectors of size ~D of ~D."
	  (length vec1)
	  (length vec2))
  (let* ((len (length vec1))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (/ (aref vec1 i) (aref vec2 i))))
    out))

;;; Transcript into Lisp...
(defun fdrthresh (vec param)
  "Get the fdr threshold of VEC with PARAM."
  (declare (type vector vec)
	   (type real param))
  )

(defun hardthresh (vec param)
  "Apply the hard threshold PARAM to VEC."
  (declare (type vector vec)
	   (type real param))
  (let* ((len (length vec))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (let ((ith (aref vec i)))
	   (if (> ith param)
	       (setf (aref out i) ith))))
    out))

(defun softthresh (vec param)
  "Apply the soft threshold PARAM to VEC."
  (declare (type vector vec)
	   (type real param))
  )
