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

;;; Error function, inspired by Python implementation.
(defun erf (vec)
  "Error function, iterating through the vector."
  (declare (type vector vec))
  (let* ((len (length vec))
	 (a1 0.254829592)
	 (a2 -0.284496736)
	 (a3 1.421413741)
	 (a4 -1.453152027)
	 (a5 1.061405429)
	 (p 0.3275911)
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (let* ((sign (if (>= (aref vec i) 0) 1 -1))
		(abs-value (abs (aref vec i)))
		(temp (/ 1.0 (+ 1.0 (* p abs-value)))))
	   (setf (aref out i) (* sign
				 (- 1.0 (* temp
					   (exp (* abs-value (- abs-value)))
					   (+ a1 (* temp (+ a2 (* temp (+ a3 (* temp (+ a4 (* temp a5))))))))))))))
    out))

(defun elementwise-* (v1 v2)
  "Element-wise multiplication of two vectors, if one of them is a scalar, expand it to a constant vector."
  (declare (type (or number vector) v1 v2))
  (typecase v1
    (vector
     (typecase v2
       (vector
	(assert (= (length v1) (length v2))
		(v1 v2)
		"Size mismatch, two vectors of size ~D and ~D."
		(length v1)
		(length v2))
	(let* ((len (length v1))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (* (aref v1 i) (aref v2 i))))
	  out))
       (number
	(let* ((len (length v1))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (* (aref v1 i) v2)))
	  out))))
    (number
     (typecase v2
       (vector
	(let* ((len (length v2))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (* (aref v2 i) v1)))
	  out))
       (number
	(* v1 v2))))))

(defun elementwise-/ (v1 v2)
  "Element-wise division of two vectors, if one of them is a scalar, expand it to a constant vector."
  (declare (type (or number vector) v1 v2))
  (typecase v1
    (vector
     (typecase v2
       (vector
	(assert (= (length v1) (length v2))
		(v1 v2)
		"Size mismatch, two vectors of size ~D and ~D."
		(length v1)
		(length v2))
	(let* ((len (length v1))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (/ (aref v1 i) (aref v2 i))))
	  out))
       (number
	(let* ((len (length v1))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (/ (aref v1 i) v2)))
	  out))))
    (number
     (typecase v2
       (vector
	(let* ((len (length v2))
	       (out (make-array len :initial-element 0)))
	  (loop for i from 0 to (1- len) do
	       (setf (aref out i) (/ v1 (aref v2 i))))
	  out))
       (number
	(/ v1 v2))))))

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
  (let* ((len (length vec))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (let* ((sign (if (>= (aref vec i) 0) 1 -1))
		(abs-value (abs (aref vec i)))
		(temp (- abs-value param)))
	   (set (aref out i) (* sign (/ (+ temp (abs temp)) 2)))))
    out))
