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

;;;; Sparse vector and matrix functions.

(in-package :texonomy-util)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct sparse-vector
    (values (make-array 0) :type simple-array)
    (index (make-array 0 :element-type 'fixnum) :type simple-array)
    (len 0 :type fixnum)))

(defun make-sparse-vector-from-vector (vector)
  "Make a sparse vector from a general vector."
  (declare (type vector vector))
  (let ((len (length vector))
	(values ())
	(index ()))
    (loop for i from 0 to (1- len) do
	 (let ((value (aref vector i)))
	   (if (/= 0 value)
	       (progn
		 (push value values)
		 (push i index)))))
    (make-sparse-vector :values (list-to-array (reverse values) 1)
			:index (list-to-array (reverse index) 1)
			:len len)))

(defun make-vector-from-sparse-vector (svec)
  "Make a general vector from a sparse vector."
  (declare (type sparse-vector svec))
  (let* ((len (sparse-vector-len svec))
	 (out (make-array len :initial-element 0))
	 (index (sparse-vector-index svec)))
    (loop for i from 0 to (1- len) do
	 (if (1d-array-member i index)
	     (setf (aref out i) (aref-sparse-vector svec i))))
    out))

(defun aref-sparse-vector (svec index)
  "AREF function of a sparse vector."
  (declare (type sparse-vector svec)
	   (type fixnum index))
  (let ((index-array (sparse-vector-index svec))
	(values (sparse-vector-values svec))
	(len (sparse-vector-len svec)))
    (assert (< index len)
	    (svec index)
	    "Index ~D out of bounds of sparse vector ~A."
	    index
	    svec)
    (loop for i from 0 to (1- (length index-array)) do
	 (if (= index (aref index-array i))
	     (return-from aref-sparse-vector (aref values i))))
    0))

(defun (setf aref-sparse-vector) (value svec index)
  "(SETF AREF) function of a sparse vector."
  (declare (type sparse-vector svec)
	   (type fixnum index))
  (let ((index-array (sparse-vector-index svec))
	(values (sparse-vector-values svec))
	(len (sparse-vector-len svec))
	(helper-list1 ())
	(helper-list2 ()))
    (assert (< index len)
	    (value svec index)
	    "Index ~D out of bounds of sparse vector ~A."
	    index
	    svec)
    (loop for i from 0 to (1- (length index-array)) do
	 (let ((ind (aref index-array i)))
	   ;; There are chances that the last index is smaller than the index
	   ;; of where we wanna change the value...
	   (if (<= index ind)
	       ;; If we meet a slot with non-zero value, substitute it or die.
	       (if (= index ind)
		   (if (/= value 0)
		       (progn
			 (setf (aref values index) value)
			 (return-from aref-sparse-vector value))
		       ;; Remove this slot from values and index.
		       (progn
			 (setf helper-list1
			       (remove-by-position ind (1d-array-to-list values))
			       helper-list2
			       (remove-by-position ind (1d-array-to-list index-array))
			       (sparse-vector-values svec)
			       (list-to-array helper-list1 1)
			       (sparse-vector-index svec)
			       (list-to-array helper-list2 1))
			 (return-from aref-sparse-vector value)))
		   ;; The first time we meet a index greater than our goal.
		   (if (/= value 0)
		       ;; Add a new index and form a new values array.
		       (progn
			 (setf helper-list1
			       (insert (1d-array-to-list values) i value)
			       helper-list2
			       (insert (1d-array-to-list index-array) i index)
			       (sparse-vector-values svec)
			       (list-to-array helper-list1 1)
			       (sparse-vector-index svec)
			       (list-to-array helper-list2 1))
			 (return-from aref-sparse-vector value))
		       ;; Since this is the first time index >= our place,
		       ;; So there's no chance to change anything, we are done.
		       (return-from aref-sparse-vector value))))))
    ;; The biggest index is smaller than ours, append it and the value.
    (setf helper-list1 (append (1d-array-to-list values) (list value))
	  helper-list2 (append (1d-array-to-list index-array) (list index))
	  (sparse-vector-values svec) (list-to-array helper-list1 1)
	  (sparse-vector-index svec) (list-to-array helper-list2 1))
    value))

(defun negate-sparse-vector (svec)
  "Negating function of a sparse vector."
  (declare (type sparse-vector svec))
  (let* ((values (sparse-vector-values svec))
	 (index (sparse-vector-index svec))
	 (out (make-sparse-vector :values (make-array (length values) :initial-element 0)
				  :index (make-array (length index) :initial-element 0)
				  :len (sparse-vector-len svec)))
	 (out-values (sparse-vector-values out))
	 (out-index (sparse-vector-index out)))
    (loop for i from 0 to (1- (length values)) do
	 (setf (aref out-values i) (- (aref values i))
	       (aref out-index i) (aref index i)))
    out))

(defun sparse-vector-+-2 (svec1 svec2)
  "Helper function of general sparse vector addition."
  (declare (type sparse-vector svec1 svec2))
  (assert (= (sparse-vector-len svec1)
	     (sparse-vector-len svec2))
	  (svec1 svec2)
	  "Size mismatch, two vectors of length ~D and ~D."
	  (sparse-vector-len svec1)
	  (sparse-vector-len svec2))
  (let ((len (sparse-vector-len svec1))
	(ilen (length (sparse-vector-index svec2)))
	(value-index (mapcar #'list
			     (1d-array-to-list (sparse-vector-values svec1))
			     (1d-array-to-list (sparse-vector-index svec1)))))
    (flet ((list-second (lst) (mapcar #'second lst))
	   (second-< (lst1 lst2)
	     (if (< (second lst1) (second lst2))
		 t
		 nil))
	   ;; Assuming no repeat item, since we use it only to index array,
	   ;; it will cause no mistakes.
	   (find-nth (item lst)
	     (loop for i from 0 to (1- (length lst)) do
		  (if (= item (nth i lst))
		      (return-from find-nth i)))
	     nil))
      ;; If we have found a slot are nonzero in both vectors,
      ;; add the value of svec2 to the value of svec1,
      ;; if there's a new slot in svec2, push it to the back.
      (loop for i from 0 to (1- ilen) do
	   (let ((item (aref (sparse-vector-index svec2) i))
		 (val (aref (sparse-vector-values svec2) i))
		 (n 0))
	     (if (setf n (find-nth item (list-second value-index)))
		 (incf (first (nth n value-index)) val)
		 ;; Not found, push it to the back.
		 (push (list val item) value-index))))
      (setf value-index (sort value-index #'second-<)))
    (make-sparse-vector :values (list-to-array (mapcar #'first value-index) 1)
			:index (list-to-array (mapcar #'second value-index) 1)
			:len len)))

(defun sparse-vector---2 (svec1 svec2)
  "Helper function of general sparse vector subtraction."
  (declare (type sparse-vector svec1 svec2))
  (sparse-vector-+-2 svec1 (negate-sparse-vector svec2)))

(defun sparse-vector-+ (svec &rest more)
  "Addition function of sparse vectors, from left to right."
  (declare (type sparse-vector svec))
  (reduce #'sparse-vector-+-2 (cons svec more)))

(defun sparse-vector-- (svec &rest more)
  "Subtraction function of sparse vectors, from left to right."
  (declare (type sparse-vector svec))
  (reduce #'sparse-vector---2 (cons svec more)))

(defun matrix-*-sparse-vector (mat svec)
  "Multiplication routine of a general matrix and a sparse vector."
  (declare (type matrix mat)
	   (type sparse-vector svec))
  (assert (= (array-dimension mat 1)
	     (sparse-vector-len svec))
	  (mat svec)
	  "Size mismatch, matrix with ~D columns and vector of length ~D."
	  (array-dimension mat 1)
	  (sparse-vector-len svec))
  (let* ((row (array-dimension mat 0))
	 (out (make-array row :initial-element 0))
	 (index (sparse-vector-index svec))
	 (values (sparse-vector-values svec))
	 (ilen (length index)))
    (loop for i from 0 to (1- row) do
	 (loop for j from 0 to (1- ilen) do
	      (incf (aref out i) (* (aref mat i (aref index j))
				    (aref values j)))))
    out))

(defun sparse-inner-product (svec1 svec2)
  "Inner product function of two sparse vectors."
  (declare (type sparse-vector svec1 svec2))
  (assert (= (sparse-vector-len svec1)
	     (sparse-vector-len svec2))
	  (svec1 svec2)
	  "Size mismatch, two vectors of length ~D and ~D."
	  (sparse-vector-len svec1)
	  (sparse-vector-len svec2))
  (let* ((values1 (sparse-vector-values svec1))
	 (values2 (sparse-vector-values svec2))
	 (index1 (1d-array-to-list (sparse-vector-index svec1)))
	 (index2 (1d-array-to-list (sparse-vector-index svec2)))
	 (index (sort (intersection index1 index2) #'<))
	 (out 0))
    (flet ((find-nth (item lst)
	     (loop for i from 0 to (1- (length lst)) do
		  (if (= (nth i lst) item)
		      (return-from find-nth i)))
	     nil))
      ;; All entries in INDEX will be in INDEX1 and INDEX2, get their
      ;; corresponding index, and their corresponding value can be found
      ;; easily, accumulate their products together then...
      (loop for i from 0 to (1- (length index)) do
	   (let ((i1 (find-nth (nth i index) index1))
		 (i2 (find-nth (nth i index) index2)))
	     (incf out (* (aref values1 i1) (aref values2 i2))))))
    out))

(defun sparse-vector-abs (svec)
  "Sparse vector version of ABS function."
  (declare (type sparse-vector svec))
  (let* ((len (sparse-vector-len svec))
	 (index (sparse-vector-index svec))
	 (index-len (length index))
	 (out (make-sparse-vector :values (make-array index-len :initial-element 0)
				  :index index
				  :len len)))
    (loop for i from 0 to (1- index-len) do
	 (setf (aref (sparse-vector-values out) i)
	       (abs (aref (sparse-vector-values svec) i))))
    out))
