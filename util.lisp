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

;;;; General utilities.

(in-package :texonomy-util)

(deftype matrix (&optional type row col)
  "Matrix type."
  `(array ,type (,row ,col)))

(deftype square-matrix (&optional type dim)
  "Square matrix type."
  `(array ,type (,dim ,dim)))

;;; TODO: How if the list can't form an array?
;;; Shall we append zeros or do something?
(defun list-to-array (list depth)
  "Make an array from a given list."
  (make-array (list-dimensions list depth) :initial-contents list))

(defun 1d-array-to-list (array)
  "Make a list from an 1-dimensional array."
  (loop for i below (array-dimension array 0) collect (aref array i)))

(defun 2d-array-to-vector (array)
  "Helper function, cast a #2A((x) (y) (z))-like pseudo-vector to a normal one."
  ;; ARRAY is a matrix indeed.
  (declare (type matrix array))
  (let* ((len (array-dimension array 0))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (aref array i 0)))
    out))

(defun vector-to-2d-array (vec)
  "Helper function, cast a #(x y z)-like vector to a #2A((x) (y) (z)) matrix."
  (declare (type vector vec))
  (let* ((len (length vec))
	 (out (make-array `(,len ,1) :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i 0) (aref vec i)))
    out))

;;; Even the list can't form an array, like '((1 2) (3))
;;; We can still get the answer of '((1 2) (3 whatever)).
(defun list-dimensions (list depth)
  "List counterpart of function ARRAY-DIMENSIONS."
  (loop repeat depth
       collect (length list)
       do (setf list (car list))))

(defun 1d-array-member (item vec &key (test #'=))
  "Vector version of MEMBER function."
  (member item (1d-array-to-list vec) :test test))

(defun remove-by-position (index list)
  "REMOVE function by index."
  (loop for i in list
     for j from 1 unless (= j (1+ index)) collect i))

(defun insert (list index value)
  "Insert VALUE before the INDEXth element in the list."
  (let ((dis (1- index)))
    (push value (cdr (nthcdr dis list)))
    list))

(defun vector-abs (vec)
  "Vector version of ABS absolute value function."
  (declare (type vector vec))
  (let* ((len (length vec))
	 (out (make-array len :initial-element 0.0d0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (abs (aref vec i))))
    out))

(defun vector-max (vec)
  "Vector version of MAX function, scan over the vector and find the maximum."
  (declare (type vector vec))
  (let ((len (length vec))
	(out 0))
    (loop for i from 0 to (1- len) do
	 (let ((slot (aref vec i)))
	   (if (> slot out)
	       (setf out slot))))
    out))

(defun norm (vec)
  "Compute a vector's norm, input can be of type VECTOR or SPARSE-VECTOR."
  (declare (type (or vector sparse-vector) vec))
  (typecase vec
    (vector
     (let ((len (length vec)))
       (sqrt (loop for i from 0 to (1- len) sum (expt (aref vec i) 2)))))
    (sparse-vector
     (let* ((values (sparse-vector-values vec))
	    (vlen (length values)))
       (sqrt (loop for i from 0 to (1- vlen) sum (expt (aref values i) 2)))))))

(defun dot-product (vec1 vec2)
  "Dot product of two vectors, get a scalar value."
  (declare (type vector vec1 vec2))
  (assert (= (length vec1) (length vec2))
	  (vec1 vec2)
	  "Size mismatch, two vectors of length ~D and ~D."
	  (length vec1)
	  (length vec2))
  (let ((len (length vec1)))
    (loop for i from 0 to (1- len) sum (* (aref vec1 i) (aref vec2 i)))))

(defun tensor-product (vec1 vec2)
  "Tensor product of two vectors, get a matrix value."
  (declare (type vector vec1 vec2))
  (let* ((len1 (length vec1))
	 (len2 (length vec2))
	 (out (make-array `(,len1 ,len2) :initial-element 0.0d0)))
    (loop for i from 0 to (1- len1) do
	 (loop for j from 0 to (1- len2) do
	      (setf (aref out i j) (* (aref vec1 i) (aref vec2 j)))))
    out))

(defun random-array (length start end)
  "Get an array of length LENGTH, numbers in array ranging from START to END."
  (declare (type fixnum length start end)
	   (optimize (speed 3) (safety 0)))
  (let ((out (make-array length :element-type 'fixnum))
	(rand (the fixnum (- end start))))
    (declare (type fixnum rand))
    (dotimes (i length)
      (setf (aref out i) (+ (the fixnum (random rand)) start)))
    out))

(defun random-matrix (row col start end)
  "Get a matrix of size ROW-by-COL, ranging from START to END."
  (declare (type fixnum row col start end))
  (let ((out (make-array `(,row ,col) :element-type 'fixnum))
	(rand (the fixnum (- end start))))
    (declare (type fixnum rand))
    (loop for i from 0 to (1- row) do
	 (loop for j from 0 to (1- col) do
	      (setf (aref out i j) (+ (the fixnum (random rand)) start))))
    out))

;;; FIXME: This function only works when we are handling sparse vector,
;;; make it more general.
(defun ignore-trailing-zero (array)
  "Remove an array's trailing 0s, assuming 0 will only appear on the tail."
  (declare (type vector array))
  (let ((len 0))
    (dovec (item array)
      (if (/= item 0)
	  (incf len)
	  (return-from ignore-trailing-zero (adjust-array array len))))))

(defmacro with-gensyms ((&rest names) &body body)
  "Expand into code that binds all names to symbols generated by GENSYM."
  `(let ,(loop for n in names collect `(,n (gensym (format nil "~a-" ',n))))
     ,@body))

(defmacro dovec ((var vector) &body body)
  "Vector version of DOXXX macros."
  `(map nil #'(lambda (,var) ,@body) ,vector))

(defun matrix-invert (matrix)
  "Get the inversion of a square matrix."
  (declare (type square-matrix matrix))
  (let* ((dim (array-dimension matrix 0))
	 (l (make-array dim :initial-element 0.0d0))
	 (m (make-array dim :initial-element 0.0d0))
	 (temp 0)
	 (det 1)
	 (out (make-array `(,dim ,dim) :initial-element 0.0d0)))
    (when (not (equal matrix out))
      (loop for i from 0 to (1- dim) do
	   (loop for j from 0 to (1- dim) do
		(setf (aref out i j) (aref matrix i j)))))
    (do ((k 0 (1+ k))
	 (maximum 0)
	 (1/max 0))
	((>= k dim))
      (setf (svref l k) k
	    (svref m k) k
	    maximum (aref out k k))
      (loop for i from k to (1- dim) do
	   (loop for j from k to (1- dim) do
		(when (> (abs (aref out i j)) (abs maximum))
		  (setf maximum (aref out i j)
			(svref l k) i
			(svref m k) j))))

      ;; Interchange rows with pivot.
      (if (> (svref l k) k)
	  (do ((j 0 (1+ j))
	       (i (svref l k)))
	      ((>= j dim))
	    (setf temp (- (aref out k j))
		  (aref out k j) (aref out i j)
		  (aref out i j) temp)))
      (if (> (svref m k) k)
	  (do ((i 0 (1+ i))
	       (j (svref m k)))
	      ((>= i dim))
	    (setf temp (- (aref out i k))
		  (aref out i k) (aref out i j)
		  (aref out i j) temp)))
      (if (equalp maximum 0)
	  (return-from matrix-invert 0))
      (setf 1/max (/ 1 maximum))
      (loop for i from 0 to (1- dim) do
	   (if (not (= i k))
	       (setf (aref out i k)
		     (* (aref out i k) (- 1/max)))))

      ;; Then reduce it.
      (loop for i from 0 to (1- dim) do
	   (when (not (= i k))
	     (setf temp (aref out i k))
	     (loop for j from 0 to (1- dim) do
		  (if (not (= j k))
		      (incf (aref out i j)
			    (* temp (aref out k j)))))))

      ;; Divide by pivot row.
      (loop for j from 0 to (1- dim) do
	   (if (not (= j k))
	       (setf (aref out k j)
		     (* (aref out k j) 1/max))))
      (setf det (* det maximum)
	    (aref out k k) 1/max))

    ;; And finally we are nearly done...
    (loop for n from (1- dim) downto 0 do
	 (if (> (svref l n) n)
	     (do ((j 0 (1+ j))
		  (i (svref l n)))
		 ((>= j dim))
	       (setf temp (aref out j n)
		     (aref out j n) (- (aref out j i))
		     (aref out j i) temp)))
	 (if (> (svref m n) n)
	     (do ((i 0 (1+ i))
		  (j (svref m n)))
		 ((>= i dim))
	       (setf temp (aref out n i)
		     (aref out n i) (- (aref out j i))
		     (aref out j i) temp))))
    ;; Return the inverted matrix and its determinant.
    (values out det)))

(defun matrix-conjugate (matrix)
  "Get the complex conjugation of a matrix"
  (declare (type matrix matrix))
  (let* ((row (array-dimension matrix 0))
	 (col (array-dimension matrix 1))
	 (out (make-array `(,row ,col) :initial-element 0.0d0)))
    (loop for i from 0 to (1- row) do
	 (loop for j from 0 to (1- col) do
	      (setf (aref out i j) (conjugate (aref matrix i j)))))
    out))

(defun matrix-transpose (matrix)
  "Get the transposition matrix of a matrix."
  (declare (type matrix matrix))
  (let* ((row (array-dimension matrix 0))
	 (col (array-dimension matrix 1))
	 (out (make-array `(,col ,row) :initial-element 0.0d0)))
    (loop for i from 0 to (1- row) do
	 (loop for j from 0 to (1- col) do
	      (setf (aref out j i)
		    (aref matrix i j))))
    out))

(defun matrix-*-vector (matrix vec)
  "Multiplication routine of a general matrix and a general vector."
  (declare (type matrix matrix)
	   (type vector vec))
  (assert (= (array-dimension matrix 1)
	     (length vec))
	  (matrix vec)
	  "Size mismatch, matrix with ~D columns and vector of length ~D."
	  (array-dimension matrix 1)
	  (length vec))
  (let ((row (array-dimension matrix 0))
	(col (array-dimension matrix 1)))
    (let ((out (make-array row :initial-element 0.0d0)))
      (loop for i from 0 to (1- row) do
	   (loop for j from 0 to (1- col) do
		(incf (aref out i) (* (aref matrix i j) (aref vec j)))))
      out)))

(defun identity-matrix (n)
  "Get an N-by-N identity matrix."
  (declare (type fixnum n))
  (let ((mat (make-array `(,n ,n) :initial-element 0)))
    (loop for i from 0 to (1- n) do
	 (setf (aref mat i i) 1))
    mat))

;;; FIXME: Need assertion or not?
(defun element-wise-general (func array1 array2)
  "Kernel function of matrix element-wise arithmetic functions."
  (typecase array1
    (vector
     (let* ((len (length array1))
	    (out (make-array len :initial-element 0.0d0)))
       (loop for i from 0 to (1- len) do
	    (setf (aref out i) (funcall func
					(aref array1 i)
					(aref array2 i))))
       out))
    (matrix
     (let* ((len (array-total-size array1))
	    (row (array-dimension array1 0))
	    (col (array-dimension array1 1))
	    (out (make-array `(,row ,col) :initial-element 0.0d0)))
       (loop for i from 0 to (1- len) do
	    (setf (row-major-aref out i)
		  (funcall func
			   (row-major-aref array1 i)
			   (row-major-aref array2 i))))
       out))))

(defun m+ (array1 array2) (element-wise-general #'+ array1 array2))
(defun m- (array1 array2) (element-wise-general #'- array1 array2))
(defun m* (array1 array2) (element-wise-general #'* array1 array2))
(defun m/ (array1 array2) (element-wise-general #'/ array1 array2))

(defun element-wise-scalar (func array c)
  "Kernel function of array and scalar arithmetic functions."
  (typecase array
    (vector
     (let* ((len (length array))
	    (out (make-array len :initial-element 0.0d0)))
       (loop for i from 0 to (1- len) do
	    (setf (aref out i) (funcall func
					(aref array i)
					c)))
       out))
    (matrix
     (let* ((len (array-total-size array))
	    (row (array-dimension array 0))
	    (col (array-dimension array 1))
	    (out (make-array `(,row ,col) :initial-element 0.0d0)))
       (loop for i from 0 to (1- len) do
	    (setf (row-major-aref out i)
		  (funcall func
			   (row-major-aref array i)
			   c)))
       out))))

(defun .+ (array c) (element-wise-scalar #'+ array c))
(defun .- (array c) (element-wise-scalar #'- array c))
(defun .* (array c) (element-wise-scalar #'* array c))
(defun ./ (array c) (element-wise-scalar #'/ array c))

(defun matrix-range (matrix rstart rend cstart cend)
  "Get the ranged matrix in a matrix."
  (let* ((rdis (1+ (- rend rstart)))
	 (cdis (1+ (- cend cstart)))
	 (out (make-array `(,rdis ,cdis) :initial-element 0.0d0)))
    (loop for i from 0 to (1- rdis) do
	 (loop for j from 0 to (1- cdis) do
	      (setf (aref out i j) (aref matrix (+ rstart i) (+ cstart j)))))
    out))

(declare (inline mrow))
(defun mrow (matrix n)
  (let ((col (array-dimension matrix 1)))
    (matrix-range matrix n n 0 (1- col))))

(declare (inline mcol))
(defun mcol (matrix n)
  (let ((row (array-dimension matrix 0)))
    (matrix-range matrix 0 (1- row) n n)))

;;; FIXME: Could be faster, we are doing useless works when we firstly set
;;; matrix1's value and then change them!
(defun matrix-embed (matrix1 matrix2 row col)
  "Embed a smaller matrix into a bigger one."
  (let* ((row1 (array-dimension matrix1 0))
	 (col1 (array-dimension matrix1 1))
	 (row2 (array-dimension matrix2 0))
	 (col2 (array-dimension matrix2 1))
	 (out (make-array `(,row1 ,col1) :initial-element 0.0d0)))
    (loop for i from 0 to (1- row1) do
	 (loop for j from 0 to (1- col1) do
	      (setf (aref out i j) (aref matrix1 i j))))
    (loop for i from 0 to (1- row2) do
	 (loop for j from 0 to (1- col2) do
	      (setf (aref out (+ row i) (+ col j)) (aref matrix2 i j))))
    out))

(defun unit-vector (length)
  (let ((out (make-array length :initial-element 0.0d0)))
    (setf (aref out 0) 1.0d0)
    out))

;;; FIXME: Speed up!
(defun matrix-multiply (matrix1 matrix2)
  "Normal matrix multiplication function."
  (assert (= (array-dimension matrix1 1)
	     (array-dimension matrix2 0))
	  (matrix1 matrix2)
	  "Size mismatch, multiplying two matrices of size ~D-by-~D and ~D-by-~D."
	  (array-dimension matrix1 0)
	  (array-dimension matrix1 1)
	  (array-dimension matrix2 0)
	  (array-dimension matrix2 1))
  (let* ((row1 (array-dimension matrix1 0))
	 (col1 (array-dimension matrix1 1))
	 (col2 (array-dimension matrix2 1))
	 (out (make-array `(,row1 ,col2) :initial-element 0.0d0)))
    (loop for i from 0 to (1- row1) do
	 (loop for j from 0 to (1- col2) do
	      (setf (aref out i j)
		    (loop for k from 0 to (1- col1)
		       sum (* (aref matrix1 i k) (aref matrix2 k j))))))
    out))

;;; QR functions
(defun householder (vec)
  (declare (type vector vec))
  (let* ((len (length vec))
	 (sign (signum (aref vec 0)))
	 (unit (unit-vector len))
	 (u (m+ vec (.* unit (* sign (norm vec)))))
	 (v (./ u (aref u 0)))
	 (beta (/ 2 (dot-product v v))))
    (m- (identity-matrix len)
	(.* (tensor-product v v) beta))))

(defun qr-decomposition (matrix)
  "QR decomposition function, return a matrix's QR decomposition matrices."
  (declare (type matrix matrix))
  (let ((row (array-dimension matrix 0))
	(col (array-dimension matrix 1)))
    (let ((q (identity-matrix row))
	  (r matrix))
      (loop for i from 0 to (if (= row col) (- col 2) (1- col)) do
	   (let* ((sub (matrix-range r i (1- row) i (1- col)))
		  (first-col (2d-array-to-vector (mcol sub 0)))
		  (house (matrix-embed (identity-matrix row)
				       (householder first-col)
				       i
				       i)))
	     (setf q (matrix-multiply q house)
		   r (matrix-multiply house r))))
      (values q r))))

(defun qr-solve (matrix vec)
  "Solve the problem of system of linear functions Ax = b, Using QR solver."
  (declare (type matrix matrix)
	   (type vector vec))
  (assert (= (array-dimension matrix 0)
	     (length vec))
	  (matrix vec)
	  "Size mismatch, matrix of size ~D-by-~D but vector of length ~D."
	  (array-dimension matrix 0)
	  (array-dimension matrix 1)
	  (length vec))
  (multiple-value-bind (q r) (qr-decomposition matrix)
    (let ((col (array-dimension r 1)))
      (upper-triangular-solve (matrix-range r 0 (1- col) 0 (1- col))
			      (matrix-range (vector-to-2d-array
					     (matrix-*-vector
					      (matrix-transpose q)
					      vec))
					     0 (1- col) 0 0)))))

(defun upper-triangular-solve (matrix vec)
  "Solve the problem of system of linear functions Ax = b, where A is an upper triangular matrix."
  (declare (type matrix matrix vec))
  (let* ((col (array-dimension matrix 1))
	 (out (make-array col :initial-element 0.0d0)))
    (loop for i from (1- col) downto 0 do
	 (setf (aref out i) (/ (- (aref vec i 0)
				  (loop for j from (1+ i) to (1- col)
				     sum (* (aref matrix i j)
					    (aref out j))))
			       (aref matrix i i))))
    out))

(defun mask-matrix (matrix mask)
  "Apply a column mask to the matrix, make masked columns all zero."
  (declare (type matrix matrix)
	   (type list mask))
  (let ((row (array-dimension matrix 0))
	(col (array-dimension matrix 1)))
    ;; Find the complement vector of MASK, copy only those columns.
    (let ((out (make-array `(,row ,col) :initial-element 0.0d0)))
      (loop for j from 0 to (1- col) do
	   (if (member j mask)
	       ;; do nothing
	       ()
	       (loop for i from 0 to (1- row) do
		    (setf (aref out i j) (aref matrix i j)))))
      out)))

