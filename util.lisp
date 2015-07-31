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

;;; Shall we make every function runnable with argument of type SPARSE-VECTOR?
(defun vector-abs (vec)
  "Vector version of ABS absolute value function."
  (declare (type vector vec))
  (let* ((len (length vec))
	 (out (make-array len :initial-element 0)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (abs (aref vec i))))
    out))

(defun vector-max (vec)
  "Vector version of MAX function, scan over the vector and find the maximum."
  (declare (type vector vec))
  ;; TODO: We should choose a function, or write it by ourselves?
  )

(defun norm (vec)
  "Compute a vector's norm, input can be of type VECTOR or SPARSE-VECTOR."
  (declare (type (or vector sparse-vector) vec))
  (typecase vec
    (vector
     (let ((len (length vec))
	   (out 0))
       (loop for i from 0 to (1- len) do
	    (incf out (* (aref vec i) (aref vec i))))
       out))
    (sparse-vector
     (let* ((values (sparse-vector-values vec))
	    (vlen (length values))
	    (out 0))
       (loop for i from 0 to (1- vlen) do
	    ;; We only concern with non-zero values.
	    (incf out (* (aref values i) (aref values i))))
       out))))

;;; Only sparse vector is needed.
;;; Simply switch between sparse vector and normal vector.
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

(defmacro doseq ((n seq) &rest body)
  "Sequence version of DOXXX macros."
  `(map nil #'(lambda (,n) ,@body) ,seq))

(defmacro dovec ((var vector) &body body)
  "Vector version of DOXXX macros."
  `(map nil #'(lambda (,var) ,@body) ,vector))

(defun matrix-invert (matrix)
  "Get the inversion of a square matrix."
  (declare (type square-matrix matrix))
  (let* ((dim (array-dimension matrix 0))
	 (l (make-array dim :initial-element 0))
	 (m (make-array dim :initial-element 0))
	 (temp 0)
	 (det 1)
	 (out (make-array `(,dim ,dim) :initial-element 0)))
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
	 (out (make-array `(,row ,col) :initial-element 0)))
    (loop for i from 0 to (1- row) do
	 (loop for j from 0 to (1- col) do
	      (setf (aref out i j) (conjugate (aref matrix i j)))))
    out))

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

;;; The new sparse vector will have index array the intersection of two
;;; original arrays, so the implementation maybe easier.
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
