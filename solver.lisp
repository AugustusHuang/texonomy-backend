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
(defun make-1-to-n-list (n)
  "Helper function, generate a list from 1 to n inclusively."
  (let ((out ()))
    (loop for i from 1 to n do
	 (push i out))
    (reverse out)))

(defun make-1-to-n-vector (n)
  "Helper function, generate a vector from 1 to n inclusively."
  (let ((out (make-array n :initial-element 0)))
    (loop for i from 1 to n do
	 (setf (svref out (1- i)) i))
    out))

(defun sort-index (seq predicate)
  "Sort function with sorted index stored and returned."
  (declare (type sequence seq)
	   (type function predicate))
  (typecase seq
    (list
     (let ((seq-index (mapcar #'list seq (make-1-to-n-list (length seq)))))
       (flet ((first-predicate (lst1 lst2)
		(if (funcall predicate (first lst1) (first lst2))
		    t
		    nil)))
	 (mapcar #'second (sort seq-index #'first-predicate)))))
    (vector
     (let* ((seq-list (:texonomy-util::1d-array-to-list seq))
	    (seq-index (mapcar #'list seq-list (make-1-to-n-list (length seq)))))
       (flet ((first-predicate (lst1 lst2)
		(if (funcall predicate (first lst1) (first lst2))
		    t
		    nil)))
	 (mapcar #'second (sort seq-index #'first-predicate)))))))

;;; Error function, inspired by the Python version of implementation.
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

(defun erfc (vec)
  "Compliment of error function, iterate over the vector."
  (declare (type vector vec))
  (let* ((len (length vec))
	 (out (make-array len :initial-element 0))
	 (erf-vec (erf vec)))
    (loop for i from 0 to (1- len) do
	 (setf (aref out i) (1- (aref erf-vec i))))
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
  (let* ((abs-vec (vector-abs vec))
	 (len (length vec))
	 (sorted-vec (sort vec #'<))
	 (sort-index (sort-index vec #'<))
	 (pobs (erfc (elementwise-/ sorted-vec (sqrt 2)))))
    (let* ((n (make-1-to-n-vector len))
	   (pnull (elememtwise-/ n len))
	   (maximum 0))
      (loop for i from 0 to (1- (length pobs)) do
	 ;; pobs has the same length as sorted-vec,
	 ;; and pnull has the same length as vec.
	   (let ((good (<= (aref probs (- (1- n) i)) (* param (aref pnull i)))))
	     (if (and good (> (aref n i) maximum))
		 (setf maximum (aref n i)))))
      (if (/= maximum 0)
	  ;; We've found some GOOD non-nil.
	  (aref abs-vec (nth (- (1+ len) maximum) sort-index))
	  ;; All GOODs are nil, return trivial value.
	  (+ 0.01 (vector-max abs-vec))))))

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

;;; Some main algorithms solving L_1 minimization problems, the core of CS.
;;; Those functions are directly transcripted into Lisp, originally appeared
;;; in SparseLab, implemented by D. Donoho, V. Stodden, Y. Tsaig, I. Drori and
;;; other contributors, I appreciate them.
(defun stagewise-omp (matrix vec &key (thresh #'fdrthresh) (param 0.5) (iter 10) (err 1e-5))
  "Stagewise Orthogonal Matching Pursuit algorithm, get an approximating solution to L_1 minimization problem."
  (declare (type matrix matrix)
	   ;; VEC will be a vector, since it's not necessarily be sparse.
	   (type vector vector))
  (assert (= (array-dimension matrix 0)
	     (length vec))
	  (matrix vec thresh param iter err)
	  "Size mismatch, matrix of size ~D-by-~D but vector of length ~D."
	  (array-dimension matrix 0)
	  (array-dimension matrix 1)
	  (length vec))
  (let* ((len (length vec))
	 (col (array-dimension matrix 1))
	 ;; Initially the residual will be VEC.
	 (residual vec)
	 (vnorm (norm vec))
	 (i-full (make-1-to-n-vector col))
	 (i-now ())
	 (active ())
	 (j-active ())
	 (x-i (make-array col :initial-element 0))
	 ;; Now the output sparse vector is zero-sparse vector.
	 (out (make-sparse-vector :len col)))
    ;; Iterate ITER times and output the result in sparse vector form.
    (loop for i from 0 to (1- iter) do
	 (let* ((corr (elementwise-/ (vector-* (elementwise-* (sqrt n)
							      (transpose matrix))
					       residual)
				     (norm residual)))
		(thr (funcall thresh corr param)))
	   (setf i-now (1d-array-to-list (hardthresh (vector-abs corr) thr))
		 j-active (union active i-now))
	   (if (= (length j-active) (length active))
	       ;; Maybe we shall use some more gentle way?
	       (go done))
	   (setf active j-active
		 x-i (solve (mask-matrix matrix active) vec)
		 residual (elementwise-- (vector-* (mask-matrix matrix active)
						   x-i)))
	   (if (<= (norm residual) (* err vnorm))
	       (go done))))
    ;; No way to assign a list to a vector... FIXME
    (setf (sparse-vector-values out) x-i
	  (sparse-vector-index out) active)
    out))
