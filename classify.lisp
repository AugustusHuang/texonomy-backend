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

(in-package :texonomy-recognition)

;;; TODO:
;;; Score system, if implemented in the backend, should make tuples of
;;; score-symbols.

(defvar *current-training-set* nil)

;;; TODO: maybe metadata could be larger? Like a list of symbol-score pairs?
;;; A SAMPLE object will be:
;;; DATA: 16 ratios/doubles
;;; META: a list of symbol-score assoc-list, our default training sample will
;;; be '((SYMBOL . 1))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct sample
    (data (make-array 16) :type simple-array)
    (meta nil :type list)))

(defun load-training-samples (&optional file)
  "load the training samples in json format, if FILE unspecified, use the default sample set."
  )

;;; Select the metadata type in TODO above...
(defun incf-training-samples (sample)
  "Add the SAMPLE into the training sample set."
  )

;;; Algorithm:
;;; 1. Get the surrounding rectangle. in RECOGNITION or in front-end.
;;; 2. Resize and re-sample the training samples, in GRAPHIC.
;;; 3. Normalize all training samples and make them a grand matrix, in UTIL.
;;; 4. Solve L_1 minimization get the solution and residuals, in CS.
;;; 5. Get the argmin of residuals, and it's the output, in CS.
(defun cs-classify (vec)
  "Main classification function, accept a test sample in vector form."
  )

(defun get-score (vec)
  "Score system, read the VEC and generate the scores corresponding symbols."
  )
