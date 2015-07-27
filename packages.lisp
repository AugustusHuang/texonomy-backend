;;;; General package information

;;; General utilites package, will contain basic type and routines.
(defpackage :texonomy-util
  (:nicknames :tex-util)
  (:use :cl)
  (:export))

;;; Compressed sensing package, will contain everything about the underlying
;;; core tech CS.
(defpackage :texonomy-cs
  (:nicknames :tex-cs)
  (:use :cl
	:texonomy-util)
  (:export))

;;; Core recognition package, will contain interface and wrappers of the
;;; front-end, and a recognition system based on CS.
(defpackage :texonomy-recognition
  (:nicknames :tex-rec)
  (:use :cl
	:texonomy-util
	:texonomy-cs)
  (:export))

;;; OPTIONAL: Server package,
;;; if we want to realise server on Lisp level, it will be present,
;;; but if we want to have a native-C socket level instead, it will
;;; never be here.
(defpackage :texonomy-server
  (:nicknames :tex-server)
  (:use :cl
	:texonomy-util)
  (:export))
