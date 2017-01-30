(defpackage :typed
  (:use :cl)
  (:import-from :cl-annot.class
                :export-structure)
  (:export :read-annotated-expression))
(in-package :typed)

(cl-annot:enable-annot-syntax)

@export-structure
(defstruct param name type nullable-p optional-p)

@export-structure
(defstruct ret type nullable-p)

@export-structure
(defstruct func name params ret)

;;;
;; @param (string ...) strings
;; @return string
;;;
(defun join-strings (strings)
  (reduce (lambda (a b)
            (concatenate 'string a " " b))
          strings))

;;;
;; @param string string
;; @return param
;;;
(defun parse-param (string)
  (let ((elems (cl-ppcre:split " " string)))
    (make-param :name (car (last elems))
                :type (join-strings (butlast elems))
                :nullable-p nil
                :optional-p nil)))

;;;
;; @param string string
;; @return return
;;;
(defun parse-return (string)
  (make-ret :type string :nullable-p nil))

;;;
;; @param string string
;; @return (string . string)
;;;
(defun split-into-two-strings (string)
  (let ((pos (position #\space string)))
    (cons (subseq string 0 pos) (subseq string (1+ pos)))))

;; Convert a string into its annotation.
;;;
;; @param string string
;; @return (or param ret)
;;;
(defun parse-annotation (string)
  (destructuring-bind (annot-name . annot-arg)
      (split-into-two-strings string)
    (cond ((string= annot-name "@param")
           (parse-param annot-arg))
          ((string= annot-name "@return")
           (parse-return annot-arg))
          (t
           (error "Invalid annotation: ~A" string)))))

;;;
;; @param input-stream stream
;; @return ((or param return) ...)
;;;
(defun read-annotations (stream)
  (let ((annotations nil))
    (loop for line = (read-line stream nil nil) do
      (cond ((cl-ppcre:scan ";; @" line)
             (push (parse-annotation (subseq line 3))
                   annotations))
            ((string= ";;;" line)
             (return-from read-annotations (nreverse annotations)))
            (t
             (error "invalid annoation: ~A" line))))))

(defun read-expression (stream annots)
  (let ((exp (read stream)))
    (case (car exp)
      (defun
        (make-func
         :name (symbol-name (cadr exp))
         :params (remove-if-not (lambda (annot)
                                  (typep annot 'param))
                                annots)
         :ret (find-if (lambda (annot)
                         (typep annot 'ret))
                       annots))))))

(defun read-annotated-expression (stream)
  (let ((annots nil))
    (loop for line = (read-line stream nil nil) while line do
      (when (cl-ppcre:scan ";;;" line)
        (let ((annot (read-expression stream (read-annotations stream))))
          (when annot
            (push annot annots)))))
    annots))
