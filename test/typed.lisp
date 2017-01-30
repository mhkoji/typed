(defpackage :typed.test
  (:use :cl :typed :fiveam))
(in-package :typed.test)
(in-suite* :typed)

(test parse-annotation
  (let ((func
         (with-input-from-string (s
                           ";;;
;; @param string str
;; @return (or param ret)
;;;
(defun parse-annotation (string))")
           (car (read-annotated-expression s)))))
    (is (string= (func-name func) "PARSE-ANNOTATION"))
    (is (= (length (func-params func)) 1))
    (let ((param (car (func-params func))))
      (is (string= (param-name param) "str"))
      (is (string= (param-type param) "string")))
    (let ((ret (func-ret func)))
      (is (string= (ret-type ret) "(or param ret)")))))
