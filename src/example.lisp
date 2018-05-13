(defpackage :lisp-pref.example
  (:use :cl)
  (:export #:foo))

(in-package :lisp-pref.example)

(lisp-pref:define-custom-variable (:lisp-pref :example :hostname)
  :type :string
  :validator #'valid-hostname-p)

(defun foo ()
  (let ((hostname (lisp-pref:get-value '(:lisp-pref :example :hostname) "default-name")))
    (format t "The hostname is: ~s~%" hostname)))
