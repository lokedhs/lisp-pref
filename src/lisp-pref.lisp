(in-package :lisp-pref)

(defclass lisp-pref-type ()
  ((name :initarg :name)
   (encode :initarg :encode)
   (decode :initarg :decode)
   (documentation :initarg :documentation)))

(defmacro define-type (name &key (encode 'identity) (decode 'identity) documentation)
  (check-type name symbol)
  (check-type encode symbol)
  (check-type decode symbol)
  (check-type documentation string)
  (alexandria:with-gensyms (type)
    `(let ((,type (make-instance 'lisp-pref-type
                                 :name ',name
                                 :encode ',encode
                                 :decode ',decode
                                 :documentation ,documentation)))
       (setf (get ,name :lisp-pref-type) ,type))))

(defvar *variable-registry* nil)

(defmacro define-custom-variable (path &rest params)
  (alexandria:once-only (path)
    `(pushnew ,(cons path params) *variable-registry*
              :key #'car :test #'equal)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *provider* nil)

(defgeneric provider-get-value (provider path))

(defun find-provider ()
  (or *provider* (setq *provider* (create-provider))))

(defun create-provider ()
  (destructuring-bind (&key spec)
      (get :lisp-pref-defaults :provider '(:spec (#:LISP-PREF #:SIMPLE-PROVIDER)))
    (let ((package (find-package (first spec))))
      (unless package
        (error "Package for provider ~s not found" spec))
      (let ((class-sym (find-symbol (second spec) package)))
        (make-instance class-sym)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass file-provider ()
  ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass simple-provider ()
  ((values :initform nil
           :accessor simple-provider/values)))

(defmethod provider-get-value ((provider simple-provider) path)
  (let ((value (find path (simple-provider/values provider) :key #'car :test #'equal)))
    (cdr value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-type :string
  :documentation "Plain string value")

(defun get-value (path &optional default-value)
  (or (provider-get-value (find-provider) path) default-value))
