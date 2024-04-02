;;;; stack.lisp
(in-package #:cl-user)

(defpackage #:stack
  (:use #:cl)
  (:export #:stack
           #:make-stack
           #:stack-push
           #:stack-pop
           #:stack-empty-p
           #:stack-size
           #:stack-top))

(in-package #:stack)

;;; Protocol
(defgeneric stack-push (stack element)
  (:documentation "Push an element onto the stack."))

(defgeneric stack-pop (stack)
  (:documentation "Pop an element from the stack. If the stack is empty, an error is signaled."))

(defgeneric stack-empty-p (stack)
  (:documentation "Return T if the stack is empty, NIL otherwise."))

(defgeneric stack-size (stack)
  (:documentation "Return the number of elements in the stack."))

(defgeneric stack-top (stack)
  (:documentation "Return the top element of the stack. If the stack is empty, an error is signaled."))

;;; stack
;;; A stack implemented by using an adjustable array.
(defclass stack ()
  ((array :accessor stack-array
          :initarg :array
          :documentation "The array that represents the stack.")))

(defun make-stack (&optional (capacity 0))
  (make-instance 'stack
                 :array (make-array capacity :element-type t :adjustable t :fill-pointer 0)))

(defmethod stack-push ((stack stack) element)
  (vector-push-extend element (stack-array stack))
  nil)

(defmethod stack-pop ((stack stack))
  (when (stack-empty-p stack)
    (error "Stack is empty."))
  (vector-pop (stack-array stack)))

(defmethod stack-empty-p ((stack stack))
  (zerop (stack-size stack)))

(defmethod stack-size ((stack stack))
  (length (stack-array stack)))

(defmethod stack-top ((stack stack))
  (if (stack-empty-p stack)
      (error "Stack is empty.")
      (aref (stack-array stack) (1- (fill-pointer (stack-array stack))))))
