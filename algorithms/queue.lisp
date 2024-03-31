;;;; queue.lisp
(in-package #:cl-user)

(defpackage #:queue
  (:use #:cl)
  (:export #:make-queue
           #:queue-empty-p
           #:enqueue
           #:dequeue
           #:queue-size
           #:queue-top))

(in-package #:queue)

;;; Class
(defclass queue ()
  ((array :accessor queue-array
          :initarg :array
          :documentation "An adjustable array to store elements.")
   (ptr   :accessor queue-ptr
          :initform 0
          :documentation "A pointer to the top element.")))

;;; Protocol
(defgeneric queue-empty-p (queue)
  (:documentation "Return t if the queue is empty, otherwise nil."))

(defgeneric enqueue (queue element)
  (:documentation "Add an element to the queue."))

(defgeneric dequeue (queue)
  (:documentation "Remove an element from the queue. If the queue is empty, signal an error."))

(defgeneric queue-size (queue)
  (:documentation "Return the number of elements in the queue."))

(defgeneric queue-top (queue)
  (:documentation "Return the top element of the queue. If the queue is empty, signal an error."))

;;; Implementation
(defun make-queue (&optional (capacity 0))
  (make-instance 'queue
                 :array (make-array capacity :element-type 't :adjustable t :fill-pointer 0)))

(defmethod queue-empty-p ((queue queue))
  (zerop (queue-size queue)))

(defmethod enqueue ((queue queue) element)
  (vector-push-extend element (queue-array queue))
  nil)

(defmethod dequeue ((queue queue))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (let ((elm (aref (queue-array queue) (queue-ptr queue))))
    (incf (queue-ptr queue))
    elm))

(defmethod queue-size ((queue queue))
  (- (fill-pointer (queue-array queue))
     (queue-ptr queue)))

(defmethod queue-top ((queue queue))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (aref (queue-array queue) (queue-ptr queue)))
