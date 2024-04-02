;;;; queue.lisp
(in-package #:cl-user)

(defpackage #:queue
  (:use #:cl)
  (:export #:queue
           #:make-queue
           #:queue/list
           #:make-queue/list
           #:queue/sv
           #:make-queue/sv
           #:queue/s
           #:make-queue/s
           #:queue-empty-p
           #:enqueue
           #:dequeue
           #:queue-size
           #:queue-top))

(in-package #:queue)

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

;;; Basic
;;; A basic queue that is implemented by using an adjustable array.
(defclass queue ()
  ((array :accessor queue-array
          :initarg :array
          :documentation "An adjustable array to store elements.")
   (ptr   :accessor queue-ptr
          :initform 0
          :type fixnum
          :documentation "A pointer to the top element.")))

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

;;; queue/list
;;; A queue that is implemented by using a list.
(defclass queue/list ()
  ((lst :accessor queue-list
        :initform nil
        :documentation "A list to store elements.")))

(defun make-queue/list ()
  (make-instance 'queue/list))

(defmethod queue-empty-p ((queue queue/list))
  (null (queue-list queue)))

(defmethod enqueue ((queue queue/list) element)
  (setf (queue-list queue)
        (append (queue-list queue) (list element)))
  nil)

(defmethod dequeue ((queue queue/list))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (pop (queue-list queue)))

(defmethod queue-size ((queue queue/list))
  (length (queue-list queue)))

(defmethod queue-top ((queue queue/list))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (first (queue-list queue)))

;;; queue/sv
;;; A queue that is implemented by using a simple vector.
(defclass queue/sv ()
  ((sv   :accessor queue-sv
         :initform (make-array 1 :element-type 't :initial-element nil)
         :documentation "A simple vector to store elements.")
   (head :accessor queue-sv-head
         :initform 0
         :type fixum
         :documentation "A pointer to the top element.")
   (tail :accessor queue-sv-tail
         :initform 0
         :type fixnum
         :documentation "A pointer to the last element.")))

(defun make-queue/sv ()
  (make-instance 'queue/sv))

(defun queue/sv-capacity (queue)
  (length (queue-sv queue)))

(defmethod queue-empty-p ((queue queue/sv))
  (zerop (queue-size queue)))

(defun queue/sv-reshape (queue new-capacity)
  (loop with size fixnum = (queue-size queue)
        with new-sv simple-vector = (make-array new-capacity :element-type 't :initial-element nil)
        for i fixnum from 0 below size
        with offset fixnum = (queue-sv-head queue)
        do (setf (svref new-sv i) (svref (queue-sv queue) (+ i offset)))
        finally (setf (queue-sv queue) new-sv
                      (queue-sv-head queue) 0
                      (queue-sv-tail queue) size)))

(defun queue/sv-full-p (queue)
  (= (queue-sv-tail queue)
     (queue/sv-capacity queue)))

(defmethod enqueue ((queue queue/sv) element)
  (when (queue/sv-full-p queue)
    (if (<= (queue-size queue) (/ (length (queue-sv queue)) 2))
        (queue/sv-reshape queue (queue/sv-capacity queue))
        (queue/sv-reshape queue (* 2 (queue/sv-capacity queue)))))
  (setf (svref (queue-sv queue) (queue-sv-tail queue)) element)
  (incf (queue-sv-tail queue))
  nil)

(defmethod dequeue ((queue queue/sv))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (let ((elm (svref (queue-sv queue) (queue-sv-head queue))))
    (incf (queue-sv-head queue))
    (when (<= (queue-size queue) (/ (queue/sv-capacity queue) 4))
      (queue/sv-reshape queue (/ (queue/sv-capacity queue) 2)))
    elm))

(defmethod queue-size ((queue queue/sv))
  (- (queue-sv-tail queue) (queue-sv-head queue)))

(defmethod queue-top ((queue queue/sv))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (svref (queue-sv queue) (queue-sv-head queue)))

;;; queue/s
;;; A queue that is implemented by using two stacks
(defclass queue/s ()
  ((s1 :accessor queue-s1 :initform nil)
   (s2 :accessor queue-s2 :initform nil)))

(defun make-queue/s ()
  (make-instance 'queue/s))

(defmethod queue-empty-p ((queue queue/s))
  (and (null (queue-s1 queue))
       (null (queue-s2 queue))))

(defmethod enqueue ((queue queue/s) element)
  (push element (queue-s1 queue))
  nil)

(defmethod dequeue ((queue queue/s))
  (queue-top queue)
  (pop (queue-s2 queue)))

(defmethod queue-size ((queue queue/s))
  (+ (length (queue-s1 queue))
     (length (queue-s2 queue))))

(defmethod queue-top ((queue queue/s))
  (when (queue-empty-p queue)
    (error "Queue is empty."))
  (when (null (queue-s2 queue))
    (do ()
        ((null (queue-s1 queue)))
      (push (pop (queue-s1 queue))
            (queue-s2 queue))))
  (first (queue-s2 queue)))
