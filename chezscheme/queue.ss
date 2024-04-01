;;;; queue.ss

#!chezscheme
(library (queue)
  (export make-queue queue? enqueue! dequeue! queue-empty? queue-top queue-size)
  (import (chezscheme))

  (define-record-type (queue mk-queue queue?)
    (fields (mutable vec)
            (mutable head)
            (mutable tail)))

  (define (make-queue)
    (mk-queue (make-vector 1) 0 0))

  (define (queue-full? q)
    (= (vector-length (queue-vec q))
       (queue-tail q)))

  (define (queue-capacity q)
    (vector-length (queue-vec q)))

  (define (queue-reshape! q new-size)
    (define new-vec (make-vector new-size))
    (define size (queue-size q))
    (define offset (queue-head q))
    (let loop ([i 0])
      (if (< i size)
          (begin
            (vector-set! new-vec i (vector-ref (queue-vec q) (+ offset i)))
            (loop (+ i 1)))
          (begin
            (queue-vec-set! q new-vec)
            (queue-head-set! q 0)
            (queue-tail-set! q size)))))

  (define (enqueue! q item)
    (define capacity (queue-capacity q))
    (when (queue-full? q)
      (queue-reshape! q (if (= (queue-size q) (quotient capacity 2))
                            capacity
                            (* 2 capacity))))
    (vector-set! (queue-vec q) (queue-tail q) item)
    (queue-tail-set! q (+ (queue-tail q) 1))
    (void))

  (define (dequeue! q)
    (define capacity (queue-capacity q))
    (when (queue-empty? q)
      (error 'dequeue "queue is empty"))
    (let ([item (vector-ref (queue-vec q) (queue-head q))])
      (queue-head-set! q (+ (queue-head q) 1))
      (when (= (queue-size q) (quotient capacity 4))
        (queue-reshape! q (quotient capacity 2)))
      item))

  (define (queue-empty? q)
    (= (queue-head q) (queue-tail q)))

  (define (queue-top q)
    (when (queue-empty? q)
      (error 'queue-top "queue is empty"))
    (vector-ref (queue-vec q) (queue-head q)))

  (define (queue-size q)
    (- (queue-tail q) (queue-head q)))

  ;; Test
  ;; > (load "./chezscheme/queue.ss")
  ;; > (import (queue))
  ;; > (define q (make-queue))
  ;; > (queue? q)
  ;; #t
  ;; > (queue-empty? q)
  ;; #t
  ;; > (do ([i 0 (1+ i)])
  ;;       ((= i 100))
  ;;     (enqueue! q (random 100)))
  ;; > (queue-size q)
  ;; 100
  ;; > (queue-top q)
  ;; 7
  ;; > (dequeue! q)
  ;; 7
  ;; > (do ([i 0 (1+ i)])
  ;;       ((= i 75))
  ;;     (dequeue! q))
  ;; > (queue-size q)
  ;; 24
  ;; > (do ()
  ;;       ((queue-empty? q))
  ;;     (dequeue! q))
  ;; > (queue-empty? q)
  ;; #t
  ;; > (queue-size q)
  ;; 0
  ) ;; end of library
