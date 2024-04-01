;;;; binary-heap.ss

#!chezscheme
(library (binary-heap)
  (export heapify! heapsort!)
  (import (chezscheme))

  (define (_parent i)
    (if (odd? i)
        (- (quotient (+ i 1) 2) 1)
        (- (quotient i 2) 1)))

  (define (left i) (- (right i) 1))

  (define (right i) (* 2 (+ i 1)))

  (define (swap! vec i j)
    (let ([tmp (vector-ref vec i)])
      (vector-set! vec i (vector-ref vec j))
      (vector-set! vec j tmp)
      (void)))

  ;; (MAX by default) heapify!
  (define (heapify! vec greater? start size)
    (define l (left start))
    (define r (right start))
    (let ([largest start])
      (when (and (< l size) (greater? (vector-ref vec l) (vector-ref vec largest)))
        (set! largest l))
      (when (and (< r size) (greater? (vector-ref vec r) (vector-ref vec largest)))
        (set! largest r))
      (when (not (= largest start))
        (swap! vec start largest)
        (heapify! vec greater? largest size))))

  (define (build-max-heap! vec cmp)
    (define size (vector-length vec))
    (let loop ([i (_parent (1- size))])
      (when (>= i 0)
        (heapify! vec cmp i size)
        (loop (1- i)))))

  (define (heapsort! vec cmp)
    (build-max-heap! vec cmp)
    (let loop ([i (1- (vector-length vec))])
      (when (>= i 1)
        (swap! vec 0 i)
        (heapify! vec cmp 0 i)
        (loop (1- i)))))

  ;; Test
  ;; > (load "./chezscheme/binary-heap.ss")
  ;; > (import (binary-heap))
  ;; > (define v (make-vector 100))
  ;; > (let loop ([i 0])
  ;;     (when (< i (vector-length v))
  ;;       (vector-set! v i (random 1000))
  ;;       (loop (1+ i))))
  ;; > v
  ;; #(407 248 294 763 592 869 868 472 797 318 606 791 278 641 385
  ;;   724 122 619 488 537 449 738 272 310 868 988 51 85 953 606
  ;;   894 329 825 238 909 547 211 962 906 669 399 48 403 873 381
  ;;   278 202 520 316 808 211 124 165 454 203 494 655 932 589 682
  ;;   1 641 748 655 988 546 469 297 831 637 586 673 890 992 545
  ;;   456 102 968 12 320 152 619 464 492 522 264 840 460 867 179
  ;;   568 379 42 42 635 115 713 268 374 820)
  ;; > (time (heapsort! v <))
  ;; (time (heapsort! v ...))
  ;;     no collections
  ;;     0.000027930s elapsed cpu time
  ;;     0.000027415s elapsed real time
  ;;     9936 bytes allocated
  ;; > v
  ;; #(992 988 988 968 962 953 932 909 906 894 890 873 869 868 868
  ;;   867 840 831 825 820 808 797 791 763 748 738 724 713 682 673
  ;;   669 655 655 641 641 637 635 619 619 606 606 592 589 586 568
  ;;   547 546 545 537 522 520 494 492 488 472 469 464 460 456 454
  ;;   449 407 403 399 385 381 379 374 329 320 318 316 310 297 294
  ;;   278 278 272 268 264 248 238 211 211 203 202 179 165 152 124
  ;;   122 115 102 85 51 48 42 42 12 1)
  ;; > (time (heapsort! v >))
  ;; (time (heapsort! v ...))
  ;;     no collections
  ;;     0.000024208s elapsed cpu time
  ;;     0.000023532s elapsed real time
  ;;     9056 bytes allocated
  ;; > v
  ;; #(1 12 42 42 48 51 85 102 115 122 124 152 165 179 202 203 211
  ;;   211 238 248 264 268 272 278 278 294 297 310 316 318 320 329
  ;;   374 379 381 385 399 403 407 449 454 456 460 464 469 472 488
  ;;   492 494 520 522 537 545 546 547 568 586 589 592 606 606 619
  ;;   619 635 637 641 641 655 655 669 673 682 713 724 738 748 763
  ;;   791 797 808 820 825 831 840 867 868 868 869 873 890 894 906
  ;;   909 932 953 962 968 988 988 992)
  ) ;; end of library
