;;;; ugly-number.ss
;;; 261. Ugly Number: https://leetcode.com/problems/ugly-number/description/
;;; 264. Ugly Number II: https://leetcode.com/problems/ugly-number-ii/description/

#!chezscheme
(library (ugly-number)
  (export ugly-number? nth-ugly-number)
  (import (chezscheme))

  (define (ugly-number? n)
    (if (or (negative? n) (zero? n))
        #f
        (let loop ([n n])
          (cond [(zero? (remainder n 2)) (loop (quotient n 2))]
                [(zero? (remainder n 3)) (loop (quotient n 3))]
                [(zero? (remainder n 5)) (loop (quotient n 5))]
                [else (= n 1)]))))

  (define (nth-ugly-number n)
    (let loop ([ugly (make-vector (add1 n) 1)]
               [p2 1]
               [p3 1]
               [p5 1]
               [product2 1]
               [product3 1]
               [product5 1]
               [p 1])
      (when (<= p n)
        (let ([val (min product2 product3 product5)])
          (vector-set! ugly p val)
          (loop ugly
                (if (= product2 val) (add1 p2) p2)
                (if (= product3 val) (add1 p3) p3)
                (if (= product5 val) (add1 p5) p5)
                (if (= product2 val) (* 2 (vector-ref ugly p2)) product2)
                (if (= product3 val) (* 3 (vector-ref ugly p3)) product3)
                (if (= product5 val) (* 5 (vector-ref ugly p5)) product5)
                (add1 p))))
      (vector-ref ugly n)))

  ;; Test
  ;; > (load "./chezscheme/ugly-number.ss")
  ;; > (import (ugly-number))
  ;; > (ugly-number? 12)
  ;; #t
  ;; > (ugly-number? 14)
  ;; #f
  ;; > (nth-ugly-number 10)
  ;; 12
  ;; > (time (nth-ugly-number 100))
  ;; (time (nth-ugly-number 100))
  ;;     no collections
  ;;     0.000007824s elapsed cpu time
  ;;     0.000007121s elapsed real time
  ;;     2416 bytes allocated
  ;; 1536
  ) ;; end of library
