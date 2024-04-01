;;;; n-sum.ss
;;; 1. Two sum: https://leetcode.com/problems/two-sum/
;;; 15. 3Sum: https://leetcode.com/problems/3sum/
;;; 167. Two Sum II - Input Array Is Sorted: https://leetcode.com/problems/two-sum-ii-input-array-is-sorted/
;;; 18. 4Sum: https://leetcode.com/problems/4sum/

#!chezscheme
(library (n-sum)
  (export n-sum)
  (import (chezscheme))

  ;; nums: a sorted vector of integers
  ;; target: the target sum
  ;; start: the index to start the search
  (define (two-sum nums target start)
    (let ([res '()]
          [lo start]
          [hi (sub1 (vector-length nums))])
      (do ()
          ((not (< lo hi)) res)
        (let* ([left (vector-ref nums lo)]
               [right (vector-ref nums hi)]
               [sum (+ left right)])
          (cond [(< sum target)
                 (do ()
                     ((not (and (< lo hi) (= (vector-ref nums lo) left))))
                   (set! lo (add1 lo)))]
                [(> sum target)
                 (do ()
                     ((not (and (< lo hi) (= (vector-ref nums hi) right))))
                   (set! hi (sub1 hi)))]
                [else
                 (set! res (cons (list left right) res))
                 (do ()
                     ((not (and (< lo hi) (= (vector-ref nums lo) left))))
                   (set! lo (add1 lo)))
                 (do ()
                     ((not (and (< lo hi) (= (vector-ref nums hi) right))))
                   (set! hi (sub1 hi)))])))))

  ;; nums: a sorted vector of integers
  ;; n: the number of integers to sum
  ;; target: the target sum
  ;; start: the index to start the search
  (define (real-n-sum nums n target start)
    (if (= n 2)
        (two-sum nums target start)
        (do ([res '()]
             [i start (add1 i)])
            ((not (< i (vector-length nums))) res)
          (for-each (lambda (subres)
                      (set! res (cons (cons (vector-ref nums i) subres)
                                      res)))
                    (real-n-sum nums (sub1 n) (- target (vector-ref nums i)) (add1 i)))
          (do ()
              ((not (and (< i (sub1 (vector-length nums)))
                         (= (vector-ref nums i) (vector-ref nums (add1 i))))))
            (set! i (add1 i))))))

  (define (n-sum nums n target)
    (if (or (< n 2) (< (vector-length nums) n))
        '()
        (real-n-sum (vector-sort < nums) n target 0)))

  ;; Test
  ;; > (load "./chezscheme/n-sum.ss")
  ;; > (import (n-sum))
  ;; compiling n-sum.ss with output to n-sum.so
  ;; > (n-sum (vector -1 0 1 2 -1 -4) 3 0)
  ;; ((-1 -1 2) (-1 0 1))
  ;; > (n-sum (vector 1 0 -1 0 -2 2) 4 0)
  ;; ((-1 0 0 1) (-2 -1 1 2) (-2 0 0 2))
  ;; > (n-sum (vector 2 2 2 2) 4 8)
  ;; ((2 2 2 2))
  ) ; end of library
