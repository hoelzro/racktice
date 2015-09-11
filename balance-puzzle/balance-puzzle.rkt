#lang racket

; you have an object of unknown weight
; you have weights whose values are powers of 3 (1, 3, 9, etc)
; return two lists L and R such that (= (sum (cons object-weight L)) (sum R))

(define-syntax-rule (values->list call-to-values)
  (call-with-values (λ () call-to-values) (λ args args)))

(define (log3 n) (/ (log n) (log 3)))

(define (max-right n)
  (let* [(next-lowest-3-power  (floor (log3 n)))
         (next-highest-3-power (ceiling (log3 n)))
         (power-sum (floor (/ (expt 3 next-highest-3-power) 2)))]
    (inexact->exact (expt 3 (if (> n power-sum) next-highest-3-power next-lowest-3-power)))))

(define (determine-weights object-weight)
  (if (zero? object-weight)
    (values '() '())
    (let*-values [((mr) (max-right object-weight))
                  ((sub-left sub-right) (determine-weights (abs (- object-weight mr))))]
      (if (<= mr object-weight)
        (values sub-left  (cons mr sub-right))
        (values sub-right (cons mr sub-left))))))

(define samples (range 1 101))
(define (map-orig f values) (map (λ (arg) (list arg (f arg))) values))

(map displayln (map-orig (λ (n) (values->list (determine-weights n))) samples))
