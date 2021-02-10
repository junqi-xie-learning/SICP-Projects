;;;; This is the file code.scm

;;; The following definitions install rational arithmetic.  Warning:
;;; Don't use any arithmetic operations other than these.

; (define + (access + '()))
; (define - (access - '()))
; (define * (access * '()))
; (define / (access / '()))

;;; some basic stream operations

;; the empty stream is the same as the empty list in our implementation
;; of streams
(define the-empty-stream '())

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream (apply proc (map stream-car argstreams))
                   (apply stream-map (cons proc (map stream-cdr argstreams))))))

(define (add-streams s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (cons-stream (+ (stream-car s1) (stream-car s2))
                      (add-streams (stream-cdr s1)
                                   (stream-cdr s2))))))

(define (scale-stream c stream)
  (stream-map (lambda (x) (* x c)) stream))

;;; power series operations

(define add-series add-streams)

(define scale-series scale-stream)

(define (negate-series s)
  (scale-series -1 s))

(define (subtract-series s1 s2)
  (add-series s1 (negate-series s2)))

(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-series 
                 (add-series (scale-series (stream-cdr s1)
                                           (stream-car s2))
                             (scale-series (stream-cdr s2)
                                           (stream-car s1)))
                 (cons-stream 0 (mul-series (stream-cdr s1)
                                            (stream-cdr s2))))))

(define (invert-unit-series s)
  (cons-stream (stream-car s)
               (stream-map - (mul-series (stream-cdr s)
                                         (invert-unit-series s)))))

(define (div-series s1 s2)
  (if (not (= (stream-car s2) 0))
      (mul-series s1 (invert-unit-series s2))
      (error "Zero constant term -- DIV-SERIES")))

(define (integrate-series-tail s)
  (stream-map / s (stream-cdr non-neg-integers)))

(define (derivative-series s)
  (stream-cdr (stream-map * s non-neg-integers)))

;;; display the first n coefficients of a series

(define (show-series s nterms)
  (if (= nterms 0)
      'done
      (begin (write-line (stream-car s))
             (show-series (stream-cdr s) (- nterms 1)))))

;;; return the coefficient of x^n

(define (series-coeff s n)
  (stream-ref s n))



;;; create a (finite) series from a list of coefficients

(define (coeffs->series list-of-coeffs)
  (define zeros (cons-stream 0 zeros))
  (define (iter list)
    (if (null? list)
        zeros
        (cons-stream (car list)
                     (iter (cdr list)))))
  (iter list-of-coeffs))


;;; create a series from a procedure: nth term is P(n)
;;; requires non-neg-integers to be 0,1,2,3....


(define (proc->series proc)
  (stream-map proc non-neg-integers))



;;; defining basic streams and series

(define ones (cons-stream 1 ones))
(define non-neg-integers
  (cons-stream 0 (stream-map (lambda (x) (+ x 1)) non-neg-integers)))
(define alt-ones
  (cons-stream 1 (stream-map (lambda (x) (- x)) alt-ones)))
(define zeros
  (add-streams alt-ones (stream-cdr alt-ones)))

(define s1 ones)
(define s2 (stream-cdr non-neg-integers))

(define exp-series
  (cons-stream 1 (integrate-series-tail exp-series)))



;;; approximating function value at x

(define (partial-sums stream)
  (cons-stream (stream-ref stream 0)
               (add-series (partial-sums stream) (stream-cdr stream))))

(define (pow n x)
  (if (= x 0) 1
      (* n (pow n (- x 1)))))

(define (approximate x s)
  (partial-sums 
    (stream-map * s 
      (stream-map (lambda (n) (pow x n))
                  non-neg-integers))))