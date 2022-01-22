(define-class <vector> <object> xcor ycor)

(define-method + ((v1 <vector>) (v2 <vector>))
  (make <vector>
        (xcor (+ (xcor v1) (xcor v2)))
        (ycor (+ (ycor v1) (ycor v2)))))

(define-method * ((v1 <vector>) (v2 <vector>))
  (+ (* (xcor v1) (xcor v2))
     (* (ycor v1) (ycor v2))))

(define-method print ((v <vector>))
  (print (cons (xcor v) (ycor v))))

; some test cases:

(define x (make <vector> (xcor 1) (ycor 2)))
(define y (make <vector> (xcor 3) (ycor 4)))

(+ x y)
(* x y)
