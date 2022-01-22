(define-class <vector> <object> xcor ycor)

(define-method + ((v1 <vector>) (v2 <vector>))
  (make <vector>
        (xcor (+ (get-slot v1 'xcor) (get-slot v2 'xcor)))
        (ycor (+ (get-slot v1 'ycor) (get-slot v2 'ycor)))))

(define-method * ((v1 <vector>) (v2 <vector>))
  (+ (* (get-slot v1 'xcor) (get-slot v2 'xcor))
     (* (get-slot v1 'ycor) (get-slot v2 'ycor))))

(define-method print ((v <vector>))
  (print (cons (get-slot v 'xcor) (get-slot v 'ycor))))

; some test cases:

(define x (make <vector> (xcor 1) (ycor 2)))
(define y (make <vector> (xcor 3) (ycor 4)))

(+ x y)
(* x y)
