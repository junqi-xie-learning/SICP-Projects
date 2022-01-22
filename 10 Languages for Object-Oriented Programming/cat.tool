(define-generic-function 4-legged?)
(define-method 4-legged? ((x <object>))
  'Who-knows?)

(define-generic-function say)

(define-class <cat> <object> size breed)
(define-method 4-legged? ((x <cat>))
  true)
(define-method say ((cat <cat>) (stuff <object>))
  (print 'meow:) ;print is TOOL's procedure for printing things
  (print stuff))

(define-class <house-cat> <cat> address)

(define-class <show-cat> <cat> awards)
(define-method say ((cat <show-cat>) (stuff <object>))
  (print stuff)
  (print '(I am beautiful)))
(define-method say ((cat <cat>) (stuff <number>))
  (print '(cats never discuss numbers)))

; some test cases:

(define garfield (make <cat> (size 6) (breed 'weird)))
(get-slot garfield 'breed)
(4-legged? garfield)
(4-legged? 'Hal)

(define fluffy (make <house-cat> (size 'tiny)))
(get-slot fluffy 'breed)
(say garfield '(feed me))
(say fluffy '(feed me))
(say 'hal '(feed me))

(define Cornelius-Silverspoon-the-Third
  (make <show-cat> 
        (size 'large)
        (breed '(Cornish Rex))
        (awards '((prettiest skin)))))
(say cornelius-silverspoon-the-Third '(feed me))
(say fluffy 37)
