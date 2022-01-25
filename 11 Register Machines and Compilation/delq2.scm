;;; Procedure 2:

; (define (delq2 x l)
;   (define (delete-reverse maybe-xs no-xs)
;     (cond ((null? maybe-xs) no-xs)
;           ((eq? x (car maybe-xs)) (delete-reverse (cdr maybe-xs) no-xs))
;           (else (delete-reverse (cdr maybe-xs) (cons (car maybe-xs) no-xs)))))
;   (delete-reverse (delete-reverse l '()) '()))

(define delq2-machine
  (make-machine
    '(x l maybe-xs no-xs val continue)
    standard-primitives
    '(  (assign continue (label delq2-done))
      delq2-loop
        (save continue)
        (assign maybe-xs (reg l))
        (assign no-xs (const ()))
        (assign continue (label after-delete-reverse1))
        (goto (label delete-reverse-loop))
      after-delete-reverse1
        (assign maybe-xs (reg val))
        (assign no-xs (const ()))
        (assign continue (label after-delete-reverse2))
        (goto (label delete-reverse-loop))
      after-delete-reverse2
        (restore continue)
        (goto (reg continue))

      delete-reverse-loop
        (save continue)
        (save no-xs)
        (save maybe-xs)
      test-base
        (test (op null?) (reg maybe-xs))
        (branch (label base-case))
      test-equal
        (assign val (op car) (reg maybe-xs))
        (assign maybe-xs (op cdr) (reg maybe-xs))
        (test (op eq?) (reg x) (reg val))
        (branch (label equal-case))
      non-equal-case
        (assign no-xs (op cons) (reg val) (reg no-xs))
      equal-case
        (assign continue (label delete-reverse-return))
        (goto (label delete-reverse-loop))
      base-case
        (assign val (reg no-xs))
      delete-reverse-return
        (restore maybe-xs)
        (restore no-xs)
        (restore continue)
        (goto (reg continue))

      delq2-done)))
