;;; Procedure 1:

; (define (delq1 x l)
;   (cond ((null? l) '())
;         ((eq? x (car l)) (delq1 x (cdr l)))
;         (else (cons (car l) (delq1 x (cdr l))))))

(define delq1-machine
  (make-machine
    '(x l val continue)
    standard-primitives
    '(  (assign continue (label delq1-done))
      delq1-loop
        (save continue)
        (save l)
        (save x)
      test-base
        (test (op null?) (reg l))
        (branch (label base-case))
      test-equal
        (assign val (op car) (reg l))
        (assign l (op cdr) (reg l))
        (test (op eq?) (reg x) (reg val))
        (branch (label equal-case))
      non-equal-case
        (save val)
        (assign continue (label after-non-equal))
        (goto (label delq1-loop))
      after-non-equal
        (restore x)
        (assign val (op cons) (reg x) (reg val))
        (goto (label delq1-return))
      equal-case
        (assign continue (label after-equal))
        (goto (label delq1-loop))
      after-equal
        (goto (label delq1-return))
      base-case
        (assign val (const ()))
      delq1-return
        (restore x)
        (restore l)
        (restore continue)
        (goto (reg continue))
      delq1-done)))
