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

(define delq1-compiled
  (make-machine
    '(exp env val proc argl continue unev)
    eceval-operations
    '(  (assign val (op make-compiled-procedure) (label entry2) (reg env))
        (goto (label after-lambda1))
      entry2
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (x l)) (reg argl) (reg env))
        (save continue)
        (save env)
        (assign proc (op lookup-variable-value) (const null?) (reg env))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch35))
      compiled-branch34
        (assign continue (label after-call33))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch35
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call33
        (restore env)
        (restore continue)
        (test (op false?) (reg val))
        (branch (label false-branch4))
      true-branch5
        (assign val (const ()))
        (goto (reg continue))
      false-branch4
        (save continue)
        (save env)
        (assign proc (op lookup-variable-value) (const eq?) (reg env))
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const car) (reg env))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch29))
      compiled-branch28
        (assign continue (label after-call27))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch29
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call27
        (assign argl (op list) (reg val))
        (restore env)
        (assign val (op lookup-variable-value) (const x) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch32))
      compiled-branch31
        (assign continue (label after-call30))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch32
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call30
        (restore env)
        (restore continue)
        (test (op false?) (reg val))
        (branch (label false-branch7))
      true-branch8
        (assign proc (op lookup-variable-value) (const delq1) (reg env))
        (save continue)
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const cdr) (reg env))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch23))
      compiled-branch22
        (assign continue (label after-call21))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch23
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call21
        (assign argl (op list) (reg val))
        (restore env)
        (assign val (op lookup-variable-value) (const x) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch26))
      compiled-branch25
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch26
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
      after-call24
      false-branch7
        (assign proc (op lookup-variable-value) (const cons) (reg env))
        (save continue)
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const delq1) (reg env))
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const cdr) (reg env))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch14))
      compiled-branch13
        (assign continue (label after-call12))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch14
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call12
        (assign argl (op list) (reg val))
        (restore env)
        (assign val (op lookup-variable-value) (const x) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch17))
      compiled-branch16
        (assign continue (label after-call15))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch17
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call15
        (assign argl (op list) (reg val))
        (restore env)
        (save argl)
        (assign proc (op lookup-variable-value) (const car) (reg env))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch11))
      compiled-branch10
        (assign continue (label after-call9))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch11
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call9
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch20))
      compiled-branch19
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch20
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
      after-call18
      after-if6
      after-if3
      after-lambda1
        (perform (op define-variable!) (const delq1) (reg val) (reg env))
        (assign val (const (the-unspecified-value)))
        (goto (reg continue)))))
