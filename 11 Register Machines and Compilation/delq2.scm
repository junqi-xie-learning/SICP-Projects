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

(define delq2-compiled
  (make-machine
    '(exp env val proc argl continue unev)
    eceval-operations
    '(  (assign val (op make-compiled-procedure) (label entry37) (reg env))
        (goto (label after-lambda36))
      entry37
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (x l)) (reg argl) (reg env))
        (assign val (op make-compiled-procedure) (label entry45) (reg env))
        (goto (label after-lambda44))
      entry45
        (assign env (op compiled-procedure-env) (reg proc))
        (assign env (op extend-environment) (const (maybe-xs no-xs)) (reg argl) (reg env))
        (save continue)
        (save env)
        (assign proc (op lookup-variable-value) (const null?) (reg env))
        (assign val (op lookup-variable-value) (const maybe-xs) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch78))
      compiled-branch77
        (assign continue (label after-call76))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch78
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call76
        (restore env)
        (restore continue)
        (test (op false?) (reg val))
        (branch (label false-branch47))
      true-branch48
        (assign val (op lookup-variable-value) (const no-xs) (reg env))
        (goto (reg continue))
      false-branch47
        (save continue)
        (save env)
        (assign proc (op lookup-variable-value) (const eq?) (reg env))
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const car) (reg env))
        (assign val (op lookup-variable-value) (const maybe-xs) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch72))
      compiled-branch71
        (assign continue (label after-call70))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch72
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call70
        (assign argl (op list) (reg val))
        (restore env)
        (assign val (op lookup-variable-value) (const x) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch75))
      compiled-branch74
        (assign continue (label after-call73))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch75
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call73
        (restore env)
        (restore continue)
        (test (op false?) (reg val))
        (branch (label false-branch50))
      true-branch51
        (assign proc (op lookup-variable-value) (const delete-reverse) (reg env))
        (save continue)
        (save proc)
        (assign val (op lookup-variable-value) (const no-xs) (reg env))
        (assign argl (op list) (reg val))
        (save argl)
        (assign proc (op lookup-variable-value) (const cdr) (reg env))
        (assign val (op lookup-variable-value) (const maybe-xs) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch66))
      compiled-branch65
        (assign continue (label after-call64))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch66
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call64
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch69))
      compiled-branch68
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch69
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
      after-call67
      false-branch50
        (assign proc (op lookup-variable-value) (const delete-reverse) (reg env))
        (save continue)
        (save proc)
        (save env)
        (assign proc (op lookup-variable-value) (const cons) (reg env))
        (save proc)
        (assign val (op lookup-variable-value) (const no-xs) (reg env))
        (assign argl (op list) (reg val))
        (save argl)
        (assign proc (op lookup-variable-value) (const car) (reg env))
        (assign val (op lookup-variable-value) (const maybe-xs) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch57))
      compiled-branch56
        (assign continue (label after-call55))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch57
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call55
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch60))
      compiled-branch59
        (assign continue (label after-call58))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch60
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call58
        (assign argl (op list) (reg val))
        (restore env)
        (save argl)
        (assign proc (op lookup-variable-value) (const cdr) (reg env))
        (assign val (op lookup-variable-value) (const maybe-xs) (reg env))
        (assign argl (op list) (reg val))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch54))
      compiled-branch53
        (assign continue (label after-call52))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch54
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call52
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch63))
      compiled-branch62
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch63
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
      after-call61
      after-if49
      after-if46
      after-lambda44
        (perform (op define-variable!) (const delete-reverse) (reg val) (reg env))
        (assign val (const (the-unspecified-value)))
        (assign proc (op lookup-variable-value) (const delete-reverse) (reg env))
        (save continue)
        (save proc)
        (assign val (const ()))
        (assign argl (op list) (reg val))
        (save argl)
        (assign proc (op lookup-variable-value) (const delete-reverse) (reg env))
        (assign val (const ()))
        (assign argl (op list) (reg val))
        (assign val (op lookup-variable-value) (const l) (reg env))
        (assign argl (op cons) (reg val) (reg argl))
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch40))
      compiled-branch39
        (assign continue (label after-call38))
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch40
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
      after-call38
        (restore argl)
        (assign argl (op cons) (reg val) (reg argl))
        (restore proc)
        (restore continue)
        (test (op primitive-procedure?) (reg proc))
        (branch (label primitive-branch43))
      compiled-branch42
        (assign val (op compiled-procedure-entry) (reg proc))
        (goto (reg val))
      primitive-branch43
        (assign val (op apply-primitive-procedure) (reg proc) (reg argl))
        (goto (reg continue))
      after-call41
      after-lambda36
        (perform (op define-variable!) (const delq2) (reg val) (reg env))
        (assign val (const (the-unspecified-value)))
        (goto (reg continue)))
