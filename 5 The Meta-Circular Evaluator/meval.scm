;;
;; meval.scm - 6.001 Spring 2005
;; implementation of meval 
;;


(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))    
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((reversion? exp) (eval-reversion exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((case? exp) (m-eval (case->cond exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((let*? exp) (m-eval (let*->nested-lets exp) env))
        ((do-while? exp) (m-eval (do-while->nested-loops exp) env))
        ((until? exp) (m-eval (until->loops exp) env))
        ((and? exp) (m-eval (and->if exp) env))
        ((or? exp) (m-eval (or->if exp) env))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (procedure-parameters procedure)
                              arguments
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (if (null? (if-alternative-clause exp))
          #f
          (m-eval (if-alternative exp) env))))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))

(define (eval-reversion exp env)
  (unset-variable-value! (assignment-variable exp)
                         env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env))

(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
                      values)))

(define (let*->nested-lets expr)
  (let ((args (let-args expr))
        (body (let-body expr)))
    (define (unfold-let* args)
      (if (no-operands? args)
          (sequence->exp body)
          (make-let (list (first-operand args))
                    (list (unfold-let* (rest-operands args))))))
    (unfold-let* args)))

(define (do-while->nested-loops expr)
  (let ((seq (do-actions expr)))
    (define (unfold-loops seq)
      (if (while-exp? seq)
          (list (make-if (while-pred seq) expr (list 'quote 'done)))
          (cons (first-exp seq) (unfold-loops (rest-exps seq)))))
    (make-begin (unfold-loops seq))))

(define (until->loops expr)
  (let* ((test (until-pred expr))
         (other-exps (until-actions expr))
         (names-occurring (names-used-in expr))
         (loop-name (fresh-symbol names-occurring)))
    (define loop
      (make-define (list loop-name)
        (make-if test (list 'quote 'done)
          (make-begin (append other-exps (list (list loop-name)))))))
    (make-let '() (list loop (list loop-name)))))

(define (and->if expr)
  (let ((exprs (and-exprs expr)))
    (define (unfold-and exprs)
      (if (last-exp? exprs)
          (first-exp exprs)
          (let ((first-expr (first-exp exprs))
                (rest-exprs (rest-exps exprs)))
            (make-if first-expr
                     (unfold-and rest-exprs)
                     #f))))
    (unfold-and exprs)))

(define (or->if expr)
  (let* ((exprs (or-exprs expr))
         (names-occurring (names-used-in expr))
         (first-name (fresh-symbol names-occurring)))
    (define (unfold-or exprs)
      (if (last-exp? exprs)
          (first-exp exprs)
          (let ((first-expr (first-exp exprs))
                (rest-exprs (rest-exps exprs)))
            (make-let (list (list first-name first-expr))
                      (list (make-if first-name first-name
                                     (unfold-or rest-exprs)))))))
    (unfold-or exprs)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-cond-clause clauses)) 'else)
            (make-begin (cdr (first-cond-clause clauses)))
            (make-if (car (first-cond-clause clauses))
                     (make-begin (cdr (first-cond-clause clauses)))
                     (make-cond (rest-cond-clauses clauses)))))))

(define (case->cond expr)
  (let* ((message (case-message expr))
         (clauses (case-clauses expr))
         (names-occurring (names-used-in expr))
         (message-name (fresh-symbol names-occurring)))
    (map (lambda (clause)
           (let ((cond-clause (first-cond-clause clause)))
             (if (list? cond-clause)
               (set-car! clause 
                         (make-eq? message-name cond-clause)))))
         clauses)
    (make-let (list (list message-name message))
              (list (make-cond clauses)))))

(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (do-meval-read)))
    (if (eq? input '**quit**)
        'meval-done
        (let ((output (m-eval input the-global-environment)))
          (announce-output output-prompt)
          (display output)
          (driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define *meval-warn-define* #t) ; print warnings?
(define *in-meval* #f)          ; evaluator running

;;;;;;;;;;;;;;;;;; Code For Problem 9 ;;;;;;;;;;;;;;;;;;;;;;

; type: nil -> list<symbol>
(define (no-names)              ; builds an empty free list
  (list))

; type: symbol -> list<symbol>
(define (single-name var)       ; builds a free list of one variable
  (list var))

; type: symbol, list<symbol> -> list<symbol>
(define (add-name var namelist) ; adds a variable to the list
  (if (not (memq var namelist)) ; avoiding adding duplicates
      (cons var namelist)
      namelist))

; type: list<symbol>, list<symbol> -> list<symbol>
(define (merge-names f1 f2)     ; if variable is free in either list
  (fold-right add-name f1 f2))  ; it's free in the result

; type: list<expression> -> list<symbol>
(define (used-in-sequence exps) ; this is like free-in,
  (fold-right merge-names       ; but works on a sequence of expressions
              (no-names) 
              (map names-used-in exps)))

; type: list<symbol> -> symbol
(define (fresh-symbol free)         ; computes a new symbol not occurring in free
  (fold-right symbol-append 'unused free))


; This is the procedure you need to fill out.
; Depending on the predicates which you define, you may need to change some of the 
; clauses here.

; type: expression -> list<symbol>
; for simplicity, include keywords in the list as well
(define (names-used-in exp)
  (cond ((self-evaluating? exp) (no-names))
        ((variable? exp) (single-name exp))
        ((quoted? exp) (no-names))
        (else (used-in-sequence exp))))

; some test cases:

(names-used-in
 '(do (display (* loop x x))
      (set! x (+ x 1))
    while (< x n)))
;Value: (n < while + set! x loop * display do)

(names-used-in
 '(lambda (x y) (+ 2 3)))
;Value: (+ y x lambda)

(names-used-in
 '(let* ((x 4)
         (y val))
    (if (or z (not z))
        (+ x y)
        7)))
;Value: (+ not z or if val y x let*)

(fresh-symbol '(+ not z or if val y x let*))
;Value: +notzorifvalyxlet*unused

;;;;;;;;;;;;;;;;;;;;; edwin MAGIC - within the darkness magic lurks

(load-option 'format)
(define (warn-if-defined-in-regular-scheme var)
  (if (and (not *in-meval*)
           *meval-warn-define*
           (environment-bound? (the-environment) var))
      (format #t ";Warning: ~A is also bound in normal scheme.~%; Did you intend to define inside Meval?~%"
              var)))

(define meval-mode 
  (if (not (environment-bound? (the-environment) 'meval-mode))
      (in-package (->environment '(edwin))
        (if edwin-editor
            (make-mode 'Meval #f "Meval" #f "repl is in Meval mode"
                       (lambda (buffer) 'done))
            #f))
      meval-mode))

(define set-meval-mode!
  (in-package (->environment '(edwin))
    (lambda ()
      (if edwin-editor
          (let ((sm (->mode "scheme"))
                (repl (->mode "inferior-repl"))
                (me (->mode "Meval")))
            (for-each (lambda (buffer)
                        (if (or (eq? sm (buffer-major-mode buffer))
                                (eq? repl (buffer-major-mode buffer)))
                            (enable-buffer-minor-mode! buffer me)))
                      (buffer-list)))
          'nothing-to-do))))

(define clear-meval-mode!
  (in-package (->environment '(edwin))
    (lambda ()
      (if edwin-editor
          (let ((sm (->mode "scheme"))
                (repl (->mode "inferior-repl"))
                (me (->mode "Meval")))
            (for-each (lambda (buffer)
                        (if (or (eq? sm (buffer-major-mode buffer))
                                (eq? repl (buffer-major-mode buffer)))
                            (disable-buffer-minor-mode! buffer me)))
                      (buffer-list)))
          'nothing-to-do))))

(define (do-meval-read)
  (if *in-meval*
      (read)
      (dynamic-wind (lambda () (set! *in-meval* #t) (set-meval-mode!))
                    (lambda () (read))
                    (lambda () (set! *in-meval* #f) (clear-meval-mode!)))))

;;;;;;;;;;;;;;;;;;; end gift

