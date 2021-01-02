;; 
;;  The play-loop procedure takes as its arguments two prisoner's
;;  dilemma strategies, and plays an iterated game of approximately
;;  one hundred rounds.  A strategy is a procedure that takes
;;  two arguments: a history of the player's previous plays and 
;;  a history of the other player's previous plays.  The procedure
;;  returns either a "c" for cooperate or a "d" for defect.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (play-loop strat0 strat1)
  (define (play-loop-iter strat0 strat1 count history0 history1 limit)
    (cond ((= count limit) (print-out-results history0 history1 limit))
          (else (let ((result0 (strat0 history0 history1))
                      (result1 (strat1 history1 history0)))
                  (play-loop-iter strat0 strat1 (+ count 1)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  limit)))))
  (play-loop-iter strat0 strat1 0 the-empty-history the-empty-history
                  (+ 90 (random 21))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  The following procedures are used to compute and print
;;  out the players' scores at the end of an iterated game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (print-out-results history0 history1 number-of-games)
  (let ((scores (get-scores history0 history1)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)))

(define (get-scores history0 history1)
  (define (get-scores-helper history0 history1 score0 score1)
    (cond ((empty-history? history0)
           (list score0 score1))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1))))))
  (get-scores-helper history0 history1 0 0))

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define *game-association-list*
  ;; format is that first sublist identifies the players' choices 
  ;; with "c" for cooperate and "d" for defect; and that second sublist 
  ;; specifies payout for each player
  '((("c" "c") (3 3))
    (("c" "d") (0 5))
    (("d" "c") (5 0))
    (("d" "d") (1 1))))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list*)))

;; note that you will need to write extract-entry
(define (extract-entry game *game-association-list*)
  (define (extract-iter game part-list)
    (let ((current (car part-list)))
      (if (equal? game (car current)) current
          (extract-iter game (cdr part-list)))))
  (extract-iter game *game-association-list*))

;; test cases
(define a-play (make-play "c" "d"))
(extract-entry a-play *game-association-list*) ;; (("c" "d") (0 5))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

;; A sampler of strategies

(define (NASTY my-history other-history)
  "d")

(define (PATSY my-history other-history)
  "c")

(define (SPASTIC my-history other-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(define (EGALITARIAN my-history other-history)
  (define (count-instances-of test hist)
    (cond ((empty-history? hist) 0)
          ((string=? (most-recent-play hist) test)
           (+ (count-instances-of test (rest-of-plays hist)) 1))
          (else (count-instances-of test (rest-of-plays hist)))))
  (let ((ds (count-instances-of "d" other-history))
        (cs (count-instances-of "c" other-history)))
    (if (> ds cs) "d" "c")))

(define (EYE-FOR-EYE my-history other-history)
  (if (empty-history? my-history)
      "c"
      (most-recent-play other-history)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  play games among the five defined strategies
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(play-loop PATSY PATSY)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop PATSY NASTY)
;; Player 1 Score:  0
;; Player 2 Score:  5.0

(play-loop PATSY SPASTIC)
;; Player 1 Score:  1.5789473684210527
;; Player 2 Score:  3.9473684210526314

(play-loop PATSY EGALITARIAN)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop PATSY EYE-FOR-EYE)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop NASTY NASTY)
;; Player 1 Score:  1.0
;; Player 2 Score:  1.0

(play-loop NASTY SPASTIC)
;; Player 1 Score:  2.676190476190476
;; Player 2 Score:  0.580952380952381

(play-loop NASTY EGALITARIAN)
;; Player 1 Score:  1.0373831775700935
;; Player 2 Score:  0.9906542056074766

(play-loop NASTY EYE-FOR-EYE)
;; Player 1 Score:  1.0408163265306123
;; Player 2 Score:  0.9897959183673469

(play-loop SPASTIC SPASTIC)
;; Player 1 Score:  2.1333333333333333
;; Player 2 Score:  2.1809523809523808

(play-loop SPASTIC EGALITARIAN)
;; Player 1 Score:  1.8061224489795917
;; Player 2 Score:  2.4693877551020407

(play-loop SPASTIC EYE-FOR-EYE)
;; Player 1 Score:  2.3434343434343434
;; Player 2 Score:  2.3434343434343434

(play-loop EGALITARIAN EYE-FOR-EYE)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "eye-for-two-eyes" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (EYE-FOR-TWO-EYES my-history other-history)
  (cond ((empty-history? my-history) "c")
        ((empty-history? (rest-of-plays my-history)) "c")
        (else (let ((previous (most-recent-play other-history))
              (previous-two (most-recent-play (rest-of-plays other-history))))
          (if (and (string=? previous "d") (string=? previous-two "d"))
              "d"
              "c")))))

(play-loop EYE-FOR-TWO-EYES PATSY)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop EYE-FOR-TWO-EYES NASTY)
;; Player 1 Score:  0.978494623655914
;; Player 2 Score:  1.086021505376344

(play-loop EYE-FOR-TWO-EYES SPASTIC)
;; Player 1 Score:  1.8969072164948453
;; Player 2 Score:  3.0309278350515463

(play-loop EYE-FOR-TWO-EYES EGALITARIAN)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "eye-for-n-eyes" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (eye-for-n-eyes n)
  (define (empty-n-history? my-history n)
    (cond ((= n 0) false)
          ((empty-history? my-history) true)
          (else (empty-n-history? (rest-of-plays my-history) (- n 1)))))
  (define (n-ds? other-history n)
    (cond ((= n 0) true)
          ((string=? (most-recent-play other-history) "c") false)
          (else (n-ds? (rest-of-plays other-history) (- n 1)))))
  (lambda (my-history other-history)
    (if (empty-n-history? my-history n)
        "c"
        (if (n-ds? other-history n)
            "d"
            "c"))))

(define EYE-FOR-TWO-EYES (eye-for-n-eyes 2))

(play-loop EYE-FOR-TWO-EYES PATSY)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop EYE-FOR-TWO-EYES NASTY)
;; Player 1 Score:  0.9782608695652174
;; Player 2 Score:  1.0869565217391304

(play-loop EYE-FOR-TWO-EYES SPASTIC)
;; Player 1 Score:  2.047169811320755
;; Player 2 Score:  3.1792452830188678

(play-loop EYE-FOR-TWO-EYES EGALITARIAN)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop EYE-FOR-TWO-EYES EYE-FOR-EYE)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "make-rotating-strategy" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-rotating-strategy strat0 strat1 freq0 freq1)
  (define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
  (lambda (my-history other-history)
    (let ((current (remainder (length my-history) (+ freq0 freq1))))
      (if (< current freq0) (strat0 my-history other-history)
          (strat1 my-history other-history)))))

(define ROTATING-PATSY-NASTY
  (make-rotating-strategy PATSY NASTY 2 2))

(play-loop ROTATING-PATSY-NASTY PATSY)
;; Player 1 Score:  3.981132075471698
;; Player 2 Score:  1.528301886792453

(play-loop ROTATING-PATSY-NASTY NASTY)
;; Player 1 Score:  0.4946236559139785
;; Player 2 Score:  3.021505376344086

(play-loop ROTATING-PATSY-NASTY SPASTIC)
;; Player 1 Score:  2.391304347826087
;; Player 2 Score:  2.1739130434782608

(play-loop ROTATING-PATSY-NASTY EGALITARIAN)
;; Player 1 Score:  4.0
;; Player 2 Score:  1.5

(play-loop ROTATING-PATSY-NASTY EYE-FOR-EYE)
;; Player 1 Score:  2.265957446808511
;; Player 2 Score:  2.265957446808511

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "make-higher-order-spastic" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-higher-order-spastic strats)
  (define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
  (lambda (my-history other-history)
    (let ((current (remainder (length my-history) (length strats))))
      ((list-ref strats current) my-history other-history))))

(define HIGHER-ORDER-SPASTIC
  (make-higher-order-spastic (list PATSY NASTY EGALITARIAN EYE-FOR-EYE)))

(play-loop HIGHER-ORDER-SPASTIC PATSY)
;; Player 1 Score:  3.4948453608247423
;; Player 2 Score:  2.2577319587628866

(play-loop HIGHER-ORDER-SPASTIC NASTY)
;; Player 1 Score:  0.75
;; Player 2 Score:  2.0

(play-loop HIGHER-ORDER-SPASTIC SPASTIC)
;; Player 1 Score:  2.488888888888889
;; Player 2 Score:  2.5444444444444443

(play-loop HIGHER-ORDER-SPASTIC EGALITARIAN)
;; Player 1 Score:  3.511111111111111
;; Player 2 Score:  2.2333333333333334

(play-loop HIGHER-ORDER-SPASTIC EYE-FOR-EYE)
;; Player 1 Score:  2.5051546391752577
;; Player 2 Score:  2.5051546391752577

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "gentle" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (gentle strat gentleness-factor)
  (lambda (my-history other-history)
    (let ((result (strat my-history other-history)))
      (if (string=? result "c") "c"
          (let ((random-factor (/ (random 100) 100)))
            (if (< random-factor gentleness-factor)
                "c"
                "d"))))))

(define SLIGHTLY-GENTLE-NASTY (gentle NASTY 0.1))
(define SLIGHTLY-GENTLE-EYE-FOR-EYE (gentle EYE-FOR-EYE 0.1))

(play-loop SLIGHTLY-GENTLE-NASTY PATSY)
;; Player 1 Score:  4.77319587628866
;; Player 2 Score:  0.3402061855670103

(play-loop SLIGHTLY-GENTLE-NASTY NASTY)
;; Player 1 Score:  0.8910891089108911
;; Player 2 Score:  1.4356435643564356

(play-loop SLIGHTLY-GENTLE-NASTY SPASTIC)
;; Player 1 Score:  2.9285714285714284
;; Player 2 Score:  0.8367346938775511

(play-loop SLIGHTLY-GENTLE-NASTY EGALITARIAN)
;; Player 1 Score:  1.0833333333333333
;; Player 2 Score:  1.5

(play-loop SLIGHTLY-GENTLE-NASTY EYE-FOR-EYE)
;; Player 1 Score:  1.3627450980392157
;; Player 2 Score:  1.3137254901960784

(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE PATSY)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE NASTY)
;; Player 1 Score:  0.8878504672897196
;; Player 2 Score:  1.4485981308411215

(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE SPASTIC)
;; Player 1 Score:  2.3666666666666667
;; Player 2 Score:  2.477777777777778

(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE EGALITARIAN)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

(play-loop SLIGHTLY-GENTLE-EYE-FOR-EYE EYE-FOR-EYE)
;; Player 1 Score:  3.0
;; Player 2 Score:  3.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; code to use in 3 player game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (play-loop-3 strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count history0 history1 history2 limit)
    (cond ((= count limit) (print-out-results-3 history0 history1 history2 limit))
          (else (let ((result0 (strat0 history0 history1 history2))
                      (result1 (strat1 history1 history0 history2))
                      (result2 (strat2 history2 history0 history1)))
                  (play-loop-iter strat0 strat1 strat2 (+ count 1)
                                  (extend-history result0 history0)
                                  (extend-history result1 history1)
                                  (extend-history result2 history2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0
                  the-empty-history the-empty-history the-empty-history
                  (+ 90 (random 21))))

(define (print-out-results-3 history0 history1 history2 number-of-games)
  (let ((scores (get-scores-3 history0 history1 history2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))

(define (get-scores-3 history0 history1 history2)
  (define (get-scores-helper history0 history1 history2 score0 score1 score2)
    (cond ((empty-history? history0)
           (list score0 score1 score2))
          (else (let ((game (make-play (most-recent-play history0)
                                       (most-recent-play history1)
                                       (most-recent-play history2))))
                  (get-scores-helper (rest-of-plays history0)
                                     (rest-of-plays history1)
                                     (rest-of-plays history2)
                                     (+ (get-player-points 0 game) score0)
                                     (+ (get-player-points 1 game) score1)
                                     (+ (get-player-points 2 game) score2))))))
  (get-scores-helper history0 history1 history2 0 0 0))

(define *game-association-list*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  basic strategies for 3 player game
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (NASTY-3 my-history player1-history player2-history)
  "d")

(define (PATSY-3 my-history player1-history player2-history)
  "c")

(define (SPASTIC-3 my-history player1-history player2-history)
  (if (= (random 2) 0)
      "c"
      "d"))

(play-loop-3 PATSY-3 PATSY-3 PATSY-3)
;; Player 1 Score:  4.0
;; Player 2 Score:  4.0
;; Player 3 Score:  4.0

(play-loop-3 PATSY-3 PATSY-3 NASTY-3)
;; Player 1 Score:  2.0
;; Player 2 Score:  2.0
;; Player 3 Score:  5.0

(play-loop-3 PATSY-3 PATSY-3 SPASTIC-3)
;; Player 1 Score:  3.0869565217391304
;; Player 2 Score:  3.0869565217391304
;; Player 3 Score:  4.456521739130435

(play-loop-3 PATSY-3 NASTY-3 NASTY-3)
;; Player 1 Score:  0
;; Player 2 Score:  3.0
;; Player 3 Score:  3.0

(play-loop-3 PATSY-3 NASTY-3 SPASTIC-3)
;; Player 1 Score:  0.7920792079207921
;; Player 2 Score:  3.792079207920792
;; Player 3 Score:  2.603960396039604

(play-loop-3 PATSY-3 SPASTIC-3 SPASTIC-3)
;; Player 1 Score:  1.9595959595959596
;; Player 2 Score:  3.5353535353535355
;; Player 3 Score:  3.4444444444444446

(play-loop-3 NASTY-3 NASTY-3 NASTY-3)
;; Player 1 Score:  1.0
;; Player 2 Score:  1.0
;; Player 3 Score:  1.0

(play-loop-3 NASTY-3 NASTY-3 SPASTIC-3)
;; Player 1 Score:  1.9433962264150944
;; Player 2 Score:  1.9433962264150944
;; Player 3 Score:  0.5283018867924528

(play-loop-3 NASTY-3 SPASTIC-3 SPASTIC-3)
;; Player 1 Score:  3.14
;; Player 2 Score:  1.49
;; Player 3 Score:  1.58

(play-loop-3 SPASTIC-3 SPASTIC-3 SPASTIC-3)
;; Player 1 Score:  2.1847826086956523
;; Player 2 Score:  2.5760869565217392
;; Player 3 Score:  2.8043478260869565

(define (TOUGH-EYE-FOR-EYE my-history player1-history player2-history)
  (if (empty-history? my-history)
      "c"
      (let ((previous1 (most-recent-play player1-history))
            (previous2 (most-recent-play player2-history)))
        (if (or (string=? previous1 "d") (string=? previous2 "d"))
            "d"
            "c"))))

(define (SOFT-EYE-FOR-EYE my-history player1-history player2-history)
  (if (empty-history? my-history)
      "c"
      (let ((previous1 (most-recent-play player1-history))
            (previous2 (most-recent-play player2-history)))
        (if (and (string=? previous1 "d") (string=? previous2 "d"))
            "d"
            "c"))))

(play-loop-3 TOUGH-EYE-FOR-EYE PATSY-3 PATSY-3)
;; Player 1 Score:  4.0
;; Player 2 Score:  4.0
;; Player 3 Score:  4.0

(play-loop-3 TOUGH-EYE-FOR-EYE PATSY-3 NASTY-3)
;; Player 1 Score:  2.9902912621359223
;; Player 2 Score:  0.019417475728155338
;; Player 3 Score:  3.0194174757281553

(play-loop-3 TOUGH-EYE-FOR-EYE NASTY-3 NASTY-3)
;; Player 1 Score:  0.9908256880733946
;; Player 2 Score:  1.018348623853211
;; Player 3 Score:  1.018348623853211

(play-loop-3 SOFT-EYE-FOR-EYE PATSY-3 PATSY-3)
;; Player 1 Score:  4.0
;; Player 2 Score:  4.0
;; Player 3 Score:  4.0

(play-loop-3 SOFT-EYE-FOR-EYE PATSY-3 NASTY-3)
;; Player 1 Score:  2.0
;; Player 2 Score:  2.0
;; Player 3 Score:  5.0

(play-loop-3 SOFT-EYE-FOR-EYE NASTY-3 NASTY-3)
;; Player 1 Score:  0.9908256880733946
;; Player 2 Score:  1.018348623853211
;; Player 3 Score:  1.018348623853211

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "make-combined-strategies" strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-combined-strategies strat1 strat2 combining)
  (lambda (my-history player1-history player2-history)
    (let ((r1 (strat1 my-history player1-history))
          (r2 (strat2 my-history player2-history)))
      (combining r1 r2))))

(define TOUGH-EYE-FOR-EYE
  (make-combined-strategies EYE-FOR-EYE EYE-FOR-EYE
                            (lambda (r1 r2)
                              (if (or (string=? r1 "d") (string=? r2 "d"))
                                  "d"
                                  "c"))))

(define EYE-FOR-EYE-EGALITARIAN
  (make-combined-strategies EYE-FOR-EYE EGALITARIAN
                            (lambda (r1 r2)
                              (if (= (random 2) 0)
                                  r1
                                  r2))))

(play-loop-3 TOUGH-EYE-FOR-EYE PATSY-3 NASTY-3)
;; Player 1 Score:  2.9902912621359223
;; Player 2 Score:  0.019417475728155338
;; Player 3 Score:  3.0194174757281553

(play-loop-3 EYE-FOR-EYE-EGALITARIAN PATSY-3 NASTY-3)
;; Player 1 Score:  2.431578947368421
;; Player 2 Score:  1.1368421052631579
;; Player 3 Score:  4.136842105263158

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "history-summary" data structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-response list)

(define (cooperate-numbers response)
  (list-ref response 0))

(define (defect-numbers response)
  (list-ref response 1))

(define (total-numbers response)
  (list-ref response 2))

(define (add-case response case)
  (if (string=? case "c")
      (make-response (+ (cooperate-numbers response) 1)
                     (defect-numbers response)
                     (+ (total-numbers response) 1))
      (make-response (cooperate-numbers response)
                     (+ (defect-numbers response) 1)
                     (+ (total-numbers response) 1))))

(define (make-history-summary hist-0 hist-1 hist-2)
  (define (summary-iter hists cc cd dd)
    (if (empty-history? (list-ref hists 1))
        (list cc cd dd)
        (let ((current (most-recent-play (list-ref hists 0)))
              (prev-two (list (most-recent-play (list-ref hists 1))
                              (most-recent-play (list-ref hists 2)))))
          (cond ((equal? prev-two (list "c" "c"))
                 (summary-iter (map rest-of-plays hists)
                               (add-case cc current) cd dd))
                ((or (equal? prev-two (list "c" "d")) (equal? prev-two (list "d" "c")))
                 (summary-iter (map rest-of-plays hists)
                               cc (add-case cd current) dd))
                (else
                     (summary-iter (map rest-of-plays hists)
                                   cc cd (add-case dd current)))))))
  (summary-iter (list hist-0 (rest-of-plays hist-1) (rest-of-plays hist-2))
                (make-response 0 0 0) (make-response 0 0 0) (make-response 0 0 0)))

(define (cooperate-cooperate history-summary)
  (list-ref history-summary 0))

(define (cooperate-defect history-summary)
  (list-ref history-summary 1))

(define (defect-defect history-summary)
  (list-ref history-summary 2))

(define summary (make-history-summary
                 (list "c" "c" "d" "d" "c" "d" "c" "c") ;; hist-0
                 (list "c" "c" "c" "d" "d" "c" "d" "c") ;; hist-1
                 (list "c" "c" "d" "d" "d" "c" "c" "c") ;; hist-2
                 ))
;; ((3 0 3) (1 1 2) (0 2 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test "get-probability-of-c" procedure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (get-probability-of-c summary)
  (define (calculate-probability response)
    (let ((cooperate (cooperate-numbers response))
          (total (total-numbers response)))
      (if (= total 0) nil
          (/ cooperate total))))
  (list (calculate-probability (cooperate-cooperate summary))
        (calculate-probability (cooperate-defect summary))
        (calculate-probability (defect-defect summary))))

(define summary (make-history-summary
                 (list "c" "c" "c" "c") ;; hist-0
                 (list "d" "d" "d" "c") ;; hist-1
                 (list "d" "d" "c" "c"))) ;; hist-2
(get-probability-of-c summary)
;; (1 1 1)

(define new-summary (make-history-summary
                     (list "c" "c" "c" "d" "c")
                     (list "d" "c" "d" "d" "c")
                     (list "d" "c" "c" "c" "c")))
(get-probability-of-c new-summary)
;; (0.5 1 ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  define and test some predicate procedures that help in
;;  deciphering another player's strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; in expected-values: #f = don't care 
;;                      X = actual-value needs to be #f or X 
(define (test-entry expected-values actual-values) 
   (cond ((null? expected-values) (null? actual-values)) 
         ((null? actual-values) #f) 
         ((or (not (car expected-values)) 
              (not (car actual-values)) 
              (= (car expected-values) (car actual-values))) 
          (test-entry (cdr expected-values) (cdr actual-values))) 
         (else #f)))

(define (is-he-a-fool? hist0 hist1 hist2) 
   (test-entry (list 1 1 1) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt) 
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)  
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0 
                                                               hist1
                                                               hist2)))))

(define (is-soft-Eye-for-Eye? hist0 hist1 hist2) 
   (test-entry (list 1 1 0) 
               (get-probability-of-c 
                (make-history-summary hist0 hist1 hist2))))

(define (DONT-TOLERATE-FOOLS my-history player1-history player2-history)
  (define (length items)
    (if (null? items)
        0
        (+ 1 (length (cdr items)))))
  (cond ((< (length my-history) 10) "c")
        ((and (could-he-be-a-fool? player1-history my-history player2-history)
              (could-he-be-a-fool? player2-history my-history player1-history)) "d")
        (else "c")))

(play-loop-3 DONT-TOLERATE-FOOLS PATSY-3 PATSY-3)
;; Player 1 Score:  4.89247311827957
;; Player 2 Score:  2.21505376344086
;; Player 3 Score:  2.21505376344086

(play-loop-3 DONT-TOLERATE-FOOLS PATSY-3 NASTY-3)
;; Player 1 Score:  2.0
;; Player 2 Score:  2.0
;; Player 3 Score:  5.0

(play-loop-3 DONT-TOLERATE-FOOLS PATSY-3 SPASTIC-3)
;; Player 1 Score:  3.1515151515151514
;; Player 2 Score:  3.1515151515151514
;; Player 3 Score:  4.424242424242424
