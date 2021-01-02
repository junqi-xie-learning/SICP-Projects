;;; Project 1, 6.001, Spring 2005

;;; idea is to simulate a baseball robot

;; imagine hitting a ball with an initial velocity of v 
;; at an angle alpha from the horizontal, at a height h
;; we would like to know how far the ball travels.

;; as a first step, we can just model this with simple physics
;; so the equations of motion for the ball have a vertical and a 
;; horizontal component

;; the vertical component is governed by
;; y(t) = v sin alpha t + h - 1/2 g t^2 
;; where g is the gravitational constant of 9.8 m/s^2

;; the horizontal component is governed by
;; x(t) = v cos alpha t
;; assuming it starts at the origin

;; First, we want to know when the ball hits the ground
;; this is governed by the quadratic equation, so we just need to know when 
;; y(t)=0 (i.e. for what t_impact is y(t_impact)= 0).
;; note that there are two solutions, only one makes sense physically

(define square
  (lambda (x) (* x x)))

;; these are constants that will be useful to us
(define gravity 9.8)  ;; in m/s
(define pi 3.14159)

;; Problem 1

(define position
  (lambda (a v u t)
    (+ (* (/ 1 2) a (square t)) (* v t) u)))

;; you need to complete this procedure, then show some test cases

(position 0 0 0 0) ;; 0
(position 0 0 20 0) ;; 20
(position 0 5 10 10) ;; 60
(position 2 2 2 2) ;; 10
(position 5 5 5 5) ;; 92.5


;; Problem 2

(define delta
  (lambda (a b c)
    (- (square b) (* 4 a c))))

(define root1
  (lambda (a b c)
    (if (< (delta a b c) 0)
        false
        (/ (+ (- b) (sqrt (delta a b c))) (* 2 a)))))

(define root2
  (lambda (a b c)
    (if (< (delta a b c) 0)
        false
        (/ (- (- b) (sqrt (delta a b c))) (* 2 a)))))

;; complete these procedures and show some test cases

(root1 1 -3 2) ;; 2
(root2 1 -3 2) ;; 1
(root1 1 2 1) ;; -1
(root2 1 2 1) ;; -1
(root1 5 3 6) ;; false
(root2 5 3 6) ;; false

;; Problem 3

(define time-to-impact
  (lambda (vertical-velocity elevation)
    (root2 (- (* (/ 1 2) gravity)) vertical-velocity elevation)))

;; Note that if we want to know when the ball drops to a particular height r 
;; (for receiver), we have

(define time-to-height
  (lambda (vertical-velocity elevation target-elevation)
    (root2 (- (* (/ 1 2) gravity)) vertical-velocity (- elevation target-elevation))))

;; Problem 4

;; once we can solve for t_impact, we can use it to figure out how far the ball went

;; conversion procedure
(define degree2radian
  (lambda (deg)
    (/ (*  deg pi) 180.)))

(define travel-distance-simple
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (* velocity (cos alpha) (time-to-impact (* velocity (sin alpha)) elevation)))))

;; let's try this out for some example values.  Note that we are going to 
;; do everything in metric units, but for quaint reasons it is easier to think
;; about things in English units, so we will need some conversions.

(define meters-to-feet
  (lambda (m)
    (/ (* m 39.6) 12)))

(define feet-to-meters
  (lambda (f)
    (/ (* f 12) 39.6)))

(define hours-to-seconds
  (lambda (h)
    (* h 3600)))

(define seconds-to-hours
  (lambda (s)
    (/ s 3600)))

;; what is time to impact for a ball hit at a height of 1 meter
;; with a velocity of 45 m/s (which is about 100 miles/hour)
;; at an angle of 0 (straight horizontal)
;; at an angle of (/ pi 2) radians or 90 degrees (straight vertical)
;; at an angle of (/ pi 4) radians or 45 degrees

;; what is the distance traveled in each case?
;; record both in meters and in feet

(travel-distance-simple 1 45 0) ;; 20.32892781536815 (meter)
(travel-distance-simple 1 45 90) ;; 0.000549641898961246 (meter)
(travel-distance-simple 1 45 45) ;; 207.6278611514906 (meter)
(travel-distance-simple (meters-to-feet 1)
                        (seconds-to-hours (* 100 5280))
                        0) ;; 120.36226497508248 (feet)
(travel-distance-simple (meters-to-feet 1)
                        (seconds-to-hours (* 100 5280))
                        90) ;; 0.005829034819278623 (feet)
(travel-distance-simple (meters-to-feet 1)
                        (seconds-to-hours (* 100 5280))
                        45) ;; 2198.306395843612 (feet)

;; Problem 5

;; these sound pretty impressive, but we need to look at it more carefully

;; first, though, suppose we want to find the angle that gives the best
;; distance
;; assume that angle is between 0 and (/ pi 2) radians or between 0 and 90
;; degrees

(define angle-increment 1)

(define find-best-angle
  (lambda (velocity elevation)
    (define (try angle init max)
      (let ((distance (travel-distance-simple elevation velocity angle)))
        (cond ((= angle 90) max)
              ((> distance init) (try (+ angle angle-increment) distance angle))
              (else (try (+ angle angle-increment) init max)))))
    (try 0 0 0)))

;; find best angle
;; try for other velocities
;; try for other heights

(find-best-angle 45 1) ;; 45
(find-best-angle 40 1) ;; 45
(find-best-angle 35 1) ;; 45
(find-best-angle 45 1.5) ;; 45
(find-best-angle 40 1.5) ;; 45
(find-best-angle 35 1.5) ;; 45

;; Problem 6

;; problem is that we are not accounting for drag on the ball (or on spin 
;; or other effects, but let's just stick with drag)
;;
;; Newton's equations basically say that ma = F, and here F is really two 
;; forces.  One is the effect of gravity, which is captured by mg.  The
;; second is due to drag, so we really have
;;
;; a = drag/m + gravity
;;
;; drag is captured by 1/2 C rho A vel^2, where
;; C is the drag coefficient (which is about 0.5 for baseball sized spheres)
;; rho is the density of air (which is about 1.25 kg/m^3 at sea level 
;; with moderate humidity, but is about 1.06 in Denver)
;; A is the surface area of the cross section of object, which is pi D^2/4 
;; where D is the diameter of the ball (which is about 0.074m for a baseball)
;; thus drag varies by the square of the velocity, with a scaling factor 
;; that can be computed

;; We would like to again compute distance , but taking into account 
;; drag.
;; Basically we can rework the equations to get four coupled linear 
;; differential equations
;; let u be the x component of velocity, and v be the y component of velocity
;; let x and y denote the two components of position (we are ignoring the 
;; third dimension and are assuming no spin so that a ball travels in a plane)
;; the equations are
;;
;; dx/dt = u
;; dy/dt = v
;; du/dt = -(drag_x/m + g_x)
;; dv/dt = -(drag_y/m + g_y)
;; we have g_x = - and g_y = - gravity
;; to get the components of the drag force, we need some trig.
;; let speed = (u^2+v^2)^(1/2), then
;; drag_x = - drag * u /speed
;; drag_y = - drag * v /speed
;; where drag = beta speed^2
;; and beta = 1/2 C rho pi D^2/4
;; note that we are taking direction into account here

;; we need the mass of a baseball -- which is about .15 kg.

;; so now we just need to write a procedure that performs a simple integration
;; of these equations -- there are more sophisticated methods but a simple one 
;; is just to step along by some step size in t and add up the values

;; dx = u dt
;; dy = v dt
;; du = - 1/m speed beta u dt
;; dv = - (1/m speed beta v + g) dt

;; initial conditions
;; u_0 = V cos alpha
;; v_0 = V sin alpha
;; y_0 = h
;; x_0 = 0

;; we want to start with these initial conditions, then take a step of size dt
;; (which could be say 0.1) and compute new values for each of these parameters
;; when y reaches the desired point (<= 0) we stop, and return the distance (x)

(define drag-coeff 0.5)
(define density 1.25)  ; kg/m^3
(define mass .145)  ; kg
(define diameter 0.074)  ; m
(define beta (* .5 drag-coeff density (* 3.14159 .25 (square diameter))))

(define integrate
  (lambda (x0 y0 u0 v0 dt g m beta)
    (if (< y0 0) x0
        (integrate (+ x0 (* u0 dt))
                   (+ y0 (* v0 dt))
                   (- u0 (* (/ 1 m) beta (sqrt (+ (square u0) (square v0))) u0 dt))
                   (- v0 (* (+ (* (/ 1 m) (sqrt (+ (square u0) (square v0))) v0 beta)
                               g) dt))
                   dt g m beta))))

(define travel-distance
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (integrate 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha))
                 0.01 gravity mass beta))))

;; RUN SOME TEST CASES

(travel-distance 1 45 45) ;; 92.23060925057175
(travel-distance 1 40 45) ;; 81.66785838214466
(travel-distance 1 35 45) ;; 70.30036328016962

;; what about Denver?

(define density-Denver 1.06)  ; kg/m^3
(define beta-Denver (* .5 drag-coeff density-Denver (* 3.14159 .25 (square diameter))))
(define travel-distance-Denver
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (integrate 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha))
                 0.01 gravity mass beta-Denver))))

(travel-distance-Denver 1 45 45) ;; 99.82569987946395
(travel-distance-Denver 1 40 45) ;; 87.71874467858154
(travel-distance-Denver 1 35 45) ;; 74.94986997657804

;; Problem 7
 
;; now let's turn this around.  Suppose we want to throw the ball.  The same
;; equations basically hold, except now we would like to know what angle to 
;; use, given a velocity, in order to reach a given height (receiver) at a 
;; given distance

(define integrate-time
  (lambda (x0 y0 u0 v0 dt g m beta t)
    (if (< y0 0) t
        (integrate-time (+ x0 (* u0 dt))
                   (+ y0 (* v0 dt))
                   (- u0 (* (/ 1 m) beta (sqrt (+ (square u0) (square v0))) u0 dt))
                   (- v0 (* (+ (* (/ 1 m) (sqrt (+ (square u0) (square v0))) v0 beta)
                               g) dt))
                   dt g m beta (+ t dt)))))

(define travel-time
  (lambda (elevation velocity angle)
    (let ((alpha (degree2radian angle)))
      (integrate-time 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha))
                 0.01 gravity mass beta 0))))

(define angle-increment-refined 0.1)

(define find-best-angle-time
  (lambda (velocity elevation target)
    (define (desired? distance)
      (< (abs (- distance target)) 0.5))
    (define (try angle init min)
      (let ((distance (travel-distance elevation velocity angle)))
        (cond ((> angle 90) init)
              ((desired? distance)
               (let ((time (travel-time elevation velocity angle)))
                 (if (or (= init 0) (< time init))
                     (try (+ angle angle-increment-refined) time angle)
                     (try (+ angle angle-increment-refined) init min))))
              (else (try (+ angle angle-increment-refined) init min)))))
    (try -90 0 0)))

;; a catcher trying to throw someone out at second has to get it roughly 36 m
;; (or 120 ft) how quickly does the ball get there, if he throws at 55m/s,
;;  at 45m/s, at 35m/s?

(find-best-angle-time 55 1 36) ;; 0.7700000000000005
(find-best-angle-time 45 1 36) ;; 0.9500000000000006
(find-best-angle-time 35 1 36) ;; 1.2200000000000009

;; try out some times for distances (30, 60, 90 m) or (100, 200, 300 ft) 
;; using 45m/s

(find-best-angle-time 45 1 30) ;; 0.7600000000000005
(find-best-angle-time 45 1 60) ;; 1.8100000000000014
(find-best-angle-time 45 1 90) ;; 3.6299999999999666

;; Problem 8

(define travel-distance-bouncing
  (lambda (elevation velocity angle n)
    (define (sum-up n velocity sum)
      (if (= n 0) sum
          (sum-up (- n 1) (/ velocity 2)
                  (+ sum (travel-distance 0 (/ velocity 2) angle)))))
    (sum-up n (/ velocity 2) (travel-distance elevation velocity angle))))

;; Test cases:

(travel-distance-bouncing 1 45 45 1) ;; 104.16677768590627
(travel-distance-bouncing 1 45 45 2) ;; 107.37341770613027
(travel-distance-bouncing 1 45 45 10) ;; 108.50839268331028

;; Problem 9

(define integrate-bouncing
  (lambda (x0 y0 u0 v0 dt g m beta n)
    (if (< y0 0) (if (= n 0) x0
                     (integrate-bouncing x0 0 u0 (- v0) dt g m beta (- n 1)))
        (integrate-bouncing (+ x0 (* u0 dt))
                            (+ y0 (* v0 dt))
                            (- u0 (* (/ 1 m) beta (sqrt (+ (square u0) (square v0)))
                                     u0 dt))
                            (- v0 (* (+ (* (/ 1 m) (sqrt (+ (square u0) (square v0)))
                                           v0 beta) g) dt))
                            dt g m beta n))))

(define travel-distance-bouncing-refined
  (lambda (elevation velocity angle n)
    (let ((alpha (degree2radian angle)))
      (integrate-bouncing 0 elevation (* velocity (cos alpha)) (* velocity (sin alpha))
                          0.01 gravity mass beta n))))

;; Test cases:

(travel-distance-bouncing-refined 1 45 45 1) ;; 125.14326097227813
(travel-distance-bouncing-refined 1 45 45 2) ;; 143.98387860377278
(travel-distance-bouncing-refined 1 45 45 10) ;; 194.72136542228554
