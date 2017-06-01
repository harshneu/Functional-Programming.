#lang racket

;; the model consists of a particle, bouncing with its center from x=0
;; to x=150.  It accepts commands and reports when its status changes

;;Required Files:

(require "extras.rkt")
(require "Interfaces.rkt")
(require 2htdp/image)
(require "PerfectBounce.rkt")
(require rackunit)
(require "PositionController.rkt")
;;Provided Function:
(define EMPTY-CANVAS (empty-scene 600 500))
(provide Model%)

;; ModelClass:
;;The Model Class has the particle and rectangle the initial position of the
;;particle and rectangle are established in this class.

(define Model% 
  (class* object% (Model<%>)
    (init-field [p particle])
    (init-field [rect (make-rect
                       0 BOUNDARY 0 100)])   ;;represents coordnates.
    
    ; ListOfController<%>
    (init-field [controllers empty])               ;;No controllers initially.
    (field [stop-tick? false] )
    
    (super-new)
    (set! p (make-particle
             WIDTH3 HEIGHT1 0 0))            ;;Initial particle Condition.
    
    ;;#########################################################################
    
    ;; stop-tick:->Boolean
    ;; Effects:A boolean results to manage the ball movement in the canvas.
    ;; Strategy: Communicate via state.
    ;; Examples: Covered In test Cases.
    
    (define/public (stop-tick value)
      (set! stop-tick? value))
    
    ;; after-tick:-> Void
    ;; Given :the world.
    ;; Effects:Moves the object by v.if the resulting x is >= 200 or <= 0 
    ;;             reports x at ever tick reports velocity only when it changes
    ;; Strategy: Communicate via state.
    ;; Examples :Covered in tests.
    
    (define/public (after-tick)
      (if stop-tick?
          this
          (begin
            (set! p (particle-after-tick p rect))
            (publish-particle))))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
    ;; Controller -> Void
    ;; Given: A controller.
    ;; Returns:register the new controller and send it some data 
    ;; Strategy: Communicate via state.
    
    (define/public (register c)
      (begin
        (set! controllers (cons c controllers))
        (send c receive-signal p)))
    
    ;; execute-command -> Void
    ;; Given :A command.
    ;; Returns : decodes the command, executes it, and sends updates to the
    ;; controllers.
    ;; Strategy:Communicate via state.
    
    (define/public (execute-command cmd)
      (begin
        (set! p cmd)
        (publish-particle)))
    
    
    ;; publish-particle: Paricle->Particle.
    ;; Given :A particle.
    ;; Returns: report position or velocity to each controllers.
    
    (define (publish-particle)
      (for-each
       (lambda (obs) (send obs receive-signal p))
       controllers))
    

    ;;For Test Functions:
    (define/public (for-test:get-particle)p)
    (define/public (for-test:get-controllers)controllers)))

;;#########################################################################

;; TESTS
(define particle1 (make-particle 75 50 0 0))
(define rect1 (make-rect 0 100 0 150))


(define model1 (new Model%))
(send model1 after-tick)

(define model2 (new Model% [p particle1][rect rect1]))


 ;;#######################################################################



 

(define controller1 (new PositionController% [model model1]
                         [x 300] [y 250] [p particle1]
                         [width 160] [height 50]
                         [half-width 80] [half-height 25]
                         [selected? false] [block-selected? false]
                         [saved-mx 0] [saved-my 0]))



;;Testing:
(begin-for-test
  (begin
    (check-equal?
     (is-a?
      model1
      Model%)
     true
     "The Value Given must be true")
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 after-tick)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 stop-tick true)
    (send model2 after-tick)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")
    (send model2 register controller1)
    (check-equal?
     (length (send model2 for-test:get-controllers))
     1  
     "The value is not same")
    (send model2 execute-command particle1)
    (check-equal? 
     (send model2 for-test:get-particle)
     particle1
     "The vale of particle should not change after tick")

    ))
(define (data-image-test)
      (above
        (text "Arrow-Keys : Change Position" 10 "black")
        (text (string-append
                "X = "
                (number->string (particle-x particle1))
                 " Y = "
                (number->string (particle-y particle1))
                " VX = "
                (number->string (particle-vx particle1))
                 " VY = "
                (number->string (particle-vy particle1)))
          10
          "black")))

(define (viewer-initial-image-test)
     (let ((the-data-image (data-image-test)))
        (overlay  
          the-data-image
          (rectangle 
            160
            50
            "outline" 
            "black"))))

    (define (viewer-image-test) 
      (let ((initial-image (viewer-initial-image-test)))
        (overlay/align "left" "top"
                       initial-image
                       (square 10 "outline" "black"))))
(define (final-image)
  (place-image (viewer-image-test) 300 250 EMPTY-CANVAS))

;;Testing:
(begin-for-test
  (begin
    (check-equal? 
     (send controller1 add-to-scene EMPTY-CANVAS) 
     (final-image)
     "The vale of particle should not change after tick")))


