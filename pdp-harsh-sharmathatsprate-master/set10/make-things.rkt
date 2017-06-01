
#lang racket                     ;;Used here as to determine language from
                                 ;;source.

(require rackunit)               ;;In built module for testing framework.
(require "extras.rkt")           ;;A file inported.
(require "WidgetWorks.rkt")
(require 2htdp/universe)         ;;A racket module for using big bang functions.
(require 2htdp/image)            ;;A racket module for using image functions.
(check-location "10" "toys.rkt") ;;To check location of file.
(require "Interfaces.rkt")
(require "World.rkt")
(define RIGHT "right")
(define INITIAL-COUNT 1)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-square-toy : PosInt PosInt PosInt -> Toy<%>
;;GIVEN: an x and a y position, and a speed
;;RETURNS: an object representing a square toy at the given position,
;;travelling right at the given speed.
;;EXAMPLE:
;; (make-square-toy 10 10 1)=>
;;(object:SquareToy% ...)
(define (make-square-toy tx ty speed)
  (new Square% [x tx] [y ty]
       [speed speed][direction RIGHT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;make-throbber: PosInt PosInt -> Toy<%>
;;GIVEN: an x and a y position
;;RETURNS: an object representing a throbber at the given position.
;;EXAMPLE:
;;(make-throbber 10 10 )
;;(object:Throbber% ...)
(define (make-throbber x y)
  (new Throbber% [x x][y y]
       [radius 5][flag 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;make-football : PosInt PostInt -> Toy<%>
;;;GIVEN: an x and a y position
;;;RETURNS: an object representing a clock at the given position.
;;;EXAMPLE:
;;;(make-football 10 10 )
;;;(object:Football% ...)
(define (make-football tx ty)
  (new Football% [x tx][y ty]
       [scale-count INITIAL-COUNT]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;make-clock : PosInt PostInt -> Toy<%>
;;;GIVEN: an x and a y position
;;;RETURNS: an object representing a clock at the given position.
;;;EXAMPLE:
;;;(make-clock 10 10)
;;;(object:Clock% ...)
(define (make-clock tx ty)
  (new Clock% [x tx][y ty]
      [ticks 1]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
