#lang racket
(require rackunit)
(require 2htdp/universe)
(require 2htdp/image)
(require "extras.rkt")
(require "WidgetWorks.rkt")
(provide
 Toy<%>
 PlaygroundState<%>)
(define Toy<%>
  (interface (SWidget<%>)

    ;; -> Int
    ;; Returns the x or y position of the center of the toy
    toy-x
    toy-y
    
    ;; -> Int
    ;; RETURNS: some data related to the toy.  The interpretation of
    ;; this data depends on the class of the toy.
    ;; for a square, it is the velocity of the square (rightward is
    ;; positive)
    ;; for a throbber, it is the current radius of the throbber
    ;; for the clock, it is the current value of the clock
    ;; for a football, it is the current size of the football (in
    ;; arbitrary units; bigger is more)
    toy-data
    ))

(define PlaygroundState<%>
  (interface (SWidget<%>)

    ;; ->Integer    
    ;;Returns the x and y co-ordinate of the target.
    target-x
    target-y
    
    ;; ->Boolean
    ;;Returns a boolean result which determines
    ;;whether the target is selected or not. 
    target-selected?
    
    ;; ->ListOfToys<%>
    get-toys
    ))  