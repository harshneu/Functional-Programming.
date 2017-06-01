;##########################################################################
;###########################################################################

#lang racket
(require rackunit)                     ;;Testing Module
(require "extras.rkt")                  ;;Module with inbuilt functions used in
;;the program scope.   
(require 2htdp/universe)               ;;Module used for bigbang functions.
(require 2htdp/image)                  ;;Module used to implement  images and
;;image functions.
(require "WidgetWorks.rkt")            ;;The framework delivered as a file
;;called WidgetWorks.rkt that provides three interfaces and one function

(provide StatefulInitial<%>
         StatefulBlock<%>
         Block<%>)

;##########################################################################
;;StatefulInitial Interface.

(define StatefulInitial<%> 
  (interface ()   
    ;; -> Scene
    ;; Returns a Scene depicting this Block<%> on it.
    add-to-scene
    ))




;##########################################################################

;;Interface StatefulBlock.

(define StatefulBlock<%> 
  (interface (StatefulWorld<%>) 
    ;;BLock<%> Integer Integer MouseEvent->BLock<%>
    ;;Given: An object for the world a mouse event and the x and y
    ;;coordinates for the mouse event.
    ;;Returns:A world after applying the other events of any..
    after-mouse-event
    ;; Scene-> Scene
    ;; Given: A scene 
    ;; Returns a Scene depicting this Block<%> on it.
    add-to-scene
    ;; Block-x Int-> Int
    block-x
    ;; Block-y Int-> Int
    block-y
    ;; Block-color Color->Color
    block-color
    ;;block-selected block ->Boolean
    ;;Given: A block
    ;;Returns:True iff the block is selected else false.
    block-selected?))


;##########################################################################

;##########################################################################

;; INTERFACE Block.


(define Block<%>
  (interface (SWidget<%>)  
    add-teammate
    block-x
    block-y
    ;; -> ListOfBlock<%>
    ;; RETURNS: list of blocks.
    get-team))