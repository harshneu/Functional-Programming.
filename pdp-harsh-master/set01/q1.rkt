;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |set01 question1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide distance-to-origin)
;Given:The coordinates as a point.
;Returns:The distance between two points in this case the second point is always the origin(0,0)
; distance-to-origin : Real Real -> NonNegReal 
; Calculates the distance between points (x1 y1) to (x2 y2) :For origin use points (x2 y2)as (0 0)


(begin-for-test(check-equal? (distance-to-origin 7 0 24 0) 
                25
                "distance between (7,24)and  origin must be equal to 25")
 (check-equal? (distance-to-origin 3 0 4 0) 
                5
                "distance of (3,4) to origin should be 5")) 


(define (distance-to-origin x1 x2 y1 y2 )
 (sqrt(+(*(- x2 x1)(- x2 x1))(*(- y2 y1)(- y2 y1)))))
