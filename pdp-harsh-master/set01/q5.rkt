;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Set01 Question5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")

(provide string-delete)
;Given:A string and an number at a position.
;Returns:A string str after deleting the element at the ith index.
; string-delete : String String -> String 
; The function consumes a string and a number i which  deletes the ith position from the given string str. 
; Tests/Examples.
(begin-for-test 
  (check-equal? (string-delete "Harsh" 1) 
                "Hrsh" 
                "deleting character at index 1 of Harsh must give Hrsh") 
  (check-equal? (string-delete "Harsh" 0) 
                "arsh" 
                "deleting character at index 0 of Harsh must give arsh")
    (check-equal? (string-delete "Harsh" 2) 
                "Hash" 
                "deleting character at index 2 of Harsh must give Harh") 
  (check-equal? (string-delete "Harsh" 3) 
                "Harh" 
                "deleting character at index 3 of Harsh must give Harh")) 
(define (string-delete str i) 
(string-append (substring str 0 i) (substring str (+ 1 i))))
