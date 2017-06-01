;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Set01 question4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require rackunit)
(require "extras.rkt")
(provide string-insert)
;Given:A string str and an index no.i.
;Returns :String with an added symbol'_ at ith index.

; string-insert : String String -> String 
; The function string-insert consumes a string and a number i and inserts "_"  at the ith position of the string str.
;Test/Examples.
(begin-for-test 
  (check-equal? (string-insert "Harsh" 1) 
                "H_arsh" 
                "inserting a character to Harsh at index 1 must give H_arsh as an output") 
  (check-equal? (string-insert "Shukla" 2) 
                "Sh_ukla" 
                "inserting a character to Shukla  at index 2 should return Sh_ukla as an output")) 
(define (string-insert str i) 
(string-append (substring str 0 i)"_"(substring str i ))) 

