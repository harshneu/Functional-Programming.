;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Set01 question2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require "extras.rkt")
(require rackunit)
(provide string-first)
;Given: A string and an index.
;Returns:The character at the indexed position.
;Ref represents the reference to the letter or character at the specified index.

(begin-for-test
  (check-equal? (string-first "Harsh")
                             #\H
          "First string should return H as the character.")
          
    (check-equal? (string-first "Shukla")
                             #\S
          "First string should return S as the character."))
(define (string-first str) 
  (string-ref str 0))