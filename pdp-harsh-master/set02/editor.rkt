;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(check-location "02" "editor.rkt")
(require rackunit)
(require 2htdp/universe)
(require "extras.rkt")


(provide
 make-editor
 editor-pre
 editor-post
 edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;  Data Definition
;;  Whenever a key is pressed Keyevent records the event and
;;    holds the record for it.
;;  "\b":deletes the last character before the cursor.
;;  "left":Moves the cursor to the left of the present cursor position.
;;  "right":Moves the cursor to the right of the present cursor position.


;;   Template:
;;   key-event : KeyEvent -> ??
;;   (define (key-event k)
;;   (cond
;;       [(= (string-length k)1)....]
;;       [(string-length k "\b")....]
;;       [(string-length k "left"))....]
;;       [(string-length k "right"))....]


    (define-struct editor [pre post])
    (make-editor "Hello" "World")


;;  A editor is a (make-editor String String)
;;  Interp:Pre is variable placed as the first string before the cursor.
;;         Post is variablewhich is placed as the first string after the cursor.
;;  machine-fn:Editor->String.
;;  (define (editor-fn n)
;;  (..
;;   (editor-pre n)
;;   (editor-pre n))

    (define p1 (make-editor "Hello" "World"))

;;  make-editor :String->String.
;;  Given:Two strings givenn as pre and post.
;;  Returns:String with the applied operation on it.
;;  strategy:Functional Composition.

    (define (edit ed ke)
      (cond
        [(key=? "\b" ke)(make-editor (str-delete-last (editor-pre ed))
                                                    (editor-post ed))]
        [(key=? "left" ke)(str-move-left ed)]
        [(key=? "right"ke)(str-move-right ed)]
        [else (make-editor (string-append (editor-pre ed))
                                      (editor-post ed))]))
    

;;  str-move-right :String->String.
;;  Given:   A string with pre and post variables.
;;  Returns: String which returns a string with applied operation
;;  i.e. cursor moves right.
;;  strategy:Functional Composition.



    (define (str-move-right ed)
      (if (>= (string-length (editor-post ed))1)
          (make-editor (string-append (editor-pre ed)
                                      (string-first (editor-post ed)))
                       (string-delete (editor-post ed)))
          (unchanged-editor ed)))
    
    
;;  str-move-left:String->String.
;;  Given:A string with pre and post variables.
;;  Returns:String which returns a string with applied operation
;;                                          i.e cursor moves left.


    (define (str-move-left ed)
      (if(>= (string-length (editor-pre ed))1)
         (make-editor (str-delete-last (editor-pre ed))
                   (string-append (string-end (editor-pre ed))(editor-post ed)))
         (unchanged-editor ed)))
    
    
    
;;  str-delete-last :String->String.
;;  Given: A string with the preand post variables.
;;  Returns:String which returns a string the applied functionality(back space)
;;  eliminates the lst character before the cursor i.e. last character of pre.


    (define (str-delete-last str) 
      (if (= (string-length str) 1) 
          str
          (substring str 0 (- (string-length str) 1))))
    
    
;;  string-first :String->String
;;  Given:Text as a string .
;;  Returns:First string of the given string in the editor.


    (define (string-first str)
      (substring str 0 1))
    
    
;;  string-end:String->String.
;;  Given:Text as a string.
;;  Returns:last characterof the string.

;;  FUNCTION DEFINITION :


    (define (string-end str)
      (substring str (-(string-length str)1)(string-length str)))
    
    

;;   EXAMPLES:(string-delete-last "Hello") "Hell"
;;   string-delete String->String.
;;   Given :Text as a string.
;;   Returns:String after removing the last character.
;;   EXAMPLES:(string-remove-last "Hello") "Hell"


;;   Function Definition    


    (define (string-delete str ) 
      (substring str 1 (string-length str)))
    
    
;;  string-delete-firstString->String.
;;  Given : Text as a string.
;;  Returns: String after removing the first character.
;;  Examples:(string-remove-last "Hello") "ello"

;;  Function Definition


    (define (string-delete-first st) 
      (substring st 1 (string-length st)))
    
    
;;  unchanged-editor Editor ->Editor
;;  Given :an editor.
;;  Returns: an editor with no change.


;;  Function Definition


    (define (unchanged-editor ed) 
      (make-editor (editor-pre ed)(editor-post ed)))
    
    
;;  Tests/Examples:

    (begin-for-test
      (check-equal? (edit p1 "left")(make-editor "Hell" "oWorld"))
      (check-equal? (edit p1 "right")(make-editor "HelloW" "orld"))
      (check-equal? (edit p1 "\b")(make-editor "Hell" "World"))
      (check-equal? (edit (make-editor "North" "Eastern") "left")
                                  (make-editor "Nort""hEastern"))
      (check-equal? (edit (make-editor "North" "Eastern") "\b")
                                 (make-editor "Nort""Eastern"))
      (check-equal? (edit (make-editor "North" "Eastern") "right")
                                   (make-editor "NorthE""astern"))
      (check-equal? (edit (make-editor "North" "Eastern") "\a")
                                (make-editor "North""Eastern"))
      (check-equal? (edit (make-editor "North" "Eastern") "\r")
                                (make-editor "North""Eastern"))
      (check-equal? (edit (make-editor "North" "") "up")
                               (make-editor "North" ""))
      (check-equal? (edit (make-editor "N" "") "up")
                               (make-editor "N" ""))
      (check-equal? (str-delete-last "a")"a")
      (check-equal? (unchanged-editor (make-editor "Hello""World"))
                                     (make-editor "Hello" "World"))
      (check-equal? (string-delete-first "hello")"ello" )
      (check-equal? (str-move-right (make-editor """"))
                                   (make-editor "" ""))
      (check-equal? (str-move-left (make-editor """"))
                                 (make-editor "" "")))
    
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


          
    

  