;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname outlines) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;The code in this problem works on various representations of the outlines.
;;It checks  whether the representation is a legal flat representation or not.
;;The other function in the given code passes any list  given in the tree
;;outline and converts them to legal flat format. 


;; The modules required for the Problem.

(require rackunit)     ;;The inbuilt module that is used for applying testcases. 
(require "extras.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The deliverable functions for the given problem set.

(provide
 legal-flat-rep?
 tree-rep-to-flat-rep)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; DATA DEFINITIONS.

;; An Outline is a ListOfSection
;; A Section is a (make-section String ListOfSection)
;; INTERP: (make-section str secs) is a section where
;; str is the header text of the section
;; secs is the list of subsections of the section
;; List template for loOutline.
;;ListOfOutline is one of
;;--empty
;; represents the subsection associated with given section.

;; Templates for struct section.


;;List template for losubsections.
;;ListOfSubsections is one of
;;--empty
;; represents the subsection associated with given section.
;; -- (cons section LOSUBSECTIONS)
;; section-losubsections-fn: section-losubsections -> ??

;; (define (section-loln los)
;;  (cond

;;    [(empty? los) ...]
;;    [else (section-losubsections-fn (first los)
;;          (line-losubsections-fn (rest los))]))

;; section-struct
;; Constructor template
;; A section is a
;; (make-section String List)

;; Interpretation:
;; Heading is the simple data which goes on with the sections.
;; loSubsections is the list of subsections that are assosiated with our section
;; which are indeed a part of the section.

;; Destructor template:
;; section-fn: section -> ??
;; (define (section-fn n)
;;         (section-heading "n")
;;         (section-subsection "s")

(define-struct section (heading losubsections))


;;line-struct
;;Constructor template:
;;A line is a
;;(make-line  List string)

;;Interpretation
;; loln is the list of lines that are assosiated with our representation.
;;Heading is the simple data which goes on with the sections.
;;List template for loln.


;;ListOfline is one of
;;--empty
;; represents the subsection associated with given section.
;; -- (cons line LOLN)
;; line-loln-fn: line-loln -> ??

;; (define (line-loln loc)
;;  (cond

;;    [(empty? loc) ...]
;;    [else (line-loln-fn (first loc)
;;          (line-loln-fn (rest loc))]))


(define-struct line ( loln heading))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Constants:
;;Example Lists for testing legal-flat-rep?.
(define BASE 1)
(define flat-rep1 (list
                   (make-line (list 0 ) "The first section")
                   (make-line (list 1 1) "A subsection with no subsections")
                   (make-line (list 1 2 ) "Another subsection")
                   (make-line (list 1 2 1 ) "This is a subsection of 1.2")
                   (make-line (list 1 2 2) "This is another subsection of 1.2")
                   (make-line (list 1 3) "The last subsection of 1")
                   (make-line (list 2) "Another section")
                   (make-line (list 2 1) "More stuff")
                   (make-line (list 2 2) "Still more stuff")))

(define flat-rep (list
                  (make-line (list 1 ) "The first section")
                  (make-line (list 1 1 ) "A subsection with no subsections")
                  (make-line (list 1 2 ) "Another subsection")
                  (make-line (list 1 2 1 ) "This is a subsection of 1.2")
                  (make-line (list 1 2 2) "This is another subsection of 1.2")
                  (make-line (list 1 3) "The last subsection of 1")
                  (make-line (list 2) "Another section")
                  (make-line (list 2 1) "More stuff")
                  (make-line (list 2 2) "Still more stuff")))

(define flat-rep2 (list
                   (make-line (list 5 ) "The first section")
                   (make-line (list 1 1) "A subsection with no subsections")))

(define flat-rep3 (list
                   (make-line (list 1 ) "The first section")))

;;Example Lists for the conversion of the representations:

(define tree-rep (list
                  (make-section
                   "The first section"
                   (list
                    (make-section "A subsection with no subsections" empty)
                    (make-section
                     "Another subsection"
                     (list
                      (make-section "This is a subsection of 1.2" empty)
                      (make-section "This is another subsection of 1.2" empty)))
                    (make-section "The last subsection of 1" empty)))
                  (make-section "Another section"
                                (list
                                 (make-section "More stuff" empty)
                                 (make-section "Still more stuff" empty)))))

(define tree-rep1(list 
                  (make-section
                   "The first section"
                   (list
                    (make-section "A subsection with no subsections" '())
                    (make-section "Another subsection" '())))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Function Definition:
;;legal-flat-rep? : ListOfLine -> Boolean
;;GIVEN: a list of lines, like the one above
;;RETURNS: true iff it is a legal flat representation of an outline.
;;Strategy:Combine Simpler functions.
;;Examples:
;;(legal-flat-rep? flat-rep)
;;#true

;;Function Definition:
(define(legal-flat-rep? listoflines)
  (legal-flat-empty-checker? listoflines empty))

;;Tests:
(begin-for-test
  (check-equal?
   (legal-flat-rep? flat-rep)
   #true
   "Returns True as flat-rep is in legal flat representation")
  (check-equal?(legal-flat-rep? flat-rep1)
               #false
               "Returns false as flat-rep is not in legal flat representation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;legal-flat-empty-checker? :ListofLine->Boolean
;PURPOSE Statement
;;Given:The lists listrep and the last the list of the list in an representation.
;;Where:listrep the the representation of the list and
;;last represents the last of the previous list.
;;Returns:Returns true if any of the lists are empty else
;;forwards it for further tests through other defined functions. 
;;Strategy: Combine Simpler Functions.
;;Examples:
;;(legal-flat-empty-checker? empty empty)
;;#true

;;Function Definition:
(define (legal-flat-empty-checker? listrep last)
  (cond [(empty? listrep) true]
        [(empty? last) (legal-flat-empty-checker? (rest listrep)
                                                  (line-loln(first listrep)))]
        [else (compare-list? listrep (line-loln(first listrep)) last)] 
        ))

;;Tests:
(begin-for-test
  (check-equal?
   (legal-flat-empty-checker? empty empty)
   #true
   "Returns true as empty lists are passed")
  (check-equal?
   (legal-flat-empty-checker? flat-rep1 empty)
   #false
   "Returns true as empty lists are passed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;compare-list? :ListofOutlines ListOfLine ListOfLine ->Boolean
;PURPOSE Statement
;;Given:The lists listrep list1 list2.
;;Where:listrep list1 list2 are the representation of the list 
;;Returns:Returns true if any of the lists are empty else
;;forwards it for further tests through other defined functions. 
;;Strategy: Combine Simpler Functions.
;;Examples:
;;(compare-list? flat-rep flat-rep (list 1))
;; #false
;;Function Definition:
(define (compare-list? listrep list1 list2)
  (cond
    [(equal? (length list1) (length list2)) (compare-last? listrep list1 list2)]
    [(> (length list1) (length list2)) (check-valid-sub? listrep list1 list2)]
    [else (check-valid-sec? listrep list1 list2)]
    ))
;;Tests:
(begin-for-test
  (check-equal?
   (compare-list? flat-rep flat-rep (list 1))
   #false
   "Returns a false as no condition matches"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;check-valid-sub? :ListOfLine ListOfline ListOfLine->Boolean
;;Purpose Statement
;;Given: The lists listrep and li ,list2.
;;Where: listrep is the list with the list representations and list1 list2
;;are used for comparisons.
;;Returns:A boolean result after checking the conditions.
;;Strategy: Combine Simpler fuctions.
;;Examples:
;;(check-valid-sub? flat-rep flat-rep2  flat-rep3)
;;#false

(define (check-valid-sub? listrep list1 list2)
  (if (and (equal? (reverse (rest (reverse list1))) list2)
           (equal? 1 (first (reverse list1))))
      (legal-flat-empty-checker?(rest listrep) list1)
      false))

;;Tests:
(begin-for-test
  (check-equal?
   (check-valid-sub? flat-rep flat-rep2  flat-rep3)
   #false
   "Returns false as a result as the conditions do not match"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;check-valid-sec? ListofOutlines Listelement Listelement->false
;;Purpose Statement.
;;Given:The list listrep and theelements of the list for comparison.
;;Where :list1 list2 represents the list elements.
;;Returns:A boolean result after checking the conditions.
;;Strategy:Combine simpler function.
;;Example:
;;(check-valid-sec? (rest (rest flat-rep)) (line-loln( first flat-rep))
;;(line-loln (second flat-rep))) 
;;#false
(define (check-valid-sec? listrep list1 list2)
  (compare-last? listrep list1 (trim list2 (length list1))))
;;Test
(begin-for-test
  (check-equal?
   (check-valid-sec? (rest (rest flat-rep)) (line-loln( first flat-rep))
                     (line-loln (second flat-rep))) 
   #false
   "Returns the result after comparison")) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;trim :ListofOutlines length of the given Listof REP->ListofOutlines
;;PURPOSE Statement
;;Given:The lists listrep and triml. .
;;Where:listrep is the representation of the list and trim lis the length
;;of the list. 
;;Returns:Returns the given list till the given length.
;;Strategy: Combine Simpler Functions.
;;Examples:
;; (trim flat-rep 1)
;;(list (make-line (list 1) "The first section"))
;;Function Definition:

(define (trim l triml)
  (if(equal? triml 0)
     empty
     (cons (first l) (trim (rest l) (sub1 triml)))))
;;Tests:
(begin-for-test
  (check-equal?
   (trim flat-rep 1)
   (list (make-line (list 1) "The first section"))
   "Returns the given list till the given length."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;compare-last? :ListofOutlines ListofOutlines ListofOutlines ->Boolean
;;PURPOSE Statement
;;Given:The lists listrep list1 list2.
;;Where:listrep list1 list2 are the representation of the list 
;;Returns:Checks for the condition of the last elements in the given lists
;;if all conditions are satisfied forwards it for further tests
;;through other defined functions. 
;;Strategy: Combine Simpler Functions.
;;Examples:
;;(compare-last? flat-rep (list 1) (list 1))
;;#false
;;Function Definition:

(define (compare-last? listrep list1 list2)
  (if (equal? (sub1 (first (reverse list1))) (first (reverse list2)))
      (legal-flat-empty-checker? (rest listrep) list1) 
      false))

;;Tests:
(begin-for-test
  (check-equal?
   (compare-last? flat-rep (list 1) flat-rep)
   false
   "Returns false after comparison of the given lists"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;tree-rep-to-flat-rep : Outline -> FlatRep
;;GIVEN: the representation of an outline as a list of Sections
;;RETURNS: the flat representation of the outline
;;Strategy: Combine Simpler Functions.
;;Examples:
;;(tree-rep-to-flat-rep tree-rep1)
;;(list
;; (make-line (list 1) "The first section")
;; (make-line (list 1 1) "A subsection with no subsections")
;; (make-line (list 1 2) "Another subsection"))

(define (tree-rep-to-flat-rep ltreerep)
  (concat ltreerep empty 1))
;;Test:
(begin-for-test
  (check-equal?
   (tree-rep-to-flat-rep tree-rep1)
   (list
    (make-line (list 1) "The first section")
    (make-line (list 1 1) "A subsection with no subsections")
    (make-line (list 1 2) "Another subsection"))
   "Returns a list in tree rep converted to flat representation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;concat :ListofSections ListOfLines Int->ListofOutlines.
;;Given:A list of sections aListOfLines and an Integer
;;Where:listof sections comprises of sections listoflines
;;comprises of lines and start comprises of the integer value added.
;;Returns:A concatenated list which constitutes of list of
;;rep and pre and start.
;;Example:
;;(concat tree-rep1 flat-rep3 1)
;;(list
;;(make-line
;; (list (make-line (list 1) "The first section") 1)
;;  "The first section")
;; (make-line
;;  (list (make-line (list 1) "The first section") 1 1)
;;  "A subsection with no subsections")
;; (make-line
;;  (list (make-line (list 1) "The first section") 1 2)
;;  "Another subsection"))
(define (concat listrep pre start)
  (cond [(empty? listrep) empty]
        [else (append (create-sec (first listrep) pre start)
                      (concat(rest listrep) pre (add1 start)))]))

;;Tests:
(begin-for-test
  (check-equal? (concat tree-rep1 flat-rep3 1)
                (list
                 (make-line
                  (list (make-line (list 1) "The first section") 1)
                  "The first section")
                 (make-line
                  (list (make-line (list 1) "The first section") 1 1)
                  "A subsection with no subsections")
                 (make-line
                  (list (make-line (list 1) "The first section") 1 2)
                  "Another subsection"))
                "Returns a concatensted list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;create-sec ListOfSection ListofLines Integer->A ListOfLines.
;;Given:A list of sections a List and an Integer
;;Where:listof sections comprises of sections listoflines
;;comprises of lines and start comprises of the integer value added.
;;Returns:The subsection after being converted from tree rep to list rep.
;;Example:
;;(create-sec (make-section "A subsection with no subsections" empty)
;;(list 1) (list 1))           
;;(list
;; (make-line
;;  (list 1 (list 1))

;;  "A subsection with no subsections"))

(define (create-sec section pre start)
  (cons (make-line (add-to-list pre start) (section-heading section))
        (concat(section-losubsections section) (add-to-list pre start) BASE)))
;;Test:
(begin-for-test
  (check-equal?
   (create-sec (make-section "A subsection with no subsections" empty)
               (list 1) (list 1))             
   (list
    (make-line
     (list 1 (list 1))
     "A subsection with no subsections")))) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;add-to-list List List->List.
;;Given:A listofLine/sections and a list to be appended to the existing list.
;;Returns:The appended list .
;;Example:
;;(add-to-list tree-rep1 (list 1))
;;(list
;; (make-section
;;  "The first section"
;;  (list
;;   (make-section "A subsection with no subsections" '())
;;   (make-section "Another subsection" '())))
;; (list 1))

(define (add-to-list list listelement)
  (append list (cons listelement empty)))

;;Test:
(begin-for-test
  (check-equal?
   (add-to-list tree-rep1 (list 1))
   (list
    (make-section
     "The first section"
     (list
      (make-section "A subsection with no subsections" '())
      (make-section "Another subsection" '())))
    (list 1))
   "Returns an appended list"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
