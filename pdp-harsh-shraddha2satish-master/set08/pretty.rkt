;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname pretty) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; The code is based on implementing a system in which the expressions
;; are returned as pretty printed expression.The codes are implemented
;; such that the expressions implemented are in proper implementation.


(require rackunit)       ;; inbuilt module for implementing testing.
(require "extras.rkt")   ;; a module file imported so as some  predefined
;; functions can be used directly in this peice of code.
(require htdp/error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Provides for testing
(provide
 expr-to-strings
 make-sum-exp
 sum-exp-exprs
 make-diff-exp
 diff-exp-exprs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DATA DEFINITIONS 

;;SUM-EXP
(define-struct sum-exp (exprs))

;;Constructor Template:
;; A Sum-Exp is a
;;(make-sum-exp NELOEXPR)

;;Interpretation
;;exprs are the non empty list of Expr.

;;Destructor Template:

;;sum-exp-fn :sum-exp->??
;;(define (sum-exp-fn n)
;;  (..
;;     (sum-exp-exprs n)))
;; n represents list of non-empty list of expressions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;DIFF-EXP
(define-struct diff-exp (exprs))

;;Constructor Template: 
;; A Diff-Exp is a
;;(make-diff-exp NELOEXPR)

;;Interpretation
;;exprs are the non empty list of Expr.

;;Destructor Template:

;;diff-exp-fn :diff-exp->??
;;(define (diff-exp-fn n)
;;  (..
;;     (diff-exp-exprs n)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A LOExpr is one of
;; empty
;; (cons EXPR LOExpr)

;;List Template for LOExpr
;;(define (loexpr-fn el)
;;  (cond
;;    [(empty? el) ..]
;;    [else (..
;;           (expr-fn (first el))
;;           (loexpr-fn (rest el)))]))


;;List Template for NELOExpr
;;NELOExpr-fn :NELOExpr->??
;;(define (NELOExpr-fn nel)
;;  (cond
;;    [(empty? nel)(NELOExpr-fn first nel)]
;;    [else (..
;;           (first nel)
;;           (NELOExpr-fn (rest nel)))]))


;; A ListOfString(LOS) is one of
;; --empty
;; --(cons String LOS)

;;List Template for List of String.
;;los-fn :LOS->??
;;(define (los-fn los)
;;  (cond
;;  [(empty? los)..]
;;  [else (..
;;           (first los)
;;           (los-fn (rest los)))]))

;;Template for Equality-Tests:
;;(begin-for-test 
;;  (check-equal?
;;   (fn ...
;;   Result
;;   "Test Message")).

;;Template for testing error:
;;(begin-for-test
;;  (check-error
;;    (fn...
;;     "Test Message"))


;;CONSTANTS

(define SUM-OPERATOR "+")
(define DIFFERENCE-OPERATOR "-")
(define DIVISION-OPERATOR /)
(define OPEN-BRACKETS "(")
(define CLOSED-BRACKETS ")")
(define SINGLE-SPACE " ")
(define STRING-OPERATOR "")
(define EXPRESSION-SPACE "   ")
(define ERROR-POP-UP "Not Enough Room")
(define INVALID-ENTRY "BAD OPERATOR")
(define SUM-APPEND-EXPR "(+")
(define DIFF-APPEND-EXPR "(-")
(define CHECK-SPACE >=)
(define TEST-ERROR 10)
(define CHECK-INT 4)
(define WIDTH-OPER 15)
(define ISE-INT 10)
(define LESS-WIDTH 1)
(define THRESHOLD 3)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;Examples for Test Cases:

(define MAKE-PRETTY-SUM
  (make-sum-exp
   (list
    (make-diff-exp (list 5 8 13 21))
    (make-diff-exp
     (list
      (make-sum-exp(list 34 55 89))
      (make-diff-exp (list 23 29))
      (make-diff-exp (list 61 67))))
    (make-sum-exp (list 5 4 18 54)))))


(define MAKE-PRETTY-SUM-LIST
  (list
   (make-diff-exp (list 5 8 13 21))
   (make-diff-exp
    (list
     (make-sum-exp(list 34 55 89))
     (make-diff-exp (list 23 29))))))


(define TEST-MAKE-PRETTY-SUM-LIST " (- 5 8 13 21) (- (+ 34 55 89) (- 23 29))" )

(define MAKE-PRETTY-DIFF
  (make-diff-exp
   (list
    (make-diff-exp (list 5 8 13 21))
    (make-sum-exp
     (list
      (make-sum-exp (list 34 55 89))
      (make-diff-exp (list 23 29))
      (make-sum-exp (list 60 70))))
    (make-diff-exp (list 5 4 17 3)))))

(define TEST-RESULT-N->S  "4")
(define TEST-RESULT-DISPLAY-EXPR 17)
(define TEST-RESULT-EX (+ (- 5 8 13 21) (- (+ 34 55  89) (- 23 29)
                                           (- 60 70)) (+ 5 4 17 3)))
(define TEST-EXPR (make-sum-exp (list 1 3 5 8 13 21)))

(define TEST-EXPR2 (make-diff-exp (list 1 3 5 8 13 21)))

(define TEST-SUM-LIST  (list "1" "3" "5" "8" "13" "21"))

(define TEST-EXPR-FIBONACCI "(+ 1 3 5 8 13 21)")

(define TEST-EXPR-DIFF-FIBONACCI "(- 1 3 5 8 13 21)")

(define TEST-SPACE (list "   4" "   9)"))

(define EXPR-SPACE (list "4" "9"))

(define TEST-CONCAT (list "(e w" "   3)"))

(define SINGLE-TEST-EXPR (list "1" "3" "5" "8" "13" "21"))
(define TEST-LIST (list "5" "4" "8"))
(define OEXPR (list 5 4 8))
(define CONCAT-LIST-CHAR (list "w" "3"))
(define APPEND-CHAR "e")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;;expr-to-strings : Expression Int -> ListOfString.
;;Given: An expression and a width.
;;Where:width is the space available for the expression to fit.
;;Returns: An output with the expression returned as a pretty prited document
;;the length of the single expression should never exceed the width.
;;Design Strategy: Divide into cases.
;;Example:
;;(expr-to-strings TEST-EXPR 100)
;;(list "(+ 1 3 5 8 13 21")

;;Function Definition:
(define (expr-to-strings expr width)
  (if (space-available expr width)
      (list (expr-to-strings-type-checker expr))
      (multi-line-expression expr width)))


;;multi-line-expression:Expression INt->ListOfString.
;;Given:An expression and a width
;;Where:width is the space available for the expression to fit.
;;Returns:An output with the expression returned as a pretty prited document
;;the length of the single expression should never exceed the width.
;;Design Strategy:Commbine simpler functions.
;;Example:
;;(expr-to-strings-type-checker TEST-EXPR)
;; TEST-EXPR-FIBONACCI

;;Function Definition:
(define (multi-line-expression expr width)
  (concat-operator (make-indented-expression expr width)
                   (expression-operator expr)))

;;Tests:
(begin-for-test 
  (check-equal?
   (expr-to-strings-type-checker TEST-EXPR)
   TEST-EXPR-FIBONACCI
   "Returns The given expression in String translation"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;space-available:Expression INt->ListOfString.
;;Given:An expression and a width
;;WHERE:width is the space available for the expression to fit.
;;Returns:An output with the expression returned as a pretty prited document
;;the length of the single expression should never exceed the width after checking
;;for width and length of the string.
;;Design Strategy:Commbine simpler functions.
;;Example:
;;(expr-to-strings-type-checker TEST-EXPR)
;; TEST-EXPR-FIBONACCI

;;Function Definition.
(define (space-available expr width)
  (CHECK-SPACE width (string-length (expr-to-strings-type-checker expr))))



;;expression-operator: Expression->String
;;Given:Any expression.
;;Returns: A string which comprises of the operator for the expression
;;or the error message if any.
;;Example:
;;(expression-operator TEST-EXPR2)
;;DIFFERENCE-OPERATOR
;;Design Strategy:Divide into cases.

;;Function definition
(define (expression-operator expression)
  (cond
    [(number? expression)(on-error ERROR-POP-UP)]
    [(sum-exp? expression) SUM-OPERATOR]
    [(diff-exp? expression) DIFFERENCE-OPERATOR]
    [else INVALID-ENTRY]))

;;Tests:
(begin-for-test
  (check-equal?
   (expression-operator TEST-EXPR2)
   DIFFERENCE-OPERATOR
   "Returns the operator for a given expression")
  (check-equal?
   (expression-operator TEST-EXPR)
   SUM-OPERATOR
   "Returns the operator for the given expression")
  (check-equal?
   (expression-operator DIVISION-OPERATOR)
   INVALID-ENTRY
   "Returns invalid entry for the wrong input"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;expr-to-strings-type-checker:Expression->String
;;Given: An expression.
;;Returns: Translates the given expression into string notation after applying
;;the helper functions.
;;Example:
;;(expr-to-strings-type-checker /)
;;"BAD OPERATOR"
;; (expr-to-strings-type-checker TEST-EXPR)
;; "(- 1 3 5 8 13 21)"
;; Design Strategy:Divide into cases.

;;Function Definition:
(define (expr-to-strings-type-checker expression)
  (cond
    [(number? expression)(number-to-strings expression)]
    [(sum-exp? expression)(sum-string expression)]
    [(diff-exp? expression)(difference-string expression)]
    [else INVALID-ENTRY]))

;;Tests:
(begin-for-test
  (check-equal?
   (expr-to-strings-type-checker TEST-EXPR)
   TEST-EXPR-FIBONACCI
   "Returns the string for a given expression")
  (check-equal?
   (expr-to-strings-type-checker TEST-EXPR2)
   TEST-EXPR-DIFF-FIBONACCI
   "Returns the string for a given expression")
  (check-equal?
   (expr-to-strings-type-checker DIVISION-OPERATOR)
   INVALID-ENTRY
   "Returns the string for a given expression"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;add-space: LoS->LoS
;;Given: A list of string
;;Returns:The list of string with the added spaces to the string.
;;Example:
;;(add-space EXPR-SPACE)
;;TEST-SPACE
;;Strategy:Use template on lists.

;;Function Definition:
(define (add-space nel)
  (cond
    [(empty? (rest nel))
     (list (string-append EXPRESSION-SPACE (first nel)CLOSED-BRACKETS))]
    [else (appended-string nel)]))


;;appended-string :LOS->LOS
;;Given:A list of string(non empty)
;;Returns:The appended list wit added space to the string.
;;Example:
;;(appeded-string EXPR-SPACE)
;;TEST-SPACE
;;Design Strategy: Use template on lists.

;;Function Definition:
(define (appended-string nel)
  (append (list (string-append EXPRESSION-SPACE (first nel)))
          (add-space (rest nel))))

;;Tests:
(begin-for-test
  (check-equal?
   (add-space EXPR-SPACE)
   TEST-SPACE
   "Returns the list after adding space"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; convert-expression-to-strings NELOExpr->String
;; Given: A non empty list of expression.
;; Returns:Translates the given list of expression into a string.
;; Example:
;;(convert-expression-to-strings (list "+"))
;;" BAD OPERATOR"
;; Design Strategy:Use HOF foldl on NELOExpr.

(define (convert-expression-to-strings loe)
  (foldl
   ;;Expr String->String.
   ;;Given:an expression and the iterative string.
   ;;Returns:A string representation of the given expression.
   (lambda (expression current-string)
     (string-append current-string
                    SINGLE-SPACE
                    (expr-to-strings-type-checker expression)))
   STRING-OPERATOR
   loe))

;;Tests:
(begin-for-test
  (check-equal?(convert-expression-to-strings MAKE-PRETTY-SUM-LIST )
               TEST-MAKE-PRETTY-SUM-LIST
               "Returns the string format of the given string"))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;sum-string :Sumexp->String
;;Given: Any sum expression.
;;Returns: Translates a sum expression to a string.
;;Examples:
;;(sum-string TEST-EXPR)
;;"(+ 1 3 5 8 13 21)"
;;Design Strategy:Combine Simpler function.

;;Function Definition:
(define (sum-string expr)
  (string-append SUM-APPEND-EXPR (convert-expression-to-strings
                                  (sum-exp-exprs expr))CLOSED-BRACKETS))
;;Tests:
(begin-for-test
  (check-equal?
   (sum-string TEST-EXPR)
   TEST-EXPR-FIBONACCI
   "Returns the sum string as is"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;difference-string :Diffexp->String
;;Given: Any diff expression.
;;Returns: Translates a difference expression to a string.
;;Examples:
;;(difference-string TEST-EXPR2)
;;TEST-EXPR-DIFF-FIBONACCI
;;Design Strategy:Combine Simpler function.

;;Function Definition:
(define (difference-string expr)
  (string-append DIFF-APPEND-EXPR (convert-expression-to-strings
                                   (diff-exp-exprs expr))CLOSED-BRACKETS))
;;Tests:
(begin-for-test
  (check-equal?
   (difference-string TEST-EXPR2)
   TEST-EXPR-DIFF-FIBONACCI
   "REturns the sum string as is"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;on-error :Any->errormessage
;; Given: Any entry.
;; Returns: Returns an error messages as per the popup.
;;Example:
;; (on-error 10)
;;Not Enough ROOM
;;Design Strategy: Use simpler functions.


;;Function Definition:
(define (on-error n)
  (error ERROR-POP-UP))

;;Tests:
(begin-for-test
  (check-error
   (on-error TEST-ERROR)
   "Returns Error"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;concat-operator:LOS String ->LOS
;;Given: An expression operator and a list of string.
;;Returns: Concatenates the expression-operator to the given list of string.
;;Example:
;;(concat-operator (list "w" "3") "e")
;;(list "(e w" "   3)")
;;Design Strategy: Apply template on list.


;;Function Definition:
(define (concat-operator los string)
  (append (list (string-append OPEN-BRACKETS string SINGLE-SPACE
                               (first los)))(add-space (rest los))))

;;Tests:
(begin-for-test
  (check-equal?
   (concat-operator CONCAT-LIST-CHAR APPEND-CHAR)
   TEST-CONCAT 
   "Returns the list of string after concatenation"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;make-indented-expression :Expression Int->ListOfString.
;;Given:An expression and width as minimum space.
;;WHERE:width is the space available for the expression to fit.
;;Returns:A list of string with every element implemented as a single element.
;;Example:
;;(make-indented-expression TEST-EXPR 10)
;;(list "1" "3" "5" "8" "13" "21")

;;Strategy:Divide into cases.

;;Function Definition:
(define (make-indented-expression expression width)
  (cond
    [(sum-exp? expression)(indent-sum-expression expression width)]
    [(diff-exp? expression)(indent-difference-expression expression width)]
    [else INVALID-ENTRY]))

;; Tests:
(begin-for-test
  (check-equal?
   (make-indented-expression TEST-EXPR ISE-INT)
   SINGLE-TEST-EXPR
   "REturns every element as a single element")
  (check-equal?
   (make-indented-expression TEST-EXPR2 ISE-INT)
   SINGLE-TEST-EXPR
   "REturns every element as a single element")
  (check-equal?
   (make-indented-expression "DIVISION-OPERATOR" ISE-INT)
   INVALID-ENTRY
   "REturns every element as a single element"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;indent-sum-expression: SumExp Int ->ListOfString.
;;Given:A sum expression and width as minimum space.
;;WHERE:width is the space available for the expression to fit.
;;Returns:A list of strings after applying the operations in the function.
;;Example:
;;(indent-sum-expression TEST-EXPR ISE-INT)
;;(list "1" "3" "5" "8" "13" "21")
;;(indent-sum-expression TEST-EXPR 2)
;;Not Enough ROOM
;;Strategy:Combine simpler functions.

;;Function Definition:

(define (indent-sum-expression expression width)
  (convert-operational-expression (sum-exp-exprs expression) (- width THRESHOLD)))

;;Tests:
(begin-for-test
  (check-equal?
   (indent-sum-expression TEST-EXPR ISE-INT)
   TEST-SUM-LIST
   "Returns the list with every element as a seprate list")
  (check-error(indent-sum-expression TEST-EXPR LESS-WIDTH)
              "Not Enough ROOM"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;indent-difference-expression: DiffExp Int ->ListOfString.
;;Given:A difference expression and width as minimum space.
;;WHERE:width is the space available for the expression to fit.
;;Returns:A list of strings after applying the operations in the function.
;;Example:
;;indent-sum-expression TEST-EXPR 10)
;; TEST-SUM-LIST
;;Strategy:Combine simpler functions.

;;Function Definition:
(define (indent-difference-expression expression width)
  (convert-operational-expression (diff-exp-exprs expression)(- width THRESHOLD)))

;;Tests:
(begin-for-test
  (check-equal?
   (indent-sum-expression TEST-EXPR ISE-INT)
   TEST-SUM-LIST
   "Returns the list for the sum expression")
  (check-error
   (indent-sum-expression TEST-EXPR 1)
   "Returns the error message "))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;convert-operational-expression:LOExpr Int ->ListOfString.
;;Given: An expression and width as minimum space.
;;WHERE:width is the space available for the expression to fit.
;;Returns:Append the ecxpression-operator to the given list of expression.
;;Example:
;;(indent-operational-expression (list 5 4 8) 15)
;;(list "5" "4" "8")
;;Strategy :Use template on lists.

;;Function Definition:
(define (convert-operational-expression nel width)
  (cond
    [(empty? (rest nel)) (expr-to-strings (first nel) width)]
    [else (append-operational-expression nel width)]))

;;Tests:
(begin-for-test
  (check-equal?
   (convert-operational-expression OEXPR WIDTH-OPER)
   TEST-LIST
   "Returns the list as a list of string"))

;;append-operational-expression LOExpr Int ->ListOfString.
;;Given:An expression and width as minimum space.
;;WHERE:width is the space available for the expression to fit.
;;Returns:Appends the expression operator to the given list of expression
;;as an string.
;;Example:
;;(append-operational-expression (list 5 4 8) 15)
;;(list "5" "4" "8")
;;Strategy: Apply operation on list.

;;Function Definition:
(define (append-operational-expression nel width)
  (append
   (expr-to-strings (first nel) width) 
   (convert-operational-expression (rest nel) width)))

;;Tests:
(begin-for-test
  (check-equal?
   (append-operational-expression OEXPR WIDTH-OPER)
   TEST-LIST
   "Returns the given list as a list of string"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;number-to-string Int ->String
;;Given: A number.
;;Returns:translates the number into a string.
;;Example:
;;(number-to-strings 4)
;;"4"
;;Design Strategy:Use simpler function.

;;Function Design.
(define (number-to-strings n)
  (number->string n))

;;Tests:
(begin-for-test
  (check-equal?
   (number-to-strings CHECK-INT)
   TEST-RESULT-N->S
   "Returns te number in an string form"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;display-expr :expression Int->expression
;;Given :An expression and its width.
;;Returns: A expression in the display string format as a pretty printed text.
;;Design Strategy: Combine simpler functions.
;;Example:
;;(display-expr MAKE-PRETTY 15)
;; TEST-RESULT-EX

;;Function Definition:
(define (display-expr expr n)
  (display-strings! (expr-to-strings expr n)))
;;Tests:
(begin-for-test
  (check-false
   (equal? (display-expr (make-sum-exp OEXPR ) WIDTH-OPER)
           TEST-RESULT-DISPLAY-EXPR)
   "Returns the document as a pretty printed one"))

