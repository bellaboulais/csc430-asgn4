#lang typed/racket
(require typed/rackunit)

;; Assignment 4

;; DATA DEFINITIONS:

; textbook definition for a tstruct
(define-syntax tstruct
  (syntax-rules ()
    [(_ name fields)
     (struct name fields #:transparent)]))
(tstruct fdC ([name : Symbol] [args : (Listof Symbol)] [body : ExprC]))


; Define the ExprC language that utilizes a binop data type
(define-type ExprC (U numC idC strC ifC locals lamC appC))                 
(tstruct numC ([n : Real]))                           
(tstruct idC ([name : Symbol]))                          
(tstruct strC ([str : String]))
(tstruct ifC ([test : ExprC] [then : ExprC] [else : ExprC]))
(tstruct locals ([bindings : Clauses] [body : ExprC]))
(tstruct lamC ([arg : Symbol] [body : ExprC]))
(tstruct appC ([f : ExprC] [args : (Listof ExprC)]))

; inside locals... ex. : add6 = {curradd 6} : 
(tstruct clause ([id : Symbol] [expr : ExprC]))

(define-type Value (U numV boolV strV closV primV))
(tstruct numV ([n : Number]))
(tstruct boolV ([b : (U #t #f)]))
(tstruct strV ([s : String]))
(tstruct closV ([arg : Symbol] [body : ExprC] )) ; [env : Env]
(tstruct primV ([p : Symbol]))

; top level environemnt primitive that checks for +, -, *, /, <=, equal?, true, false, error v
; need a eval prim helper function? parsed as appC and interpreted as a primitive


;; FUNCTIONS

; top-interpt NOT COMPLETE
(: top-interp (Sexp -> String)) 
;   PARAMS:  s : Sexp
;   RETURNS: String
;   PURPOSE:  
(define (top-interp s)
  "hi")
 ; (serialize (interp (parse s) top-env)))

(check-equal? (top-interp 'hi) "hi")



; interp
(: interp (ExprC -> Value))
;   PARAMS:   e:    ExprC           the expression to interpret
;             env:  Envrionment     list of functions defined in the current environment
;   RETURNS:  Value
;   PURPOSE:  
(define (interp e)
  (match e                                                                     
    [(numC n) (numV n)]                                                                
    [(idC _) (error 'interp "ZODE: shouldn't get here")]
    ))



; (define-type ExprC (U numC idC strC ifC locals lamb appC))         
; parse  
(: parse (Sexp -> ExprC))
;   PARAMS:   s :  Sexp
;   RETURNS:  ExprC
;   PURPOSE:  
(define (parse s)
  (match s
    [(? real? n) (numC n)]                            
    [(? symbol? s) (idC s)] ; variable reference cant be named operator 
    [(? string? str) (strC str)]
    ;[(list 'locals ': clause ': body)]
    
    
    [else (error 'parse "ZODE: Invalid expression")]))  



; parse test cases desugars locals into lambs?
(check-equal? (parse 3) (numC 3))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse "30") (strC "30"))


(check-equal? (parse '{locals : x = 2 : y = 6 : {+ x y}})
              (appC (lamC (clause 'x (numC 2) (clause 'y (numC 6))
                          (appC (idC '+) (list (idC 'x) (idC 'y)))))
                    (list (numC 2) (numC 6))))
              ;{{lamb : x y : {+ x y}} 2 6})

(check-equal? (parse '{locals : z = {+ 9 14} : y = 98 : {+ z y}})
              (appC (lamC (clause (idC 'z) (idC 'y))
                          (appC (idC '+) (list (idC 'z) (idC 'y))))
                    (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
              ; {{lamb : z y : {+ z y}} {+ 9 14} 98})







