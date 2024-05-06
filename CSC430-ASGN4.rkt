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


; Define the ExprC for the ZODE4 language
(define-type ExprC (U numC idC strC ifC locals lamC appC))                 
(tstruct numC ([n : Real]))                           
(tstruct idC ([name : Symbol]))                          
(tstruct strC ([str : String]))
(tstruct ifC ([test : ExprC] [then : ExprC] [else : ExprC]))
(tstruct locals ([bindings : Clauses] [body : ExprC]))
(tstruct lamC ([arg : Symbol] [body : ExprC]))
(tstruct appC ([f : ExprC] [args : (Listof ExprC)]))

(define-type Clauses (U clause clauses)) ; inside locals... ex. : add6 = {curradd 6} : 
(tstruct clause ([id : Symbol] [expr : ExprC]))
(tstruct clauses ([id : Symbol] [expr : ExprC] [rest : Clauses]))

(define-type Value (U numV boolV strV closV primV)) ; Value type represents an evaluated expression
(tstruct boolV ([b : (U #t #f)]))                   ; boolV: boolean value
(tstruct strV ([s : String]))                       ; strV: string value
(tstruct closV ([arg : Symbol] [body : ExprC] [env : Env])) ; closV: closure value
(tstruct primV ([p : Symbol]))                      ; primV: primitive value
(tstruct numV ([n : Number]))                       ; numV: number value

(define-type Env (Listof (List Symbol Value)))

(define top-env (list (list '+ (primV '+))
                      (list '- (primV '-))
                      (list '* (primV '*))
                      (list '/ (primV '/))
                      ; (list '<= (primV '<=))
                      ; (list 'equal? (primV 'equal?))
                      ; (list 'true (boolV #t))
                      ; (list 'false (boolV #f))
                      ; (list 'error (primV 'error))
                      ))

; top level environemnt primitive that checks for +, -, *, /, <=, equal?, true, false, error v
; need a eval prim helper function? parsed as appC and interpreted as a primitive

;; FUNCTIONS

; lookup NOT COMPLETE (needs more test cases)
;  PARAMS:  x : Symbol
;           env : Env
;  RETURNS: Value
;  PURPOSE: lookup the value of x in the environment env
(: lookup (Symbol Env -> Value))
(define (lookup x env)
  (cond
    [(empty? env) (error 'lookup "ZODE: Variable not found")]
    [(eq? x (first (first env))) (second (first env))]
    [else (lookup x (rest env))]))

; extend-env NOT COMPLETE (needs more test cases)
;  PARAMS:  clauses : Clauses
;           env : Env
;  RETURNS: Env
;  PURPOSE: extend the environment env with the bindings in clauses
(: extend-env (Clauses Env -> Env))
(define (extend-env clauses env)
  (cond
    [(empty? clauses) env]
    [else (extend-env (rest clauses) 
                      (cons (list (clause-id (first clauses)) 
                                  (interp (clause-expr (first clauses) env))) 
          env))]))
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
(: interp (ExprC Env -> Value))
;   PARAMS:   e:    ExprC           the expression to interpret
;             env:  Envrionment     list of functions defined in the current environment
;   RETURNS:  Value
;   PURPOSE:  
(define (interp e env)
  (match e                                                                     
    [(numC n) (numV n)]  
    [(idC x) (lookup x env)]
    [(strC s) (strV s)]
    [(ifC test then else) (if (eq? (interp test env) (boolV #t))
                            (interp then env)
                            (interp else env))]
    [(locals clauses body) (interp body (extend-env clauses env))]
    [(lamC arg body) (closV arg body)]
    [(appC f args) (apply (interp f env) (map (λ (arg) (interp arg env)) args))]
    [else (error 'interp "ZODE: Invalid expression")]))




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
    
    
    [else (error 'parse "ZODE: Invalid expression")]))  



; parse test cases desugars locals into lambs?
(check-equal? (parse 3) (numC 3))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse "30") (strC "30"))


; (check-equal? (parse '{locals : x = 2 : y = 6 : {+ x y}})
;               (appC (lamC (clauses (idC 'x) (idC 'y))
;                           (appC (idC '+) (list (idC 'x) (idC 'y))))
;                     (list (numC 2) (numC 6))))
;               ;{{lamb : x y : {+ x y}} 2 6})

; (check-equal? (parse '{locals : z = {+ 9 14} : y = 98 : {+ z y}})
;               (appC (lamC (clauses (idC 'z) (idC 'y))
;                           (appC (idC '+) (list (idC 'z) (idC 'y))))
;                     (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
;               ; {{lamb : z y : {+ z y}} {+ 9 14} 98})







