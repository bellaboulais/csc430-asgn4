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
(define-type ExprC (U numC idC strC ifC locals lamb appC))                 
(tstruct numC ([n : Real]))                           
(tstruct idC ([name : Symbol]))                          
(tstruct strC ([str : String]))
(tstruct ifC ([test : ExprC] [then : ExprC] [else : ExprC]))
(tstruct locals ([bindings : Clauses] [body : ExprC]))
(tstruct lamC ([arg : Symbol] [body : ExprC]))
(tstruct appC ([f : ExprC] [args : (Listof ExprC)]))

(define-type Clauses (U clause clauses))
(tstruct clause ([id : Symbol] [expr : ExprC]))
(tstruct clauses ([id : Symbol] [expr : ExprC] [rest : Clauses]))

(define-type Value (U numV closV))
(tstruct numV ([n : Number]))
(tstruct closV ([arg : Symbol] [body : ExprC] [env : Env]))


;; FUNCTIONS

; top-interpt
(: top-interp (Sexp -> String)) 
;   PARAMS:  s : Sexp
;   RETURNS: String
;   PURPOSE:  
(define (top-interp s)
  (serialize (interp (parse s) top-env)))


; interp
(: interp (ExprC Environment -> Value))
;   PARAMS:   e:    ExprC           the expression to interpret
;             env:  Envrionment     list of functions defined in the current environment
;   RETURNS:  Value
;   PURPOSE:  
(define (interp e env)
  (match z                                                                     
    [(numC n) n]                                                                
    [(idC _) (error 'interp "ZODE: shouldn't get here")]
    [(appC fun args) (local ([define fd (get-fundef fun fds)])                   
                       (interp (subst args
                                      (fdC-args fd)
                                      (fdC-body fd))
                               fds))]))   
       
; parse  
(: parse (Sexp -> ExprC))
;   PARAMS:   s :  Sexp
;   RETURNS:  ExprC
;   PURPOSE:  
(define (parse s)
  (match s
    [(list 'ifleq0? test then else) (ifleq0? (parse test) (parse then) (parse else))]
    [(? real? n) (numC n)]                            
    [(? symbol? s) (idC s)] ; variable reference cant be named operator 
    [(list (? symbol? op) l r)
     (if (hash-has-key? binop-table op)
         (binop op (parse l) (parse r)) 
         (appC op (list (parse l) (parse r))))]
    [(list (? symbol? fun) args ...) 
     (if (hash-has-key? binop-table fun)  
         (error 'parse "ZODE: Invalid number of arguments")
         (appC fun (map (lambda ([arg : Sexp]) (parse arg)) args)))]
    
    [else (error 'parse "ZODE: Invalid expression")]))  



 


