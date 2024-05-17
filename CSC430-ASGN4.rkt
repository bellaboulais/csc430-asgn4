#lang typed/racket
(require typed/rackunit)

;; Assignment 4

; ----------------------------- ;
; ----- DATA DEFINITIONS ------ ;
; ----------------------------- ;

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
(tstruct locals ([bindings : (Listof Clause)] [body : ExprC]))
(tstruct lamC ([args : (Listof Symbol)] [body : ExprC]))
(tstruct appC ([f : ExprC] [args : (Listof ExprC)]))

; Define the Clause: inside locals... ex. add6 = {curradd 6} : 
(tstruct Clause ([id : Symbol] [expr : ExprC]))

; Define our value types -> represents an evaluated expression
(define-type Value (U numV boolV strV closV primV)) 
(tstruct boolV ([b : (U #t #f)]))                   ; boolV: boolean value
(tstruct strV ([s : String]))                       ; strV: string value
(tstruct closV ([args : (Listof Symbol)] [body : ExprC] [env : Env])) ; closV: closure value
(tstruct primV ([p : Symbol]))                      ; primV: primitive value
(tstruct numV ([n : Real]))                         ; numV: number value

; Define our env and top-env
(define-type Env (Listof (List Symbol Value)))
(define top-env (list (list '+ (primV '+))
                      (list '- (primV '-))
                      (list '* (primV '*))
                      (list '/ (primV '/))
                      ; (list '<= (primV '<=))
                      ; (list 'equal? (primV 'equal?))
                      (list 'true (boolV #t))
                      (list 'false (boolV #f))
                      ; (list 'error (primV 'error))
                      ))


; ----------------------------- ;
; ----- INTERP FUNCTIONS ------ ;
; ----------------------------- ;

; lookup NOT COMPLETE (waiting on more primitives to write more test cases)
;  PARAMS:  x : Symbol
;           env : Env
;  RETURNS: Value
;  PURPOSE: lookup the value of x in the environment env
(: lookup (Symbol Env -> Value))
(define (lookup [x : Symbol] [env : Env])
  (cond
    [(empty? env) (error 'lookup "ZODE: Variable not found ~e" x)]
    [(equal? x (first (first env))) (second (first env))]
    [else (lookup x (rest env))]))


; extend-env NOT COMPLETE (waiting on more primitives to write more test cases)
;  PARAMS:  clauses : Clauses
;           env : Env
;  RETURNS: Env
;  PURPOSE: extend the environment env with the definitions in another environment.
(: extend-env ((Listof Clause) Env -> Env))
(define (extend-env clauses env)
  (if (empty? clauses)
      env
      (cons (list (Clause-id (first clauses)) (interp (Clause-expr (first clauses)) env))
            (extend-env (rest clauses) env))))

(: extend-env2 ((Listof Symbol) (Listof Value) Env -> Env))
(define (extend-env2 params argval env)
  (define new-bindings
    (map (lambda (param [arg : Value])
           (list param arg))
         params argval))
  (cast (append new-bindings env) Env))
  
;(: extend-env (Clauses Env -> Env))
;(define (extend-env [clses : Clauses] [env : Env])
 ; (match clses
  ;  [(clause id expr) (cons (list id (interp expr env)) env)]
   ; [(clauses id expr rest) (extend-env rest (cons (list id (interp expr env)) env))]))



; top-interp NOT COMPLETE (no test cases)
(: top-interp (Sexp -> String)) 
;   PARAMS:  s : Sexp
;   RETURNS: String
;   PURPOSE:  
(define (top-interp s)
 (serialize (interp (parse s) top-env)))


; serialize
(: serialize (Value -> String))
;   PARAMS:   v:    Value           the value to serialize
;   RETURNS:  String
;   PURPOSE:  convert a value to a string
(define (serialize v)
  (match v
    [(numV n) (~v n)]
    [(boolV b) (if b "true" "false")]
    [(strV s) s]
    [(closV arg body _) "#<procedure>"]
    [(primV p) "#<primop>"]))


; interp NOT COMPLETE (needs more test cases)
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
    [(ifC test then els) (match (interp test env)
                           [(boolV #t) (interp then env)]
                           [(boolV #f) (interp els env)]
                           [else (error 'interp "ZODE: Invalid if expression: ~e" e)])]
    [(locals clauses body) (interp body (extend-env clauses env))]
    [(lamC args body) (closV args body env)]
    [(appC f args) (match (interp f env)
                     [(closV params body env2)
                      (define argval (map (lambda ([arg : ExprC]) (interp arg env)) args))
                      (interp body (extend-env2 params argval env))]
                     [(primV p) (interp-primitive p args env)]
                     [else (error 'interp "ZODE: Invalid application: ~e" e)])]
    [else (error 'interp "ZODE: Invalid expression ~e" e)]))


; interp-primitive
(: interp-primitive (Symbol (Listof ExprC) Env -> Value))
;   PARAMS:   p:    Symbol          the primitive to interpret
;             args: (Listof Value)  the arguments to the primitive
;   RETURNS:  Value
;   PURPOSE:  helper for interp to interpret primitives
(define (interp-primitive [p : Symbol][exprs : (Listof ExprC)] [env : Env])
  (match p 
    ['+ (2num-op (interp (first exprs) env) (interp (second exprs) env) +)]
    ['- (2num-op (interp (first exprs) env) (interp (second exprs) env) -)]
    ['* (2num-op (interp (first exprs) env) (interp (second exprs) env) *)]
    ['/ (2num-op (interp (first exprs) env) (interp (second exprs) env) /)]
    ; ['<= (2num-op (interp (first exprs) env) (interp (second exprs) env) <=)]
    ; ['equal? (2num-op (interp (first exprs) env) (interp (second exprs) env) equal?)]
    ['true (boolV #t)]
    ['false (boolV #f)]
    ; ['error (error 'error "ZODE: error")]
    [else (error 'interp-primitive "ZODE: Invalid primitive ~e" p)]))

; 2num-op
(: 2num-op (Value Value (Real Real -> Real) -> Value))
;   PARAMS:   l:    Value           the left operand
;             r:    Value           the right operand
;             operator: (Number Number -> Number) the operator to apply
;   RETURNS:  Value
;   PURPOSE:  helper for interp-primitive to apply a binary operator to two numbers
(define (2num-op [l : Value] [r : Value] [operator : (Real Real -> Real)]) : Value
  (cond
    [(and (numV? l) (numV? r))
     (numV (operator (numV-n l) (numV-n r)))]
    [else
     (error 'num+ "ZODE: primitive + expects numbers as arguments, given ~e and ~e" l r)]))



; ---------------------------- ;
; ----- PARSER FUNCTIONS ----- ;
; ---------------------------- ;
; (define-type ExprC (U numC idC strC ifC locals lamb appC))         
; parse INCOMPLETE (needs more test cases, and also lambda parse is probably wrong)
(: parse (Sexp -> ExprC))
;   PARAMS:   s :  Sexp
;   RETURNS:  ExprC
;   PURPOSE:  
(define (parse s)
  (match s
    [(? real? n) (numC n)] ; real                           
    [(? symbol? s) (idC s)] ; variable
    [(? string? str) (strC str)] ; string
    [(list 'if test then else) (ifC (parse test) (parse then) (parse else))] ; if
    [(list 'locals ': clauses ... ': body) ; locals
     (locals (parse-clause (cast clauses Sexp)) (parse body))] ; cast clauses to an sexp and pass into parse-clause
    ; separate parse-clause output into a list of symbols and a list of exprC
    [(list 'lambda args body) ; lambC
     (lamC (cast args (Listof Symbol)) (parse body))] ; this is probably bad
    [(list f args ...) ; appC 
     (appC (parse f) (map (lambda ([arg : Sexp]) (parse arg)) args))]
    [else (error 'parse "ZODE: Invalid expression, ~e" s)]))

; parse-clause
(: parse-clause (Sexp -> (Listof Clause)))
;   PARAMS:   s :  Sexp
;   RETURNS:  list of Clause
;   PURPOSE:  parse clauses from locals statement 
(define (parse-clause s)
  (match s
    [(list (? symbol? id) '= expr)
     (list (Clause id (parse expr)))]
    [(list (? symbol? id) '= expr ': rest ...)
     (cons (Clause id (parse expr)) (parse-clause rest))]
    [else (error 'parse-clause "ZODE: Invalid expression, ~e" s)]))

; ---------------------- ;
; ----- TEST CASES ----- ;
; ---------------------- ;

; ----- defs for test cases ----- ;
; test parsed functions
(define testpfunc0 (list (Clause 'curradd (lamC (list 'x) (appC (idC '+) (list (idC 'x) (idC 'x)))))))
(define testpfunc1 (list (Clause 'add6 (appC (idC 'curradd) (list (numC 6))))))

;   test environments with numbers
(define testenv1 (list (list 'x (numV 5))))
(define testenv2 (list (list 'x (numV 5)) (list 'y (numV 6))))
;   test environments with functions
(define testenv3 (list (list 'x (closV (list 'x) (idC 'x) testenv1))))
(define testenv4 (list (list 'x (closV (list 'x) (idC 'x) top-env))))
;   test environments with strings
(define testenv5 (list (list 'x (strV "hello"))))
;   test environments with booleans
(define testenv6 (list (list 'tv (boolV #t))))
(define testenv7 (list (list 'fv (boolV #f))))

; ----- INTERPRETER TEST CASES ----- ;

; ----- lookup test cases -----
;   test lookup with numbers
(check-equal? (lookup 'x (list (list 'x (numV 5)))) (numV 5))
(check-equal? (lookup 'x (list (list 'x (strV "hello")))) (strV "hello"))
;   test lookup with the primitive functions
(check-equal? (lookup '+ top-env) (primV '+))
(check-equal? (lookup '- top-env) (primV '-))
(check-equal? (lookup '* top-env) (primV '*))
(check-equal? (lookup '/ top-env) (primV '/))
;   test lookup with functions
(check-equal? (lookup 'x testenv3) (closV (list 'x) (idC 'x) testenv1))
(check-equal? (lookup 'x testenv4) (closV (list 'x) (idC 'x) top-env))
;   test lookup with strings
(check-equal? (lookup 'x testenv5) (strV "hello"))
;   test lookup with booleans
(check-equal? (lookup 'tv testenv6) (boolV #t))
(check-equal? (lookup 'fv testenv7) (boolV #f))
;   test lookup with a variable not found
(check-exn #px"ZODE: Variable not found" (λ () (lookup 'y (list (list 'x (numV 5))))))
;   
; ----- extend-env test cases -----
; test extending from top level environment
(check-equal? (extend-env (list (Clause 'x (numC 5))) top-env) 
              (list (list 'x (numV 5)) 
                    (list '+ (primV '+)) 
                    (list '- (primV '-)) 
                    (list '* (primV '*)) 
                    (list '/ (primV '/))
                    ; (list '<= (primV '<=))
                    ; (list 'equal? (primV 'equal?))
                    (list 'true (boolV #t))
                    (list 'false (boolV #f))
                    ; (list 'error (primV 'error))
                    ))
; test extending testenv1 with a number
(check-equal? (extend-env (list (Clause 'y (numC 6))) testenv1) 
              (list (list 'y (numV 6)) 
                    (list 'x (numV 5))))
; test extending testenv1 with a function
(check-equal? (extend-env testpfunc0 testenv1) 
              (list (list 'curradd (closV '(x) (appC (idC '+) (list (idC 'x) (idC 'x))) testenv1))
                    (list 'x (numV 5))))
; test extending testenv1 with a string
(check-equal? (extend-env (list (Clause 'y (strC "hello"))) testenv1) 
              (list (list 'y (strV "hello")) 
                    (list 'x (numV 5))))
; test extending testenv1 with a boolean expression
(check-equal? (extend-env (list (Clause 'y (ifC (idC 'tv) (numC 5) (numC 6)))) testenv6) 
              (list (list 'y (numV 5)) 
                    (list 'tv (boolV #t))))

; ----- extend-env2 test cases -----
; test extending testenv1 with a number
(check-equal? (extend-env2 (list 'y) (list (numV 6)) testenv1) 
              (list (list 'y (numV 6)) 
                    (list 'x (numV 5))))
; test extending testenv1 with a function
(check-equal? (extend-env2 (list 'add6) (list (closV '(x) (appC (idC '+) (list (idC 'x) (idC 'x))) testenv1)) testenv1) 
              (list (list 'add6 (closV '(x) (appC (idC '+) (list (idC 'x) (idC 'x))) testenv1))
                    (list 'x (numV 5))))

; ----- interp test cases -----
; test interp with numbers
(check-equal? (interp (numC 5) top-env) (numV 5))
; test interp with strings
(check-equal? (interp (strC "hello") top-env) (strV "hello"))
; test interp with boolean expressions
(check-equal? (interp (ifC (idC 'tv) (numC 5) (numC 6)) testenv6) (numV 5))
(check-equal? (interp (ifC (idC 'fv) (numC 5) (numC 6)) testenv7) (numV 6))
; test interp with functions
(check-equal? (interp (lamC (list 'x) (idC 'x)) top-env) (closV (list 'x) (idC 'x) top-env))
; test interp with primitive functions
(check-equal? (interp (appC (idC '+) (list (numC 5) (numC 6))) top-env) (numV 11))
(check-equal? (interp (appC (idC '-) (list (numC 5) (numC 6))) top-env) (numV -1))
(check-equal? (interp (appC (idC '*) (list (numC 5) (numC 6))) top-env) (numV 30))
(check-equal? (interp (appC (idC '/) (list (numC 6) (numC 5))) top-env) (numV 6/5))
; error cases

; ----- interp-primitive test cases -----
(check-equal? (interp-primitive '+ (list (numC 5) (numC 6)) top-env) (numV 11))
(check-equal? (interp-primitive '- (list (numC 5) (numC 6)) top-env) (numV -1))
(check-equal? (interp-primitive '* (list (numC 5) (numC 6)) top-env) (numV 30))
(check-equal? (interp-primitive '/ (list (numC 6) (numC 5)) top-env) (numV 6/5))
; (check-equal? (interp-primitive '<= (list (numC 5) (numC 6)) top-env) (boolV #t))
; (check-equal? (interp-primitive 'equal? (list (numC 5) (numC 6)) top-env) (boolV #f))
(check-equal? (interp-primitive 'true (list (numC 5) (numC 6)) top-env) (boolV #t))
(check-equal? (interp-primitive 'false (list (numC 5) (numC 6)) top-env) (boolV #f))
; (check-equal? (interp-primitive 'error (list (numC 5) (numC 6)) top-env) (error 'error "ZODE: error"))

; ----- 2num-op test cases -----


; ----- serialize test cases -----
(check-equal? (serialize (numV 5)) "5")
(check-equal? (serialize (boolV #t)) "true")
(check-equal? (serialize (boolV #f)) "false")
(check-equal? (serialize (strV "hello")) "hello")
(check-equal? (serialize (closV (list 'x) (idC 'x) top-env)) "#<procedure>")
(check-equal? (serialize (primV '+)) "#<primop>")

; ----- top-interp test cases -----
(check-equal? (top-interp 5) "5")
(check-equal? (top-interp "hello") "hello")
(check-equal? (top-interp '(if true 5 6)) "5")
(check-equal? (top-interp '(if false 5 6)) "6")
(check-equal? (top-interp '{locals : x = 5 : y = 6 : {+ x y}}) "11")
(check-equal? (top-interp '{locals : curradd = {lamb : x : {+ x x}} : add6 = {curradd 6} : {add6}}) "12")
(check-equal? (top-interp '{locals : x = 5 : y = 6 : {+ x y}}) "11")
(check-equal? (top-interp '{locals : x = "hello" : y = 6 : {+ x y}}) "ZODE: Invalid application: {+ x y}")
(check-equal? (top-interp '{locals : x = 5 : y = 6 : {+ x y}}) "11")

; ----- PARSER TEST CASES ----- ;

; parse test cases desugars locals into lambs?
(check-equal? (parse 3) (numC 3))
(check-equal? (parse 'x) (idC 'x))
(check-equal? (parse "30") (strC "30"))



; (check-equal? (parse '{locals : z = {+ 9 14} : y = 98 : {+ z y}})
 ;              (appC (lamC (clauses (idC 'z) (idC 'y))
  ;                         (appC (idC '+) (list (idC 'z) (idC 'y))))
   ;                  (list (appC (idC '+) (list (numC 9) (numC 14))) (numC 98))))
; lamC list z y
;               ; {{lamb : z y : {+ z y}} {+ 9 14} 98})



;(check-equal? (parse '{locals : x = 2 : y = 6 : {+ x y}})
 ;             (appC (lamC (clauses (idC 'x) (idC 'y))
  ;                        (appC (idC '+) (list (idC 'x) (idC 'y))))
   ;                 (list (numC 2) (numC 6))))
;{{lamb : x y : {+ x y}} 2 6}



