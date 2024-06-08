#lang nanopass
(require "parser.rkt"
         "lexer.rkt")

(provide (all-defined-out))

(define (boolean-to-string val) (if val "#t" "#f"))  ;; Funcion auxiliar para pasar booleanos a string

(define (nano-pass-list l)
  (cond
    [(empty? l) ""]
    [else (string-append (->nanopass (first l)) " " (nano-pass-list (cdr l)))]))

(define (mult-list t ids)
  (cond
    [(empty? mult-list) ""]
    [else (string-append "(" (->nanopass (car mult-list)) " " t ")"
                               (mult-list (cdr mult-list) t) )]
    )
  )


(define (->nanopass e)
  (match e
    
    [(program m) (string-append  ; program
                        "(programa "
                            (->nanopass m) ")"
                            )]
    
    [(main inst func-list) (string-append      ; main
                        "(main "
                        "["
                            (->nanopass inst) "]) "
                            (->nanopass func-list))]
    
    [(if-else-exp cond t e) (string-append    ; if-exp (if normal) con else
                        "(if-else-exp "
                            (->nanopass cond) " "
                            "["
                            (->nanopass t) "] ["
                            (->nanopass e) "])")]

    [(if-exp cond t) (string-append    ; if-exp (if normal)
                        "(if-exp "
                            (->nanopass cond) " ["
                            (->nanopass t) "])")]

    [(if-corto-exp dec cond t e) (string-append  "("  ; if-corto-exp (if corto) -- En realidad es una declaracion de variable
                            (->nanopass dec) " = "
                            (->nanopass cond) " ? "
                            (->nanopass t) " : "
                            (->nanopass e) ")")]
    
    [(while-exp cond crp) (string-append            ; while-exp
                           "(while-exp "
                           (->nanopass cond) " ["
                           (->nanopass crp) "])")]

    [(funcion-expr id args tipo cuerpo) (string-append    ; funcion-expr
                                        ;  "(funcion-expr "
                                        "(funcion-expr " (->nanopass id) " ["
                                        (->nanopass args) "] "
                                        (->nanopass tipo) " ["
                                        (->nanopass cuerpo) "])")]

    [(fun id args cuerpo) (string-append    ; funcion void
                                        "(funcion-void-expr " (->nanopass id) " ["
                                        (->nanopass args) "] "
                                        (->nanopass cuerpo) ")")]

    [(funcall id args) (string-append    ; funcion void
                                        "(funcall " (->nanopass id) " ("
                                        (->nanopass args) "))")]

    
    [(id i) (symbol->string i)]
    ; [(id i) i]
    [(numero n) (number->string n)]
    [(bool b) (string-append (boolean-to-string b))]
    [(String s) (symbol->string s)]
    [(int-t) (string-append "int")]
    [(bool-t) (string-append "bool")]
    [(string-t) (string-append "String")]
    [(array a) (string-append (->nanopass a) " array")]
    [(access-array i idx) (string-append "access-array " (symbol->string i) (->nanopass idx) )]
    [(decl-var i tipo) (string-append
                        "("
                        (->nanopass i) " "
                        (->nanopass tipo) ")"
                        )]
    [(declaraciones-multiples t ids) (mult-list ids (->nanopass t))]
    [(bin-exp '= (decl-var i tipo) e1) (string-append
                        "(= "
                        (->nanopass i) " "
                        (->nanopass tipo) " "
                        (->nanopass e1) ")"
                        )]
    [(decl-arr i tipo args) (string-append "(decl-arr " (->nanopass i) " " (->nanopass tipo) " "
                                                   (->nanopass args) "))")]

    ; un-exp
    [(un-exp '- e) (string-append "(- " (->nanopass e) ")")]
    [(un-exp '! e) (string-append "(! " (->nanopass e) ")")]
    
    ; bin-exp 
    [(bin-exp '+ e1 e2) (string-append "(+ " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '- e1 e2) (string-append "(- " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '* e1 e2) (string-append "(* " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '/ e1 e2) (string-append "(/ " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '- e1 e2) (string-append "(- " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '< e1 e2) (string-append "(< " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '> e1 e2) (string-append "(> " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '>= e1 e2) (string-append "(>= " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '<= e1 e2) (string-append "(<= " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '== e1 e2) (string-append "(== " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '= e1 e2) (string-append "(= " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '!= e1 e2) (string-append "(!= " (->nanopass e1) " " (->nanopass e2) ")" )]
    [(bin-exp '% e1 e2) (string-append "(% " (->nanopass e1) " " (->nanopass e2) ")" )]

    ; Incrementos y decrementos
    [(bin-exp '= e (bin-exp '+ e (numero 1))) (string-append "(+ " (->nanopass e) " 1)" )]
    [(bin-exp '= e (bin-exp '- e (numero 1))) (string-append "- " (->nanopass e) " 1)" )]
    [(bin-exp '= e0 (bin-exp '+ e0 e1)) (string-append "(+ " (->nanopass e0) " " (->nanopass e1) ")" )]
    [(bin-exp '= e0 (bin-exp '- e0 e1)) (string-append "(- " (->nanopass e0) " " (->nanopass e1) ")" )]

    ; length
    [(length var) (string-append "(len " (->nanopass var) ")")]
    
    ; return
    [(return e) (string-append "(return " (->nanopass e) ")")]

    ; println
    [(printLn e) (string-append "(printLn " (->nanopass e) ")")]

    ; Para la lista de args o vars
    ['() ""]
    [(cons c t) (string-append  (->nanopass c) " " (string-trim (nano-pass-list t)))]
    


    
    [else "a"]))

(define-language jelly
    (terminals
        (constante  (c))
        (primitivo (pr))
        (single-primitivo (spr))
        ; (reservada  (r))
        (tipo (t))
        (id   (i)))
  (Programa (p)  
        (programa m func* ...)
        m
         )
    (Main (m)
        (main [e* ...])
        e
        )
    (Funcion (func)
        (funcion-expr i ([i* t*] ...) t [e* ...])
        (funcion-void-expr i ([i* t*] ...) [e* ...]))
  (Variable-declaration (dec)
               (i t)
               (t i* ... i)) ;; Por alguna razon no se puede poner en Expr '(i t)', el segundo es para cuando tenemos int a, b, c
  (Array-methods (arr)
               (decl-arr i t [e* ...])
               (access-array i e))
    (Expr (e)
        c
        i 
        pr
        spr
        t
        dec
        arr
        (= i t e)
        (= i e)
        ;func
        (if-exp e0 [e1* ... e])
        (if-else-exp e0 [e1* ... e2] [e3* ...])
        (while-exp e0 [e1* ...])
        (funcall i [e* ...])
        (return e)
        (printLn e)
        (len e)
        ; (i e0 e1)
        ;(pr e0 e1)
        
        (e* ... e)))

; == != < <= > >= + - * / % & or
(define (id? i) (symbol? i))
(define single-primitivo?  
        (lambda (spr) (memq spr '(not -))))
(define primitivo?  
        (lambda (pr) (memq pr '(+ - * % / &  \| < > <= >= != == += -= *= /= ++ -- : \( \)))))
(define reservada?
        (lambda (r) (memq r '(program main if while else return printLn length))))
(define constante?
        (lambda (c) (or (number? c) (boolean? c) (string? c))))
(define tipo?
        (lambda (t) (memq t '(int bool String array_int array_bool array_String))))

(define-parser parser-jelly jelly)


;; ---------------------------- RENAME-VAR --------------------------------------------------


(define-pass rename-var : jelly (in) -> jelly ()

  (rename-var-prog : Programa (in) -> * ()
                 [(programa ,m ,func* ...) (append
                   '(programa)
                   `(,(rename-var-main m))
                   (map (lambda (f) (rename-var-func f)) func*))]
                 
                 [,m (rename-var-main m)]
                 ;[,func (rename-var-func func)]
                 ;[,e (rename-var-expr e "")]
             )
  
  (rename-var-main : Main (m) -> * ()
                 [(main [,e* ...]) (list 'main  (map (lambda (exp) (rename-var-expr exp "main")) e*)
                                                     )]
                 [,e (rename-var-expr e "test")]
                 ;[,func (rename-var-func func)]
             )

  (rename-var-func : Funcion (func) -> * ()
                 [(funcion-expr ,i ([,i* ,t*] ...) ,t [,e* ...]) (list 'funcion-expr
                                                        i
                                                       (map (lambda (exp1 exp2) (list (rename-var-expr exp1 (symbol->string i)) exp2)) i* t*)
                                                       t
                                                       (map (lambda (exp) (rename-var-expr exp (symbol->string i))) e*)
                                                       )]
                 
                 [(funcion-void-expr ,i ([,i* ,t*] ...) [,e* ...]) (list 'funcion-void-expr
                                                        i
                                                       (map (lambda (exp1 exp2) (list (rename-var-expr exp1 (symbol->string i)) exp2)) i* t*)
                                                       (map (lambda (exp) (rename-var-expr exp (symbol->string i))) e*)
                                                       )]
                 )
  (rename-var-expr :  Expr (e funcion1) -> * ()
            [,c c]
            [,t t]
            [,pr pr]
            [,spr spr]
            [,i (let([n-var (string->symbol (string-append (symbol->string i) "_" funcion1
                                                                ))])
                       n-var)]

            [(,i, t) (let([n-var (string->symbol (string-append (symbol->string i) "_" funcion1))])
                                                                          (list n-var t))]

            [(= ,i ,t ,e) (let([n-var (string->symbol (string-append (symbol->string i) "_" funcion1))]
                             [expr (rename-var-expr e funcion1)])
                                                                          (list '= n-var t expr))]
            
            [(= ,i ,e) (let([n-var (string->symbol (string-append (symbol->string i) "_" funcion1))]
                             [expr (rename-var-expr e funcion1)])
                                                                          (list '= n-var expr))]

            [(,t ,i* ... , i) (append
                                 (map (lambda (exp) (list (rename-var-expr exp funcion1) t)) i*)
                                 (list (list (rename-var-expr i funcion1) t)))]
            
            [,dec (list (rename-var-expr dec funcion1))]

            [(,pr, e0, e1) (list pr (rename-var-expr e0 funcion1) (rename-var-expr e1 funcion1)) ]
            [(,spr, e) (list spr (rename-var-expr e funcion1)) ]
            
            [(if-exp ,e0 [,e1* ... ,e]) (append '(if-exp) `(,(rename-var-expr e0 funcion1))
                                              `(,(map (lambda (exp) (rename-var-expr exp funcion1)) (append e1* (list e)))))]

            [(if-else-exp ,e0 [,e1* ... ,e2] [,e3* ...]) (list 'if-else-exp (rename-var-expr e0 funcion1)
                                                               (map (lambda (exp) (rename-var-expr exp funcion1)) (append e1* (list e2)))
                                                               (map (lambda (exp) (rename-var-expr exp funcion1)) e3*))]
            
            [(while-exp ,e0 [,e1* ...]) (list 'while-exp (rename-var-expr e0 funcion1)
                                              (map (lambda (exp) (rename-var-expr exp funcion1)) e1*))]

            [(decl-arr ,i ,t [,e* ...]) (list 'decl-arr
                                              (rename-var-expr i funcion1)
                                              t
                                              (map (lambda (exp) (rename-var-expr exp funcion1)) e*))]
            
            [(access-array ,i ,e) (list 'access-array
                                        (rename-var-expr i funcion1)
                                        (rename-var-expr e funcion1))]

            [,arr (list (rename-var-expr arr funcion1))]


            [(funcall ,i [,e* ...]) (list 'funcall
                                          i
                                          (map (lambda (exp) (rename-var-expr exp funcion1)) e*))]

            
            
            [(return ,e) (list 'return (rename-var-expr e funcion1))]
            [(printLn ,e) (list 'printLn (rename-var-expr e funcion1))]
            [(len ,e) (list 'len (rename-var-expr e funcion1))]
            [(,e* ... ,e) (list (map (lambda (exp) (rename-var-expr exp funcion1)) e*) (rename-var-expr e funcion1))]
  )


  (parser-jelly (rename-var-prog in)))



;; ---------------------------- RENAME-VAR --------------------------------------------------

;; TABLA DE SIMBOLOS -------------------------

(define tabla (make-hash))

(define (symbol-table ir)
  (tabla-programa ir tabla))

(define (tabla-programa ir tb)
    (nanopass-case (jelly Programa) ir
        [(programa ,m ,func* ...) (begin
                                   (tabla-main m tb)
                                   (map (lambda (f) (tabla-func f tb)) func*)
                                   tb)]
        ))

(define (tabla-main ir tb)
    (nanopass-case (jelly Main) ir
        [(main [,e* ...]) (begin
                                    (map (lambda (exp) (tabla-expr exp tb)) e*)
                                      tb)]
        ))

(define (tabla-func ir tb)
    (nanopass-case (jelly Funcion) ir
        [(funcion-expr ,i ([,i* ,t*] ...) ,t [,e* ...])
                                        (begin
                                            (hash-set! tb i (cons t t*))
                                            (map (lambda (exp1 exp2) (hash-set! tb exp1 exp2)) i* t*)
                                            (map (lambda (exp) (tabla-expr exp tb)) e*)
                                            tb)]
        [(funcion-void-expr ,i ([,i* ,t*] ...) [,e* ...]) (begin
                                            (hash-set! tb i (append (list 'unit) t*))
                                            (map (lambda (exp1 exp2) (hash-set! tb exp1 exp2)) i* t*)
                                            (map (lambda (exp) (tabla-expr exp tb)) e*)
                                            tb)]
        [else tb]))

(define (tabla-expr ir tb)
    (nanopass-case (jelly Expr) ir
        [,c tb]
        [,i tb]
        [,pr tb]
        [,spr tb]
        [(,i ,t) (hash-set! tb i t)]
        [(= ,i ,t ,[e]) (hash-set! tb i t)]
        [(= ,i ,e) tb]
        [(,t ,i* ... , i) (map (lambda (exp) (hash-set! tb exp t)) i*)] ;; int a, b, c
        [(if-exp ,[e0] [,[e1*] ... ,[e]]) tb]
        [(if-else-exp ,[e0] [,[e1*] ... ,[e2]] [,[e3*] ...]) tb]
        [(while-exp ,[e0] [,[e1*] ...]) tb]
        [(decl-arr ,i ,t [,e* ...]) (begin
                                      (cond
                                          [(eq? t 'int) (hash-set! tb i 'array_int)]
                                          [(eq? t 'bool)(hash-set! tb i 'array_bool)]
                                          [(eq? t 'String) (hash-set! tb i 'array_String)])
                                            (map (lambda (exp) (tabla-expr exp tb)) e*)
                                            tb)]
        [(access-array ,i ,[e]) tb]
        [(funcall ,i [,[e*] ...]) tb]
        [(return ,[e]) tb]
        [(printLn ,[e]) tb]
        [(len ,[e]) tb]
        
        [(,[e*] ... ,[e]) tb]
        [else (begin
                (display "else")
                tb)]))


; ================================ type-check ======================

(define (type-check ir tabla)
  (get-type-program ir tabla))



(define (get-type-program ir tabla)
    (nanopass-case (jelly Programa) ir
        [(programa ,m ,func* ...) (let* ([main-types (get-type-main m tabla)]
                                         [func-types (map (lambda (f) (get-type-func f tabla)) func*)])
                                    (append main-types func-types))]
        ))

(define (get-type-main ir tabla)
    (nanopass-case (jelly Main) ir
        [(main [,e* ...]) (let* ([types (map (lambda (exp) (get-type-expr exp tabla)) e*)]
                                 [return-types  (filter (lambda (t) (not (eq? 'unit t))) types)])
                            (if (empty? return-types)
                                'unit
                                return-types))]
        ))

(define (get-type-func ir tabla)
    (nanopass-case (jelly Funcion) ir
        [(funcion-expr ,i ([,i* ,t*] ...) ,t [,e* ...]) (let* ([return-type (hash-ref tabla i)]
                                                              [args-types (map (lambda (exp) (get-type-expr exp tabla)) i*)]
                                                              [verify-types (andmap (lambda (a p) (eq? a p)) args-types t*)])
                                                          (if (list? return-type)
                                                              (if (not (eq? (car return-type) 'unit))
                                                                  (if (and verify-types (not (empty? args-types)))
                                                                      (car return-type)
                                                                      (error "Los parametros no coinciden con el tipo esperado."))
                                                                  (error "Esta funcion regresa void, y no deberia ser asi."))
                                                              (error "El id no corresponde a una funcion")))]
        
        [(funcion-void-expr ,i ([,i* ,t*] ...) [,e* ...]) (let* ([return-type (hash-ref tabla i)]
                                                              [args-types (map (lambda (exp) (get-type-expr exp tabla)) i*)]
                                                              [verify-types (andmap (lambda (a p) (eq? a p)) args-types t*)])
                                                          (if (list? return-type)
                                                              (if (eq? (car return-type) 'unit)
                                                                  (if (and verify-types (not (empty? args-types)))
                                                                      (car return-type)
                                                                      (error "Los parametros no coinciden con el tipo esperado."))
                                                                  (error "Esta funcion debe regresa void, y no deberia es asi."))
                                                              (error "El id no corresponde a una funcion")))]))
        

(define (get-type-expr ir tabla)
    (nanopass-case (jelly Expr) ir
        [,i (hash-ref tabla i)]
        [,c (cond
                       [(boolean? c) 'bool]
                       [(number? c)  'int]
                       [(string? c) 'String])]
        [(,pr ,e1 ,e2)
                  (match pr
                    [(or '+ '- '* '/ '%)
                     (let ([t-e1 (get-type-expr e1 tabla)]
                           [t-e2 (get-type-expr e2 tabla)])
                       (if (and (eq? t-e1 'int)
                                (eq? t-e2 'int))
                           'int
                           (error "Operandos no son enteros.")))]
                    [(or '< '> '<= '>=) (let ([t-e1 (get-type-expr e1 tabla)]
                                              [t-e2 (get-type-expr e2 tabla)])
                                          (if (and (eq? t-e1 'int)
                                                   (eq? t-e2 'int))
                                              'bool
                                              (error "Operandos no son enteros.")))]
                    [(or '&  '\|) (let ([t-e1 (get-type-expr e1 tabla)]
                                         [t-e2 (get-type-expr e2 tabla)])
                                     (if (and (eq? t-e1 'bool)
                                              (eq? t-e2 'bool))
                                         'bool
                                         (error "Operandos no son booleanos.")))]
                    [(or '== '!=) (let ([t-e1 (get-type-expr e1 tabla)]
                                        [t-e2 (get-type-expr e2 tabla)])
                                    (if (eq? t-e1 t-e2)
                                        'bool
                                        (error "Operandos no son del mismo tipo.")))]
                    )]


        [(,spr, e) (cond
                     [(eq? 'not spr) (if (eq? (get-type-expr e tabla) 'bool)
                                         'bool
                                         (error "El prefijo not sólo puede afectar a booleanos"))]
                     [(eq? '- spr) (if (eq? (get-type-expr e tabla) 'int)
                                         'int
                                         (error "El prefijo - sólo puede afectar a enteros"))])]
        
        [(,i, t) (if (eq? (hash-ref tabla i) t)
                              t
                              (error "Tipo de la variable declarada no coincide"))]


        [(= ,i ,[e -> te]) (if (eq? (hash-ref tabla i) te)
                                   'unit
                                   (error "Expresión asignada difiere del tipo declarado para la variable"))]

        [(if-exp ,[e0 -> te0] [,e1* ... ,e]) (begin
                                               ;(display ir)
                                               (if (eq? te0 'bool)
                                                 'unit
                                                 (error "La condicion no es de tipo bool")))]

        [(if-else-exp ,[e0 -> te0] [,e1* ... ,e2] [,e3* ...]) (begin
                                               (if (eq? te0 'bool)
                                                 'unit
                                                 (error "La condicion no es de tipo bool")))]

        [(while-exp ,[e0 -> te0] [,e1* ...]) (if (eq? te0 'bool)
                                                 'unit
                                                 (error "La condicion no es de tipo bool"))]

        [(funcall ,i [,e* ...]) (let ([return-type (hash-ref tabla i)]
                                      [args-types (map (lambda (exp) (get-type-expr exp tabla)) e*)])
                                  (if (list? return-type)
                                      (if (andmap (lambda (a) (check-if-bool-int-str? a)) args-types)
                                          (car return-type)
                                          (error "Los argumentos no son de algun tipo primitivo"))
                                      (error "La funcion a la que se quiere llamar no existe")))]



        [(return, e) (get-type-expr e tabla)]
        [(printLn, e) (get-type-expr e tabla)]
        
        [(decl-arr ,i ,t [,e* ...]) (let* ([args-types (map (lambda (exp) (get-type-expr exp tabla)) e*)]
                                           [var-type (hash-ref tabla i)])
                                      (if (or (eq? var-type 'array_int) (eq? var-type 'array_bool) (eq? var-type 'array_String))
                                          (cond
                                            [(eq? var-type 'array_int) (if (andmap (lambda (a) (eq? a 'int)) args-types)
                                                                           'array_int
                                                                           (error "No todos los elementos del arreglo son de tipo 'int."))]
                                            [(eq? var-type 'array_bool) (if (andmap (lambda (a) (eq? a 'bool)) args-types)
                                                                           'array_bool
                                                                           (error "No todos los elementos del arreglo son de tipo 'bool."))]
                                            [(eq? var-type 'array_String) (if (andmap (lambda (a) (eq? a 'String)) args-types)
                                                                           'array_String
                                                                           (error "No todos los elementos del arreglo son de tipo 'String."))])
                                          (error "La variable no es un arreglo.")))]

        [(access-array ,i ,[e -> te]) (let ([var-type (hash-ref tabla i)])
                                        (if (eq? 'int te)
                                            (if (or (eq? 'array_int var-type) (eq? 'array_bool var-type) (eq? 'array_String var-type))
                                                (cond
                                                  [(eq? 'array_int var-type) 'int]
                                                  [(eq? 'array_bool var-type) 'bool]
                                                  [(eq? 'array_String var-type) 'String])
                                                (error var-type))
                                            (error "La expresion no es un indice de tipo 'int")))]
        
        [(len ,[e -> te]) (if (or (eq? te 'array_int) (eq? te 'array_bool) (eq? te 'array_String))
                              'int
                              (error "La expresion no es de tipo array"))]
        

        [else (begin
                (display ir)
                'unit)]))

(define (check-if-bool-int-str? t)
  (cond
    [(eq? t 'int) #t]
    [(eq? t 'bool) #t]
    [(eq? t 'array_int) #t]
    [(eq? t 'array_bool) #t]
    [else #f]))





; ==============================


; ============================= TESTS ===============================

(define (parsea-archivo s)
  (let* ([a (open-input-file s)]
         [r (jelly-parser (lex jelly-lexer a))]
         [r2 (->nanopass r)]
         [r3 (read (open-input-string r2))]
         [r4 (parser-jelly r3)]
         [r5 (rename-var r4)]
         )
    (begin  (close-input-port a)
             r5)))


(define (tests expr tabla)
  (let* ([expr-parsed (parser-jelly expr)]
         [renamed-expr (rename-var expr-parsed)])
    (type-check renamed-expr tabla)))


(define met '(funcion-void-expr gdc [(varu int)(vard int)(vari bool)] [(vardd int) (= vardd (+ 5 5))] ))
(define ret-met '(funcion-expr max-elem [(a array_int)(current int)] int [(hola int) (= hola (access-array a 0)) (return hola)] ))
(define array-ex '(decl-arr a1 int ((+ 5 5) (+ 5 7) 5)))
(define if-ex '(if-exp (> 8 2) [(a int) (b int) (= b 6) (= a (+ b 5))]))
(define multi-ex '(int a b))
(define dec-ex '(= a int 9))
(define arr-ex '(len (decl-arr a int [9 9 9 6])))
(define acc-arr-ex '(access-array a 1))
(define simple-exp '(> 8 5))

(define prog3 '(programa (main [(printLn a) (s String) (= s "hola") (a int) (b int) (e int) (= e 4) (d String) (decl-arr a1 String ("aa" "bb" "c")) (= d (access-array a1 2)) (= a 5) (= b 9) (if-else-exp (> a b) [(x int) (= x 100) (= a (+ x a)) (= b (+ b 2))]
                                                                            [(while-exp (> b a) [(= b (- b 1))])])])
                        (funcion-void-expr gdc [(varu int)(vard int)(vari bool)] [(+ c d)] )
                        (funcion-expr max-elem [(a array_int)(current int)] int [(hola int) (= hola (access-array a 0)) (return hola)] )))

(define prog2 '(program (main ((if-exp (> a b) ((= j (+ j 1)))))) (funcion-expr gcd ((a int) (b int)) int ((= i int 0) (while-exp (< i 5) ((= i (+ i 5)) (= i (+ i 1)))) (= a int 0) (while-exp (< a 5) ((= a (+ a 5)) (= a (+ a 1)))) (return b)))))


(define if-else-ex '(if-else-exp (> 9 2) ((= a (+ vard varu)) (= b int a)) ((- vard varu) (- varu vard))))
(define while-ex '(while-exp (> varu vard) [(+ vard varu) (+ varu vard)]))

(define prog '(programa (main [(a int) (b int) (e int) (= e 4) (d int) (decl-arr a1 int ((+ 5 5) (+ 5 7) 5)) (= d (access-array a1 2)) (= a 5) (= b 9) (if-else-exp (> a b) [(x int) (= x 100) (= a (+ x a)) (= b (+ b 2))]
                                                                            [(while-exp (> b a) [(= b (- b 1))])])])
                        (funcion-void-expr gdc [(varu int)(vard int)(vari bool)] [(+ c d)] )
                        (funcion-expr max-elem [(a array_int)(current int)] int [(hola int) (= hola (access-array a 0)) (return hola)] )))

(define main-ex '(main [(a int) (= a 5) (while-exp (> a 4) [(c int) (= c 6)])]))
(define simple-exp-rename (rename-var (parser-jelly main-ex)))

(define prog-ex-parse (rename-var (parser-jelly prog)))
(define t2 (make-hash '((gdc . '(int int int)) (b.test . 'int) (a.test . 'bool))))
(define t3 (make-hash '((gdc . '(int int int)) (varu . 'int) (vard . 'int) (a.test . array_bool))))

(define tabla-prog (symbol-table prog-ex-parse))
(define test1 (tests prog tabla-prog))

;(println "Orignal expr")
;(println prog)
;(println "=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|")
;(println "     RENAME-VAR                                      ")
;(println prog-ex-parse)
;(println "                                           ")
;(println "=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|")
;(println "")
;(println "     SYMBOL-TABLE                                     ")
;(println tabla-prog)
;(println "=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|")
;(println "     TYPE-CHECK                                     ")
;(println test1)



