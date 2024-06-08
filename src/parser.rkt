
#lang nanopass
(require "lexer.rkt"         ; Asegúrate de que este archivo contenga la definición de `jelly-lex`
         parser-tools/yacc)

(provide (all-defined-out))

; Estructura: Nodos del árbol
(define-struct id (i) #:transparent)
(define-struct numero (n) #:transparent)
(define-struct bool (b) #:transparent)
(define-struct decl-var (id tipo) #:transparent) 
(define-struct arr (id tipo tam) #:transparent) ;;;;;;;
(define-struct type (tipo) #:transparent) ;;;;;;;
(define-struct un-exp (op arg1) #:transparent)
(define-struct bin-exp (op arg1 arg2) #:transparent)
(define-struct funcall (nombre argumentos) #:transparent)
(define-struct access-array (id pos) #:transparent)
(define-struct asign-arr (arr pos valor) #:transparent) ;;;;;
(define-struct if-exp (guardia si) #:transparent)
(define-struct if-corto-exp (declaracion condicion si no) #:transparent) 
(define-struct while-exp (condicion cuerpo) #:transparent) 
(define-struct funcion-expr (nombre argumentos tipo cuerpo) #:transparent)
(define-struct fun (nombre argumentos cuerpo) #:transparent)
(define-struct return (valor) #:transparent)
(define-struct main (exp func-list) #:transparent)
(define-struct int-t () #:transparent) 
(define-struct bool-t () #:transparent) 
(define-struct decl-arr (i tipo args) #:transparent) 
(define-struct array (tipo) #:transparent) 
(define-struct length (var) #:transparent) 
(define-struct program (m) #:transparent)
(define-struct declaraciones-multiples (t ids) #:transparent) 

(define-struct String (s) #:transparent)
(define-struct string-t ( ) #:transparent)
(define-struct if-else-exp (condicion si no) #:transparent)
(define-struct for (declaracion condicion cuerpo) #:transparent)
(define-struct printLn (exp) #:transparent)

(define jelly-parser
    (parser
        [start program]
        [end EOF]
        [tokens contenedores vacios]
        [error (lambda (tok-ok? tok-name tok-value)
           (raise-syntax-error 'error
                               "no fue posible procesar un token"
                               (if tok-value tok-value tok-name)))]
        [precs  (nonassoc 
                    MAIN 
                    INT BOOL BOOLEAN
                    LK RK 
                    LP RP 
                    LBr RBr
                    : COM SEMICOLON
                    IF 
                    NOT)
                (right RETURN)
                (right -- ++)
                (right QUESTION_MARK)
                (right = += -= *= /=)
                (left * / %)                
                (left + -)
                (left < > <= >=)
                (left WHILE == != AND OR)
                ]
        [grammar
            ; el main debe estar declarado al principio
            [program 
                ; [(stmt) $1]
                [(metodos) $1]
                [(main) $1]
                [(main metodos) (list* $1 $2)]]

            ; declaración de main
            [main 
                [(MAIN bloque) (main $2)]]        

            ; métodos / funciones 
            [metodos  
                [(metodo) $1]
                [(metodo metodos) (list* $1 $2)]] 

            ; declaración de función/método
            [metodo
                [(id LP params RP : tipo bloque) (funcion-expr $1 $3 $6 $7)] ;con tipo
                [(id LP params RP bloque) (fun $1 $3 $5)]     ;sin tipo
                ]  

         
            ; Parámetros al declarar una función 
            [params     
                [(param COM params) (list* $1 $3)]
                [(param) (list $1)]
                ]

            [param 
                [(id : tipo) (decl-var $1 $3)]
                [(id : tipo LBr RBr) (arr $1 $3 empty)]
                [(id : tipo LBr NUM RBr) (arr $1 $3 $5)]]

            ; Bloque de código rodeado de llaves { código }
            [bloque
                [(LK RK) empty]
                [(LK lineas RK) $2]
                [(LK expr RK) $2]

                ]

            ; Líneas de código separadas por \n
            [lineas
                [(stmt lineas) (list* $1 $2)]
                [(stmt) (list $1)]
                ]

            ; Código intermedio
            [stmt
                [(if) $1]
                [(fun-call) $1]
                [(while) $1]
                [(return) $1]
                [(asign) $1]
                ; [(params) $1]
                [(bloque) $1]
                [(expr) $1]
                ]

            ; Asignación de variables
            [asign                
                [(param = expr) (bin-exp '= $1 $3)]
                [(param = LK consts RK) (bin-exp '= $1 $4)]
                [(id LBr expr RBr = expr) (asign-arr $1 $3 $6)]
                [(id = expr) (bin-exp '= $1 $3)]
                [(id = asign) (bin-exp '= $1 $3)]
                [(id = LK consts RK) (bin-exp '= $1 $4)]]

            [if
            [(IF LP expr RP bloque) (if-exp $3 $5)]
            [(IF LP expr RP bloque) (if-exp $3 $5)]
            [(IF LP expr RP LK bloque RK) (if-exp $3 $6)]                [(IF expr bloque) (if-corto-exp $2 $3)]     ;considerando ifs sin {}
                [(IF expr stmt) (if-corto-exp $2 $3)]     ;considerando ifs sin {}
                [(IF LP expr RP bloque ELSE bloque) (if-else-exp $3 $5 $7)]
                [(IF LP expr RP LK bloque RK ELSE bloque) (if-else-exp $3 $6 $9)]
                [(IF LP expr RP LK bloque RK ELSE LK bloque RK) (if-else-exp $3 $6 $10)]
                ]

            [while
                [(WHILE expr bloque) (while-exp $2 $3)]  
                ]

            [fun-call
                [(id LP exprs RP) (funcall $1 $3)]]

            [exprs 
                [(expr COM exprs) (list* $1 $3)]
                [(expr) $1]
            ]


            [return
                [(RETURN expr) (return $2)]]
                                        
            [expr
                [(STR) (String $1)]
                [(expr + expr) (bin-exp '+ $1 $3)]
                [(expr - expr) (bin-exp '- $1 $3)]
                [(expr * expr) (bin-exp '* $1 $3)]
                [(expr / expr) (bin-exp '/ $1 $3)]
                [(expr % expr) (bin-exp '% $1 $3)]
                [(expr += expr) (bin-exp '= (id $1) (bin-exp '+ (id $1) $3))]
                [(expr -= expr) (bin-exp '= (id $1) (bin-exp '- (id $1) $3))]                
                [(expr == expr) (bin-exp '== $1 $3)]
                [(expr != expr) (bin-exp '!= $1 $3)]                
                [(expr > expr) (bin-exp '> $1 $3)]
                [(expr < expr) (bin-exp '< $1 $3)]
                [(expr >= expr) (bin-exp '>= $1 $3)]
                [(expr <= expr) (bin-exp '<= $1 $3)]
                [(expr AND expr) (bin-exp 'AND $1 $3)]
                [(expr OR expr) (bin-exp 'OR $1 $3)]
                [(expr ++) (bin-exp '= (id $1) (bin-exp '+ (id $1) (numero 1)))]
                [(expr --) (bin-exp '= (id $1) (bin-exp '- (id $1) (numero 1)))]
                [(id LBr expr RBr) (access-array $1 $3) ]
                [(fun-call) $1]   
                [(LP expr RP) $2]
                ; [(- expr) (un-exp '- $2)]
                [(NOT expr) (un-exp '! $2)]
                [(const) $1]
                [(id) $1]   
                [(PRINTLN LP expr RP) (printLn $3)]
          
                ]

            [final 
                [(id) $1]
                [(boolean) $1]
                [(numero) $1]]

            [consts 
                [(const COM consts) (list* $1 $3)]
                [(const) $1]]

            [const          
                [(numero) $1]
                [(boolean) $1]]

            ; Tipos
            [tipo
                [(INT) (type 'INT)] 
                [(STRING) (string-t)]               
                [(BOOL) (type 'BOOL)]]
                


            [ids 
                [(id COM ids) (list* $1 $3)]
                [(id) $1]
            ]

            [id 
                [(ID) (id $1)]]            

            [boolean
                [(BOOLEAN) (bool $1)]]
            
            [numero
                [(NUM) (numero $1)]]

            ]))
