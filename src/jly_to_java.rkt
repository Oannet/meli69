#lang nanopass
(require "language.rkt")
(provide (all-defined-out))

;; ======================================== JELLY A JAVA ====================================

(define (java ir)
  (java-program ir))


;; PROGRAMA
(define (java-program ir)
    (nanopass-case (jelly Programa) ir
        [(programa ,m ,func* ...) (let* ([main-java (java-main m)]
                                         [funcs-java (string-join (map (lambda (f) (string-append (java-func f) "\n")) func*))])
                                    (string-append "public class Jelly {"
                                                   "\n"
                                                   main-java
                                                   "\n"
                                                   funcs-java
                                                   "\n"
                                                   "}"))]
        ))

;; MAIN
(define (java-main ir)
    (nanopass-case (jelly Main) ir
        [(main [,e* ...]) (let* ([expr-list (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n ")) e*))])
                            (string-append "public static void main(String[] args){\n"
                                           expr-list
                                           "\n"
                                           "}"))]
        ))

;; FUNCIONES
(define (java-func ir)
    (nanopass-case (jelly Funcion) ir
        [(funcion-expr ,i ([,i* ,t*] ...) ,t [,e* ...]) (let* ([func-id (symbol->string i)]
                                                              [arguments (decl-aux i* t*)]
                                                              [return-type (symbol->string t)]                                                              
                                                              [expr-list (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) e*))])
                                                          (string-append "static " return-type
                                                                         " "
                                                                         func-id
                                                                         " ("
                                                                         arguments
                                                                         ")"
                                                                         " {\n"
                                                                         expr-list
                                                                         "\n"
                                                                         "}"
                                                                         ))]
        
        [(funcion-void-expr ,i ([,i* ,t*] ...) [,e* ...]) (let* ([func-id (symbol->string i)]
                                                              [arguments (decl-aux i* t*)]                                                              
                                                              [expr-list (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) e*))])
                                                          (string-append "static void " 
                                                                         func-id
                                                                         " ("
                                                                         arguments
                                                                         ")"
                                                                         " {\n"
                                                                         expr-list
                                                                         "\n"
                                                                         "}"
                                                                         ))]
        [else (begin
                (display ir)
                "Func")]))




;; EXPR en general
(define (java-expr ir)
    (nanopass-case (jelly Expr) ir
        [,c (cond
                       [(boolean? c) (boolean-to-string c)]
                       [(number? c)  (number->string c)]
                       [(string? c) (string-append "\"" c "\"")])]
        [,i (symbol->string i)]
        [,pr (symbol->string pr)]
        [,spr (symbol->string spr)]
        [(,spr, e) (string-append (symbol->string spr) (java-expr e))]
        ((,pr ,e1 ,e2) (string-append (java-expr e1) " " (symbol->string pr) " " (java-expr e2)))
        [(,i ,t) (string-append (symbol->string t) " " (symbol->string i))]
        [(= ,i ,t ,[e -> te]) (string-append (symbol->string t) " " (symbol->string i) " = " te)]
        [(= ,i ,[e -> te]) (string-append (symbol->string i) " = " te)]
        
        [(,t ,i* ... , i) (string-append (symbol->string t) " " (var-list (append i* (list i))))] ;; int a, b, c
        
        [(if-exp ,[e0 -> te0] [,e1* ... ,e]) (string-append "if ("
                                                                             te0
                                                                             ")"
                                                                             " {"
                                                                             "\n"
                                                                             (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) (append e1* (list e))))
                                                                             "\n"
                                                                             "}")]
        
        [(if-else-exp ,[e0 -> te0] [,e1* ... ,e2] [,e3* ...]) (string-append "if ("
                                                                             te0
                                                                             ")"
                                                                             " {"
                                                                             "\n"
                                                                             (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) (append e1* (list e2))))
                                                                             "\n"
                                                                             "} else{"
                                                                             "\n"
                                                                             (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) e3*))
                                                                             "\n"
                                                                             "}"
                                                                             )]
        [(while-exp ,[e0 -> te0] [,e1* ...]) (string-append "while ("
                                                       te0
                                                       ")"
                                                       "{\n"
                                                       (string-join (map (lambda (exp) (string-append (java-expr exp) ";\n")) e1*))
                                                       "\n"
                                                       "}"
                                                       )]
        [(decl-arr ,i ,t [,e* ...]) (string-append (symbol->string t)
                                                   "[] "
                                                    (symbol->string i)
                                                    " = "
                                                    "{"
                                                    (arr-list e*)
                                                    "}"
                                                    )]
        [(access-array ,i ,[e -> te]) (string-append (symbol->string i)
                                                     "["
                                                     te
                                                     "]")]
        [(funcall ,i [,e* ...]) (string-append (symbol->string i)
                                                 "("
                                                 (arr-list e*)
                                                 ")")]
        [(return ,[e -> te]) (string-append "return "
                                            te
                                            )]
        
        [(len , [e -> te]) (string-append te ".length")]

        [(printLn, [e -> te]) (string-append "System.out.println"
                                             "("
                                             te
                                             ")")]
        
        ;[(,[e*] ... ,[e]) (string-join (map (lambda (exp) (string-append (java-expr exp) ";")) (append e* (list e))))]
        [else "Expr"]))



;; Funcion auxiliar para obtener la longitud de una lista
(define (list-length lst)
  (cond
    [(empty? lst) 0]        
    [else (+ 1 (list-length (cdr lst)))]))


;; Funcion auxiliar para declaracion multiple
(define (var-list l)
  (cond
    [(empty? l) ""]
    [(= 1 (list-length l)) (symbol->string (car l))]
    [else (string-append (symbol->string (car l)) ", "
                               (var-list (cdr l)))]
    )
  )

;; Funcion auxiliar para la declaracion de un arreglo explicito
(define (arr-list l)
  (cond
    [(empty? l) ""]
    [(= 1 (list-length l)) (java-expr (car l))]
    [else (string-append (java-expr (car l))
                         ", "
                         (arr-list (cdr l)))]
    )
  )

; Función auxiliar para poner comas "," entre declaraciones
(define (decl-aux ids types)
  (cond
    [(empty? ids) ""]
    [(= 1 (list-length ids)) (string-append (types-aux (car types)) " " (symbol->string (car ids)))]
    [else (string-append (types-aux (car types)) " " (symbol->string (car ids))
                         ", "
                         (decl-aux (cdr ids) (cdr types)))]))

;; Funcion auxiliar para la declaracion de variables de acuerdo a su tipox
(define (types-aux t)
  (cond
    [(eq? t 'int) "int"]
    [(eq? t 'bool) "boolean"]
    [(eq? t 'String) "String"]
    [(eq? t 'array_String) "String[]"]
    [(eq? t 'array_int) "int[]"]
    [else "boolean[]"]))

;; Funcion para parsear un expresion Jelly, renombrarla y pasarla a Java
(define (parse-java archivo)
    (java-program (parsea-archivo archivo)))



;; Funcion para escribir el archivo en java
(define (genera-archivo-java input-file output-file)
  (with-output-to-file output-file #:exists 'truncate
    (lambda ()
      (newline)
      (display (parse-java input-file)))))



(define prog-ex '(programa (main [(= a int 5) (= b int 4) (= j int 0) (= i int 4) (if-else-exp (> a b) ((= j (+ j 1))) ((= i (+ i 1)))) (= j (+ j 1)) (= i (+ i 1)) (= res int (funcall gdc (i j)))])
                        (funcion-expr gdc [(varu int) (vard int)] int [(if-else-exp (> varu vard) ((= varu (+ varu 1))) ((= vard (+ vard 1)))) (return (+ varu vard))] )))

(define prog-ex2 '(programa (main [(a int) (b int) (e int) (= e 4) (d int) (decl-arr a1 int ((+ 5 5) (+ 5 7) 5)) (= d (access-array a1 2)) (= a 5) (= b 9) (if-else-exp (> a b) [(x int) (= x 100) (= a (+ x a)) (= b (+ b 2))]
                                                                            [(if-else-exp (> b a) [(= b (- b 1))] [(= b (- b 1))])])])
                        (funcion-void-expr gdc [(varu int)(vard int)(vari bool)] [(+ c d)] )
                        (funcion-expr max-elem [(a array_int)(current int)] int [(hola int) (= hola (access-array a 0)) (return hola)] )))

(define entrada (getenv "entrada"))
(define salida (getenv "salida"))
(genera-archivo-java entrada salida)
;(genera-archivo-java "input/in.jly" "output/Jelly.java")




    







