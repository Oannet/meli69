#lang nanopass

(provide contenedores
         vacios
         jelly-lex
         token-NUM)
         
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens contenedores (NUM ID BOOLEAN STR))

(define-empty-tokens vacios 
                            (MAIN           ; "main"
                            INT             ; "int"
                            BOOL            ; "bool"
                            LK              ; "{"
                            RK              ; "}"
                            LP              ; "("
                            RP              ; ")"
                            LBr             ; "["
                            RBr             ; "]"
                            --              ; "--"
                            ++              ; "++"
                            +               ; "+"
                            -               ; "-"
                            *               ; "*"
                            /               ; "/"
                            %               ; "%"
                            =               ; "="                            
                            ==              ; "=="
                            !=              ; "!="
                            >               ; ">"
                            >=              ; ">="
                            <               ; "<"
                            <=              ; "<="
                            +=              ; "+="
                            -=              ; "-="
                            *=              ; "*="
                            /=              ; "/="
                            :               ; ":"
                            %=              ; "%="
                            COM             ; ","
                            AND             ; "&"
                            OR              ; "|"
                            NOT             ; "!"
                            IF              ; "if"
                            ELSE            ; "else"
                            WHILE           ; "while"
                            RETURN          ; "return"
                            QUESTION_MARK   ; "?"
                            SEMICOLON       ; ";"
                            NEG             ; -(n)
                            ;LEN             ; "length"
                            PRINTLN
                            STRING
                            EOF))

(define jelly-lex
  (lexer
        [(:: "main") (token-MAIN)]
        [(:: "String") (token-STRING)]
        [(:: "int") (token-INT)]
        [(:: "bool") (token-BOOL)]
        [(:: "{") (token-LK)]
        [(:: "}") (token-RK)]
        [(:: "(") (token-LP)]
        [(:: ")") (token-RP)]
        [(:: "[") (token-LBr)]
        [(:: "]") (token-RBr)]
        [(:: "--") (token---)]
        [(:: "++") (token-++)] 
        [(:: "+") (token-+)]
        [(:: "-") (token--)]
        [(:: "*") (token-*)]
        [(:: "/") (token-/)]
        [(:: "%") (token-%)]
        [(:: "=") (token-=)]
        [(:: "==") (token-==)]
        [(:: "!=") (token-!=)]
        [(:: ">") (token->)]
        [(:: ">=") (token->=)]
        [(:: "<") (token-<)]
        [(:: "<=") (token-<=)]
        [(:: "+=") (token-+=)]
        [(:: "-=") (token--=)]
        [(:: "*=") (token-*=)]
        [(:: "/=") (token-/=)]
        [(:: ":") (token-:)]
        [(:: "%=") (token-%=)]
        [(:: ",") (token-COM)]
        [(:: "&") (token-AND)]
        [(:: "|") (token-OR)]
        [(:: "!") (token-NOT)]
        [(:: "if") (token-IF)]
        [(:: "else") (token-ELSE)]
        [(:: "while") (token-WHILE)]
        [(:: "return") (token-RETURN)]
        [(:: "printLn") (token-PRINTLN)]
        ;[(:: "length") (token-LEN)] 
        [(:: "?") (token-QUESTION_MARK)]
        [(:: ";") (token-SEMICOLON)]
        [(:+ (char-range #\0 #\9))  (token-NUM (string->number lexeme))]
        [(:seq (char-range #\a #\z) (:+ (:or (char-range #\a #\z) (char-range #\0 #\9) #\_)))  (token-ID lexeme)] 
        [(:+ (char-range #\a #\z))(token-ID lexeme)]
        [(:+ numeric) (token-NUM (string->number lexeme))]
        [(:: "True")  (token-BOOLEAN  (string->symbol lexeme))]
        [(:: "False") (token-BOOLEAN  (string->symbol lexeme))]
        [(:: "//" (complement (:: any-string "\n" any-string)) "\n") (jelly-lex input-port)]
        [(:: "{-" (complement (:: any-string "-}" any-string)) "-}") (jelly-lex input-port)]
        [(:: "\""any-string"\"") (token-STR (string->symbol lexeme))]
        [whitespace (jelly-lex input-port)]
        [any-char (error "Lexema no reconocido: " lexeme)]
        [(eof) (token-EOF)]))