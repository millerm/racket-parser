#lang racket

(require parser-tools/lex
         parser-tools/yacc)
                         
(define-tokens v (VALUE
                  BINARYOP
                  UNARYOP
                  ASSIGNMENT
                  LET
                  IF
                  THEN
                  ELSE
                  CALL
                  WITH
                  LAMBDA
                  FUNCTION
                  CONDITIONAL
                  ID
                  IN))
 (define-empty-tokens e (EOF
                         LEFTPAREN
                         RIGHTPAREN
                       ))

(define constantBoolLexer
  (lexer
   ["true" (token-VALUE lexeme)]
   ["false" (token-VALUE lexeme)]
   ["and" (token-BINARYOP lexeme)]
   ["or" (token-BINARYOP lexeme)]
   ["xor" (token-BINARYOP lexeme)]
   ["not" (token-UNARYOP lexeme)]
   ["let" (token-LET lexeme)]
   ["in" (token-IN lexeme)]
   ["if" (token-IF lexeme)]
   ["then" (token-THEN lexeme)]
   ["else" (token-ELSE lexeme)]
   ["call" (token-CALL lexeme)]
   ["with" (token-WITH lexeme)]
   [#\λ (token-LAMBDA lexeme)]
   [#\( (token-LEFTPAREN)]
   [#\) (token-RIGHTPAREN)]
   [#\space (constantBoolLexer input-port)]
   [#\newline (constantBoolLexer input-port)]
   [whitespace (constantBoolLexer input-port)]
   [(concatenation alphabetic (repetition 0 +inf.0 (union alphabetic numeric))) (token-ID lexeme)]
   [(eof) (token-EOF)]
   ))

(struct unaryOpExpr (expr1) #:transparent)
(struct binaryOpExpr (expr1 expr2) #:transparent)
(struct conditionalOpExpr (expr1 expr2 expr3) #:transparent)
(struct lambdaExpr (id expr) #:transparent)
(struct function (id expr) #:transparent)
(struct letExpr (id value expr) #:transparent)
(struct callExpr (id expr) #:transparent)
(struct idExpr (value) #:transparent)
(struct valueExpr (val) #:transparent)

(define expparser
  (parser
     (start exp)
     (end EOF)
     (tokens e v)
     (error void)
     (grammar
       (exp
         ((LEFTPAREN exp binaryOpExp exp RIGHTPAREN) (binaryOpExpr $2 $4))
         ((LEFTPAREN LET LEFTPAREN id exp RIGHTPAREN IN exp RIGHTPAREN) (letExpr $4 $5 $8))
         ((LEFTPAREN LET LEFTPAREN id lambdaExp RIGHTPAREN IN exp RIGHTPAREN) (lambdaExpr $4 $8))
         ((LEFTPAREN IF exp THEN exp ELSE exp RIGHTPAREN) (conditionalOpExpr $3 $5 $7))
         ((LEFTPAREN CALL id WITH exp RIGHTPAREN) (callExpr $3 $5))
         ((UNARYOP exp) (unaryOpExpr $2))
         ((VALUE) (valueExpr $1)))
       (binaryOpExp
         ((BINARYOP) $1))
       (id
         ((ID) (idExpr $1)))
       (lambdaExp
         ((LEFTPAREN LAMBDA LEFTPAREN id RIGHTPAREN exp RIGHTPAREN) (lambdaExpr $4 $6))))))

(define (evaluate aTree)
  (match aTree
        [(binaryOpExpr a b) (+ (evaluate a)
                          (evaluate b))]
       
         ))

(define (lex-this lexer input)
  (lambda () (lexer input)))
         

(define btest (open-input-string "(true or false)"))
(define utest (open-input-string "not true"))
(define valueTest (open-input-string "true"))
(define idTest (open-input-string "true"))
(define letTest (open-input-string "(let (foo true) in true)"))
(define ifTest (open-input-string "(if true then true else false)"))
(define callTest (open-input-string "(call foo with true)"))
(define lambdaTest (open-input-string "(let (foo (λ (foo) true)) in true)"))

(define (getTokens lex in)
  (let [(token (lex in))]
    (cond [(equal? token 'eof) '()]
          [else (cons token (getTokens lex in))])))


