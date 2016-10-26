#lang racket

;; Midterm Assignment 
(require parser-tools/lex
         parser-tools/yacc)
(require racket/trace)

;Tokens
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
;Lexer
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

;Structs
(struct unaryOpExpr (expr1) #:transparent)
(struct binaryOpExpr (expr1 expr2) #:transparent)
(struct andOpExpr (expr1 expr2) #:transparent)
(struct orOpExpr (expr1 expr2) #:transparent)
(struct xorOpExpr (expr1 expr2) #:transparent)
(struct conditionalOpExpr (expr1 expr2 expr3) #:transparent)
(struct lambdaExpr (id expr1) #:transparent)
(struct function (id expr) #:transparent)
(struct letExpr (id value expr) #:transparent)
(struct letLambdaExpr (id lambda callExpr) #:transparent)
(struct callExpr (id expr) #:transparent)
(struct idExpr (value) #:transparent)
(struct valueExpr (val) #:transparent)

;Parser
(define expparser
  (parser
     (start exp)
     (end EOF)
     (tokens e v)
     (error void)
     (grammar
       (exp
         ((LEFTPAREN exp binaryOpExp exp RIGHTPAREN)
          (cond
            [(equal? $3 "and") (andOpExpr $2 $4)]
            [(equal? $3 "or") (orOpExpr $2 $4)]
            [else (xorOpExpr $2 $4)]
            ))         
         ((LEFTPAREN LET LEFTPAREN ID value RIGHTPAREN IN exp RIGHTPAREN) (letExpr $4 $5 $8))
         ((LEFTPAREN LET LEFTPAREN ID lambda RIGHTPAREN IN callExp RIGHTPAREN) (letLambdaExpr $4 $5 $8))
         ((LEFTPAREN IF exp THEN exp ELSE exp RIGHTPAREN) (conditionalOpExpr $3 $5 $7))
         ((LEFTPAREN UNARYOP exp RIGHTPAREN) (unaryOpExpr $3))
         ((ID) (idExpr $1))
         ((VALUE) (valueExpr $1)))
       (binaryOpExp
         ((BINARYOP) $1))
       (value
        ((LEFTPAREN exp binaryOpExp exp RIGHTPAREN)
          (cond 
            [(equal? $3 "and") (andOpExpr $2 $4)]
            [(equal? $3 "or") (orOpExpr $2 $4)]
            [else (xorOpExpr $2 $4)]
            ))
        ((LEFTPAREN UNARYOP exp RIGHTPAREN) (unaryOpExpr $3))
        ((VALUE) (valueExpr $1)))
       (id
         ((ID) (idExpr $1)))
       (lambda
         ((LEFTPAREN LAMBDA LEFTPAREN ID RIGHTPAREN exp RIGHTPAREN) (lambdaExpr $4 $6)))
       (callExp
          ((LEFTPAREN CALL ID WITH exp RIGHTPAREN) (callExpr $3 $5))))))

(define (lex-this lexer input)
  (lambda () (lexer input)))

;Environment Stuff
;Implement an Environment

; empty-env : () -> Env
(define empty-env
  (lambda () (list 'empty-env)))

; extend-env : Var x SchemeVal x Env -> Env
(define extend-env
  (lambda (var val env)
    (list 'extend-env var val env)))

; apply-env : Env x Var -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((equal? (car env) 'empty-env)
       (error "No Binding Found: ~s" search-var))
      ((equal? (car env) 'extend-env)
       (let ((saved-var (cadr env))
             (saved-val (caddr env))
             (saved-env (cadddr env)))
         (if (equal? search-var saved-var)
             saved-val
             (apply-env saved-env search-var))))
      (else
       (error "Invalid Environment: ~s"  env)))))
(trace apply-env)
; END environment stuff

; Evaluater
(define (evaluate aTree env)
  (match aTree
        [(valueExpr a) (equal? a "true")]
        [(unaryOpExpr a) (not (evaluate a env))]
        [(andOpExpr a b) (and (evaluate a env) (evaluate b env))]
        [(orOpExpr a b) (or (evaluate a env) (evaluate b env))]
        [(xorOpExpr a b) (xor (evaluate a env) (evaluate b env))]
        [(conditionalOpExpr a b c)
         (cond
       [(and #t (and #t (evaluate a env))) (evaluate b env)]
       [else (evaluate c env)])]
        [(idExpr a) (apply-env env a)]
        [(letExpr a b c) (evaluate c (extend-env a (evaluate b env) env))]
        [(callExpr a b) (cons (evaluate b env) empty)] 
        [(lambdaExpr a b) (cons a (cons b empty))]
        [(letLambdaExpr a b c) (evaluate (second (evaluate b env)) (extend-env (first (evaluate b env)) (first (evaluate c env)) env))]))


;Helper function to call methods efficiently
(define (eval input)
  (evaluate (expparser (lex-this constantBoolLexer input)) (empty-env)))

;To call a text file
(define inputFile (open-input-file "test1.txt"))

;Tests
(define btest (open-input-string "(true or false)"))
(define utest (open-input-string "(not true)"))
(define valueTest (open-input-string "true"))
(define idTest (open-input-string "false"))
(define letTest (open-input-string "(let (foo false) in (foo and false))"))
(define ifTest (open-input-string "(if true then true else false)"))
(define callTest (open-input-string "(call foo with (if true then true else false))"))
(define lambdaTest (open-input-string "(let (foo (λ (x) (x and false))) in (call foo with true))"))
(define id1test (open-input-string "(foo or false)"))
(define unaryIdTest (open-input-string "not false"))
(define complicatedTest (open-input-string "(let (foo (λ (y) (y and true))) in (call foo with true))"))

;getTokens
(define (getTokens lex in)
  (let [(token (lex in))]
    (cond [(equal? token 'eof) '()]
          [else (cons token (getTokens lex in))])))