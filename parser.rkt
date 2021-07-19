#lang racket

(require (lib "eopl.ss" "eopl"))
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre)
         parser-tools/yacc)


(define simple-math-lexer
           (lexer
(";" (token-s-semicolon))
("pass" (token-kw-pass))
("break" (token-kw-break))
("continue" (token-kw-continue))
("=" (token-s-assign))
("return" (token-kw-return))
("global" (token-kw-global))
("def" (token-kw-def))
("(" (token-s-open-par))
(")" (token-s-close-par))
(":" (token-s-colon))
("," (token-s-comma))
("if" (token-kw-if))
("else" (token-kw-else))
("for" (token-kw-for))
("in" (token-kw-in))
("or" (token-kw-or))
("and" (token-kw-and))
("not" (token-kw-not))
("==" (token-s-eq))
("<" (token-s-lt))
(">" (token-s-gt))
("<=" (token-s-lte))
(">=" (token-s-gte))
("+" (token-s-plus))
("-" (token-s-minus))
("*" (token-s-mult))
("/" (token-s-div))
("**" (token-s-pow))
("+" (token-s-plus-assign))
("-" (token-s-minus-assign))
("*" (token-s-mult-assign))
("/" (token-s-div-assign))
("**" (token-s-pow-assign))
("[" (token-s-open-brac))
("]" (token-s-close-brac))
((:or (:+ (char-range #\0 #\9)) (:: (:+ (char-range #\0 #\9)) #\. (:+ (char-range #\0 #\9)))) (token-a-number (string->number lexeme)))
("True" (token-a-true))
("False" (token-a-false))
("None" (token-a-none))
((:: (:or (char-range #\a #\z) (char-range #\A #\Z) #\_) (:* (:or (char-range #\a #\z) (char-range #\A #\Z) #\_ (char-range #\0 #\9)))) (token-identifier (string->symbol lexeme)))
(whitespace (simple-math-lexer input-port))
((eof) (token-EOF))))

(define-tokens a (identifier a-number))
(define-empty-tokens b (
    EOF s-semicolon kw-pass kw-break kw-continue s-assign kw-return kw-global kw-def s-open-par s-close-par 
    s-colon s-comma kw-if kw-else kw-for kw-in kw-or kw-and kw-not s-eq s-lt s-gt s-lte s-gte s-plus s-minus s-mult s-div 
    s-pow s-plus-assign s-minus-assign s-mult-assign s-div-assign s-pow-assign s-open-brac s-close-brac a-true a-false a-none
))
(define-datatype program program?
  (a-program
   (prgrm-stmts statements?)))

(define-datatype statements statements?
    (a-stmt
        (stmt statement?))
    (a-stmts
        (stmt statement?)
        (stmts statements?)))

(define-datatype statement statement?
    (stmt-compound-stmt
        (stmt compound-stmt?))
    (stmt-simple-stmt
        (stmt simple-stmt?)))

(define-datatype simple-stmt simple-stmt?
    (simple-stmt-assignment
        (assignment assignment-stmt?))
    (simple-stmt-return-stmt
        (stmt return-stmt?))
    (simple-stmt-global-stmt
        (stmt global-stmt?))
    (simple-stmt-pass)
    (simple-stmt-break)
    (simple-stmt-continue))

(define-datatype compound-stmt compound-stmt?
    (compound-stmt-function-def 
        (stmt function-def?))
    (compound-stmt-if-stmt 
        (stmt if-stmt?))
    (compound-stmt-for-stmt 
        (stmt for-stmt?)))

(define-datatype assignment-stmt assignment-stmt?
    (an-assignment-stmt 
        (id symbol?)
        (exp expression?)))

(define-datatype return-stmt return-stmt?
    (void-return-stmt)
    (value-return-stmt
        (exp expression?)))

(define-datatype global-stmt global-stmt?
    (a-global-stmt 
        (id symbol?)))

(define-datatype function-def function-def?
    (function-def-with-params
        (id symbol?)
        (params params?)
        (stmts statements?))
    (funcion-def-without-params
        (id symbol?)
        (stmts statements?)))

(define-datatype params params?
    (a-param 
        (params params-with-default?)
    (a-params
        (params params?)
        (param params-with-default?))))

(define-datatype params-with-default params-with-default?
    (a-param-with-default
        (id symbol?)
        (exp expression?)))

(define-datatype if-stmt if-stmt?
    (an-if-stmt
        (exp1 expression?)
        (stmts statements?)
        (else-block else-block?)))


(define-datatype expression expression?
    (an-expression
        (exp disjunction-exp?)))

(define-datatype disjunction-exp disjunction-exp?
    (a-cnj-disj
        (exp conjunction?))
    (disjunction-or-conjuction
        (dis-exp disjunction?)
        (con-exp conjunction?)))

(define-datatype conjuction-exp conjuction-exp?
    (an-inv-conj
        (exp inversion?))
    (conjuction-and-inversion
        (con-exp conjunction?)
        (inv-exp inversion?)))

(define-datatype inversion-exp inversion-exp?
    (not-inversion
        (inv-exp inversion-exp?))
    (a-comparison-inv
        (cmp-exp comparison-exp?)))

(define-datatype comparison-exp comparison-exp?
    (a-sum-comparison
        (exp sum-exp?))
    (a-cmp-op-sum-pairs-comparison
        (exp1 sum-exp?)
        (exp2 cmp-op-sum-pairs-exp?)))

(define-datatype cmp-op-sum-pairs-exp cmp-op-sum-pairs-exp?
    (a-pair-cmp-op-sum-pairs
        (exp cmp-op-sum-pair-exp?))
    (a-cmp-op-sum-pairs
        (exp1 cmp-op-sum-pairs-exp?)
        (exp2 cmp-op-sum-pair-exp?)))

(define-datatype cmp-op-sum-pair-exp cmp-op-sum-pair-exp?
    (an-eq-sum-cmp
        (exp eq-sum-exp?))
    (an-lt-sum-cmp 
        (exp lt-sum-exp?))
    (a-gt-sum-cmp 
        (exp gt-sum-exp?)))

(define-datatype eq-sum-exp eq-sum-exp?
    (an-eq-sum 
        (exp sum-exp?)))

(define-datatype lt-sum-exp lt-sum-exp?
    (an-lt-sum 
        (exp sum-exp?)))

(define-datatype gt-sum-exp gt-sum-exp?
    (a-gt-sum 
        (exp sum-exp?)))

(define-datatype sum-exp sum-exp?
    (a-sum
        (exp1 sum-exp?)
        (exp2 term-exp?))
    (a-minus 
        (exp1 sum-exp?)
        (exp2 term-exp?))
    (a-term-sum
        (exp term-exp?)))

(define-datatype term-exp term-exp?
    (a-mul-term 
        (exp1 term-exp?)
        (exp2 factor-exp?)
    (a-div-term 
        (exp1 term-exp?)
        (exp2 factor-exp?)))
    (a-term
        (exp term-exp?)))

(define-datatype factor-exp factor-exp?
    (a-plus-factor 
        (exp factor-exp?))
    (a-minus-factor 
        (exp factor-exp?))
    (a-primary-factor 
        (exp primary-exp?)))

(define-datatype primary-exp primary-exp?
    (an-atom-primary
        (exp atom-exp?))
    (an-array-primary
        (prim primary-exp?)
        (exp expression?))
    (a-callable-primary 
        (prim primary-exp?))
    (a-callable-primary-with-args
        (prim primary-exp?)
        (args arguments-exp?)))

(define-datatype arguments-exp arguments-exp?
    (argument
        (exp expression?))
    (arguments 
        (args arguments-exp?)
        (exp expression?)))

(define-datatype atom-exp atom-exp?
    (id-atom
        (id symbol?))
    (bool-atom
        (val bool?))
    (none-atom)
    (num-atom
        (val number?))
    (list-atom
        (val list-exp?)))

(define-datatype list-exp list-exp?
    (a-list
        (exps expressions?)
    (empty-list)))    
(define-datatype expressions expressions?
    (a-exps
        (exps expressions?)
        (exp expression?))
    (a-exp
        (exp expression?)))


(define simple-math-parser
    (parser
        (start statements)
        (end EOF)
        (error void)
        (tokens a b)
        (grammar
(statements
    ((statement s-semicolon) $1)
    ((statements statement s-semicolon) (cons $1 $2)))
(statement
    ((compound-stmt) $1)
    ((simple-stmt) $1))
(simple-stmt
    ((assignment) $1)
    ((return-stmt) $1)
    ((global-stmt) $1)
    ((kw-pass) (kw-pass-exp))
    ((kw-break) (kw-break-exp))
    ((kw-continue)(kw-continue-exp)))
(compound-stmt
    ((function-def) $1)
    ((if-stmt) $1)
    ((for-stmt) $1))
(assignment
    ((identifier s-assign expression) (assingment-exp $1 $3)))
(return-stmt
    (
(kw-return) (kw-return-exp))
    ((kw-return expression) (return-exp)))
(global-stmt
    ((kw-global identifier) (global-exp $2)))
(function-def
    ((kw-def identifier s-open-par params s-close-par s-colon statements) (def-exp $2 $4 $7))
    ((kw-def identifier s-open-par s-close-par s-colon statements) (empty-def-exp $2 $6)))
(params
    ((param-with-default) $1)
    ((params s-comma param-with-default) (param-comma-exp $1 $3)))
(param-with-default
    ((identifier s-assign expression) (assign-exp $1 $3)))
(if-stmt
    ((kw-if expression s-colon statements else-block) (if-exp $2 $4)))
(else-block
    ((kw-else s-colon statements) (else-exp $3)))
(for-stmt
    ((kw-for identifier kw-in expression s-colon statements) (for-exp $2 $4 $6)))
(expression
    ((disjunction) $1))
(disjunction
    ((conjunction) $1)
    ((disjunction kw-or conjunction) (or-exp $1 $3)))
(conjunction
    ((inversion) $1)
    ((conjunction kw-and inversion) (and-exp $1 $3)))
(inversion
    ((kw-not inversion) (not-exp $2))
    ((comparison) $1))
(comparison
    ((sum compare-op-sum-pairs) (sum-compare-exp $1 $2))
    ((sum) $1))
(compare-op-sum-pairs
    ((compare-op-sum-pair) $1)
    ((compare-op-sum-pairs compare-op-sum-pair) (multi-compare-exp $2)))
(compare-op-sum-pair
    ((eq-sum) $1)
    ((lt-sum) $1)
    ((gt-sum) $1))
(eq-sum
    ((s-eq sum) (eq-exp $2)))
(lt-sum
    ((s-lt sum) (lt-exp $2)))
(gt-sum
    ((s-gt sum) (gt-exp $2)))
(sum
    ((sum s-plus term) (sum-exp $1 $3))
    ((sum s-minus term) (minus-exp $1 $3))
    ((term) $1))
(term
    ((term s-mult factor) (mul-exp $1 $3))
    ((term s-div factor) (div-exp $1 $3))
    ((factor) $1))
(factor
    ((s-plus factor) (pos-num-exp $2))
    ((s-minus factor) (neg-num-exp $2))
    ((power) $1))
(power
    ((atom s-pow factor) (pow-exp $1 $3))
    ((primary) $1))
(primary
    ((atom) $1)
    ((primary s-open-brac expression s-close-brac) (array-peak-exp $1 $3))
    (
(primary s-open-par s-close-par) (call-exp $1))
    ((primary s-open-par arguments s-close-par) (call-arg-exp $1 $3)))
(arguments
    ((expression) $1)
    ((arguments s-comma expression) (arg-exp-exp $1 $3)))
(atom
    ((identifier) (id-exp $1))
    ((a-true) (bool-exp 'f))
    ((a-false) (bool-exp 'f))
    ((a-none) (none-exp))
    ((a-number) (num-exp $1))
    ((List) $1))
(List
    ((s-open-brac expressions s-close-brac) (bracket-exp $2))
    ((s-open-brac s-close-brac) (empty-bracket-exp)))
(expressions
    ((expressions s-comma expression) (comma-exp $1 $3))
    ((expression) $1)))))

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a = 2;")))
;(simple-math-parser my-lexer)
(define parser-res (simple-math-parser my-lexer))

