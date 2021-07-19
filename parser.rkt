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
        (stmts statements?)
        (stmt statement?)))

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
        (params params-with-default?))
    (a-params
        (params-exp params?)
        (param-exp params-with-default?)))

(define-datatype params-with-default params-with-default?
    (a-param-with-default
        (id symbol?)
        (exp expression?)))

(define-datatype if-stmt if-stmt?
    (an-if-stmt
        (exp1 expression?)
        (stmts statements?)
        (else-block else-block?)))

(define-datatype else-block else-block?
    (an-else-block
        (stmts statements?)))

(define-datatype for-stmt for-stmt?
    (a-for-stmt 
        (id symbol?)
        (exp expression?)
        (stmts statements?)))

(define-datatype expression expression?
    (an-expression
        (exp disjunction-exp?)))

(define-datatype disjunction-exp disjunction-exp?
    (a-cnj-disj
        (exp conjunction-exp?))
    (or-exp
        (dis-exp disjunction-exp?)
        (con-exp conjunction-exp?)))

(define-datatype conjunction-exp conjunction-exp?
    (an-inv-conj
        (exp inversion-exp?))
    (and-exp
        (con-exp conjunction-exp?)
        (inv-exp inversion-exp?)))

(define-datatype inversion-exp inversion-exp?
    (not-exp
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
    (eq-sum-cmp
        (exp eq-sum-exp?))
    (lt-sum-cmp 
        (exp lt-sum-exp?))
    (gt-sum-cmp 
        (exp gt-sum-exp?)))

(define-datatype eq-sum-exp eq-sum-exp?
    (a-eq
        (exp sum-exp?)))

(define-datatype lt-sum-exp lt-sum-exp?
    (a-lt
        (exp sum-exp?)))

(define-datatype gt-sum-exp gt-sum-exp?
    (a-gt
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
        (exp2 factor-exp?))
    (a-div-term 
        (exp1 term-exp?)
        (exp2 factor-exp?))
    (a-term
        (exp factor-exp?)))

(define-datatype factor-exp factor-exp?
    (a-plus-factor 
        (exp factor-exp?))
    (a-minus-factor 
        (exp factor-exp?))
    (a-power-factor 
        (exp power-exp?)))

(define-datatype power-exp power-exp?
    (a-power
        (atom atom-exp?)
        (factor factor-exp?))
    (a-primary-power
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
        (val boolean?))
    (none-atom)
    (num-atom
        (val number?))
    (list-atom
        (val list-exp?)))

(define-datatype list-exp list-exp?
    (a-list
        (exps expressions?))
    (empty-list))

(define-datatype expressions expressions?
    (a-exps
        (exps expressions?)
        (exp expression?))
    (a-exp
        (exp expression?)))

(define simple-math-parser
    (parser
        (start program)
        (end EOF)
        (error void)
        (tokens a b)
        (grammar
            (program
                ((statements) (a-program $1)))
            (statements
                ((statement s-semicolon) (a-stmt $1))
                ((statements statement s-semicolon) (a-stmts $1 $2)))
            (statement
                ((compound-stmt) (stmt-compound-stmt $1))
                ((simple-stmt) (stmt-simple-stmt $1)))
            (simple-stmt
                ((assignment) (simple-stmt-assignment $1))
                ((return-stmt) (simple-stmt-return-stmt $1))
                ((global-stmt) (simple-stmt-global-stmt $1))
                ((kw-pass) (simple-stmt-pass))
                ((kw-break) (simple-stmt-break))
                ((kw-continue) (simple-stmt-continue)))
            (compound-stmt
                ((function-def) (compound-stmt-function-def $1))
                ((if-stmt) (compound-stmt-if-stmt $1))
                ((for-stmt) (compound-stmt-for-stmt $1)))
            (assignment
                ((identifier s-assign expression) (an-assignment-stmt $1 $3)))
            (return-stmt
                ((kw-return) (void-return-stmt))
                ((kw-return expression) (value-return-stmt $2)))
            (global-stmt
                ((kw-global identifier) (a-global-stmt $2)))
            (function-def
                ((kw-def identifier s-open-par params s-close-par s-colon statements) (function-def-with-params $2 $4 $7))
                ((kw-def identifier s-open-par s-close-par s-colon statements) (funcion-def-without-params $2 $6)))
            (params
                ((param-with-default) (a-param $1))
                ((params s-comma param-with-default) (a-params $1 $3)))
            (param-with-default
                ((identifier s-assign expression) (a-param-with-default $1 $3)))
            (if-stmt
                ((kw-if expression s-colon statements else-block) (an-if-stmt $2 $4)))
            (else-block
                ((kw-else s-colon statements) (an-else-block $3)))
            (for-stmt
                ((kw-for identifier kw-in expression s-colon statements) (a-for-stmt $2 $4 $6)))
            (expression
                ((disjunction) (an-expression $1)))
            (disjunction
                ((conjunction) (a-cnj-disj $1))
                ((disjunction kw-or conjunction) (or-exp $1 $3)))
            (conjunction
                ((inversion) (an-inv-conj $1))
                ((conjunction kw-and inversion) (and-exp $1 $3)))
            (inversion
                ((kw-not inversion) (not-exp $2))
                ((comparison) (a-comparison-inv $1)))
            (comparison
                ((sum compare-op-sum-pairs) (a-cmp-op-sum-pairs-comparison $1 $2))
                ((sum) (a-sum-comparison $1)))
            (compare-op-sum-pairs
                ((compare-op-sum-pair) (a-pair-cmp-op-sum-pairs $1))
                ((compare-op-sum-pairs compare-op-sum-pair) (a-cmp-op-sum-pairs $1 $2)))
            (compare-op-sum-pair
                ((eq-sum) (eq-sum-cmp $1))
                ((lt-sum) (lt-sum-cmp $1))
                ((gt-sum) (gt-sum-cmp $1)))
            (eq-sum
                ((s-eq sum) (a-eq $2)))
            (lt-sum
                ((s-lt sum) (a-lt $2)))
            (gt-sum
                ((s-gt sum) (a-gt $2)))
            (sum
                ((sum s-plus term) (a-sum $1 $3))
                ((sum s-minus term) (a-minus $1 $3))
                ((term) (a-term-sum $1)))
            (term
                ((term s-mult factor) (a-mul-term $1 $3))
                ((term s-div factor) (a-div-term $1 $3))
                ((factor) (a-term $1)))
            (factor
                ((s-plus factor) (a-plus-factor $2))
                ((s-minus factor) (a-minus-factor $2))
                ((power) (a-power-factor $1)))
            (power
                ((atom s-pow factor) (a-power $1 $3))
                ((primary) (a-primary-power $1)))
            (primary
                ((atom) (an-atom-primary $1))
                ((primary s-open-brac expression s-close-brac) (an-array-primary $1 $3))
                ((primary s-open-par s-close-par) (a-callable-primary $1))
                ((primary s-open-par arguments s-close-par) (a-callable-primary-with-args $1 $3)))
            (arguments
                ((expression) (argument $1))
                ((arguments s-comma expression) (arguments $1 $3)))
            (atom
                ((identifier) (id-atom $1))
                ((a-true) (bool-atom true))
                ((a-false) (bool-atom false))
                ((a-none) (none-atom))
                ((a-number) (num-atom $1))
                ((List) (list-atom $1)))
            (List
                ((s-open-brac expressions s-close-brac) (a-list $2))
                ((s-open-brac s-close-brac) (empty-list)))
            (expressions
                ((expressions s-comma expression) (a-exps $1 $3))
                ((expression) (a-exp $1))))))

(define empty-env
  (lambda () '()))

(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

(define apply-env
  (lambda (initial-env search-var)
    (letrec ((loop (lambda (env)
                     (cond ((null? env)
                            (report-no-binding-found search-var initial-env))
                           ((and (pair? env) (pair? (car env)))
                            (let ((saved-var (caar env))
                                  (saved-val (cdar env))
                                  (saved-env (cdr env)))
                              (if (eqv? search-var saved-var)
                                  saved-val
                                  (loop saved-env))))
                           (else
                            (report-invalid-env initial-env))))))
      (loop initial-env))))

(define report-no-binding-found
  (lambda (search-var env)
    (eopl:error 'apply-env "No binding for ~s in ~s" search-var env)))

(define report-invalid-env
  (lambda (env)
    (eopl:error 'apply-env "Bad environment ~s" env)))

(define init-env
  (lambda () (empty-env)))

(define run
  (lambda (program)
    (value-of-program program)))

(define value-of-program
  (lambda (pgm)
    (cases program pgm
      (a-program (stmts)
        (value-of-stmts stmts (init-env))))))

(define value-of-stmts
    (lambda (stmts env)
        (cases  statements stmts
            (a-stmt (stmt)
                (value-of-stmt stmt env))
            (a-stmts (stmts stmt)
                (let ((new-env (value-of-stmts stmts env)))
                    (value-of-stmt stmt new-env))))))


(define value-of-stmt
    (lambda (stmt env)
        (cases statement stmt
            (stmt-compound-stmt (stmtt)
                'f)
            (stmt-simple-stmt (stmtt)
                (value-of-simple-stmt stmtt env)))))

(define value-of-simple-stmt 
    (lambda (stmt env)
        (cases simple-stmt stmt
            (simple-stmt-assignment (stmtt)
                (value-of-assignment stmtt env))
            (simple-stmt-return-stmt (stmtt) 'f)
            (simple-stmt-global-stmt (stmtt) 'f)
            (simple-stmt-pass () env)
            (simple-stmt-break () 'f)
            (simple-stmt-continue () 'f))))



(define value-of-assignment 
    (lambda (ass env)
        (cases assignment-stmt ass
            (an-assignment-stmt (id exp) 
                (let ((exp-val (value-of-expression exp env)))
                    (extend-env id exp-val env))))))
(define value-of-expression 
    (lambda (expr env)  
        (cases expression expr
            (an-expression (exp)  
                (value-of-disjunction exp env)))))

(define value-of-disjunction
    (lambda (expr env)
        (cases disjunction-exp expr
            (a-cnj-disj (exp)   
                (value-of-conjunction exp env))
            (or-exp (dis-exp con-exp)
                (or (value-of-disjunction dis-exp env) (value-of-conjunction con-exp env))))))


(define value-of-conjunction
    (lambda (expr env)
        (cases conjunction-exp expr
            (an-inv-conj (exp)   
                (value-of-inversion exp env))
            (and-exp (con-exp inv-exp)
                (and (value-of-conjunction con-exp env) (value-of-inversion exp env))))))


(define value-of-inversion
    (lambda (expr env)
        (cases inversion-exp expr
            (not-exp (exp)   
                (not (value-of-inversion exp env)))
            (a-comparison-inv (exp)
                (value-of-comparison exp env)))))

(define value-of-comparison
    (lambda (expr env)
        (cases comparison-exp expr
            (a-sum-comparison (exp)
                (value-of-sum exp env))
            (a-cmp-op-sum-pairs-comparison (exp1 exp2) ('f)))))


(define value-of-sum 
    (lambda (expr env)
        (cases sum-exp expr
            (a-sum (exp1 exp2)  
                (+ (value-of-sum exp1 env) (value-of-term exp2 env)))
            (a-minus (exp1 exp2)  
                (- (value-of-sum exp1 env) (value-of-term exp2 env)))
            (a-term-sum (exp)
                (value-of-term exp env)))))

(define value-of-term 
    (lambda (expr env)
        (cases term-exp expr
            (a-mul-term (exp1 exp2)
                (* (value-of-term exp1 env) (value-of-factor exp2 env)))
            (a-div-term (exp1 exp2)
                (/ (value-of-term exp1 env) (value-of-factor exp2 env)))
            (a-term (exp)
                (value-of-factor exp env)))))


(define value-of-factor 
    (lambda (expr env)
        (cases factor-exp expr
            (a-plus-factor (exp)
                (value-of-factor exp env))
            (a-minus-factor (exp)
                (- 0 (value-of-factor exp env)))
            (a-power-factor (exp)
                (value-of-power exp env)))))
            

(define value-of-power 
    (lambda (expr env) 
        (cases power-exp expr
            (a-power (atom factor)
                (expt (value-of-atom atom env) (value-of-factor factor env)))
            (a-primary-power (exp)
                (value-of-primary exp env)))))
(define value-of-primary 
    (lambda (expr env)
        (cases primary-exp expr
            (an-atom-primary (exp)
                (value-of-atom exp env))
            (an-array-primary (prim exp) 'f)
            (a-callable-primary (prim) 'f)
            (a-callable-primary-with-args (prim args) 'f))))

(define value-of-atom  
    (lambda (expr env)
        (cases atom-exp expr
            (id-atom (id)
                (apply-env env id))
            (bool-atom (val) val)
            (none-atom () null)
            (num-atom (val) val)
            (list-atom (val) 'f))))



;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
;(define my-lexer (lex-this simple-math-lexer (open-input-string "a = 2; b = a; c = a + b;")))
;(simple-math-parser my-lexer)
;(define parser-res (simple-math-parser my-lexer))
(define evaluate
  (lambda (path)
     (run (simple-math-parser (lex-this simple-math-lexer (open-input-string (file->string (string->path path))))))))
(evaluate "test.txt")