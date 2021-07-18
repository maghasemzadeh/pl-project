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
            ((:: (:or (char-range #\a #\z) (char-range #\A #\Z) #\_) (:* (:or (char-range #\a #\z) (char-range #\A #\Z) #\_ (char-range #\0 #\9)))) (token-identifier lexeme))
            (whitespace (simple-math-lexer input-port))
            ((eof) (token-EOF))))

(define-tokens a (identifier a-number))
(define-empty-tokens b (
    EOF s-semicolon kw-pass kw-break kw-continue s-assign kw-return kw-global kw-def s-open-par s-close-par 
    s-colon s-comma kw-if kw-else kw-for kw-in kw-or kw-and kw-not s-eq s-lt s-gt s-lte s-gte s-plus s-minus s-mult s-div 
    s-pow s-plus-assign s-minus-assign s-mult-assign s-div-assign s-pow-assign s-open-brac s-close-brac a-true a-false a-none
    ))

(define-datatype parser-exp parser-exp?
  (kw-pass-exp)
  (kw-break-exp)
  (kw-continue-exp)
  (assingment-exp (id identifier?)
                  (exp parser-exp?))
  (kw-return-exp)
  (return-exp)
  (global-exp (id identifier?))
  (def-exp (id identifier?)
    (exp1 parser-exp?)
    (exp2 parser-exp?))
  (empty-def-exp (id identifier?)
                 (exp1 parser-exp?)
                 (exp2 parser-exp?))
  (param-comma-exp (exp1 parser-exp?)
                   (exp2 parser-exp?))
  (assign-exp (id identifier?)
              (exp1 parser-exp?))
  (if-exp (exp1 parser-exp?)
          (exp2 parser-exp?))
  (else-exp (exp1 parser-exp?))
  (for-exp (id identifier?)
           (exp1 parser-exp?)
           (exp2 parser-exp?))
  (or-exp (exp1 parser-exp?)
          (exp2 parser-exp?))
  (and-exp (exp1 parser-exp?)
           (exp2 parser-exp?))
  (not-exp (exp1 parser-exp?))
  (sum-compare-exp (exp1 parser-exp?)
                   (exp2 parser-exp?))
  (multi-compare-exp (exp1 parser-exp?))
  (eq-exp (exp1 parser-exp?))
  (lt-exp (exp1 parser-exp?))
  (gt-exp (exp1 parser-exp?))
  (sum-exp (num1 number?)
           (num2 number?))
  (minus-exp (num1 number?)
             (num2 number?))
  (mul-exp (num1 number?)
           (num2 number?))
  (div-exp (num1 number?)
           (num2 number?))
  (pos-num-exp (num number?))
  (pow-exp (num1 number?)
           (num2 number?))
  (array-peak-exp (exp1 parser-exp?)
                  (exp2 parser-exp?))
  (call-exp (exp1 parser-exp?))
  (call-arg-exp (exp1 parser-exp?)
                (exp2 parser-exp?))
  (arg-exp-exp (exp1 parser-exp?)
               (exp2 parser-exp?))
  (id-exp (id identifier?))
  (bool-exp (bool boolean?))
  (none-exp)
  (num-exp (num number?))
  (bracket-exp (exp1 parser-exp?))
  (empty-bracket-exp)
  (comma-exp (exp1 parser-exp?)
             (exp2 parser-exp?))
  (neg-num-exp (exp1 parser-exp?))
  )

(define simple-math-parser
    (parser
        (start statements)
        (end EOF)
        (error void)
        (tokens a b)
        (grammar
            (statements
                (
                    (statement s-semicolon)
                    $1
                )
                (
                    (statements statement s-semicolon)
                    (cons $1 $2)
                )
            )
            (statement
                (
                    (compound-stmt)
                    $1
                )
                (
                    (simple-stmt)
                    $1
                )
            )
            (simple-stmt
                (
                    (assignment)
                    $1
                )
                (
                    (return-stmt)
                    $1
                )
                (
                    (global-stmt)
                    $1
                )
                (
                    (kw-pass)
                    (kw-pass-exp)
                )
                (
                    (kw-break)
                    (kw-break-exp)
                )
                (
                    (kw-continue)
                    (kw-continue-exp)
                )
            )
                (compound-stmt
                (
                    (function-def)
                    $1
                )
                (
                    (if-stmt)
                    $1
                )
                (
                    (for-stmt)
                    $1
                )
             )
            (assignment
                (
                    (identifier s-assign expression)
                    (assingment-exp $1 $3)
                )
            )
            (return-stmt
                (
                    (kw-return)
                    (kw-return-exp)
                )
                (
                    (kw-return expression)
                    (return-exp)
                )
            )
            (global-stmt
                (
                    (kw-global identifier)
                    (global-exp $2)
                )
            )
            (function-def
                (
                    (kw-def identifier s-open-par params s-close-par s-colon statements)
                    (def-exp $2 $4 $7)
                )
                (
                    (kw-def identifier s-open-par s-close-par s-colon statements)
                    (empty-def-exp $2 $6)
                )
            )
            (params
                (
                    (param-with-default)
                    $1
                )
                (
                    (params s-comma param-with-default)
                    (param-comma-exp $1 $3)
                )
            )
            (param-with-default
                (
                    (identifier s-assign expression)
                    (assign-exp $1 $3)
                )
            )
            (if-stmt
                (
                    (kw-if expression s-colon statements else-block)
                    (if-exp $2 $4)
                )
            )
            (else-block
                (
                    (kw-else s-colon statements)
                    (else-exp $3)
                )
            )
            (for-stmt
                (
                    (kw-for identifier kw-in expression s-colon statements)
                    (for-exp $2 $4 $6)
                )
            )
            (expression
                (
                    (disjunction)
                    $1
                )
            )
            (disjunction
                (
                    (conjunction)
                    $1
                )
                (
                    (disjunction kw-or conjunction)
                    (or-exp $1 $3)
                )
            )
            (conjunction
                (
                    (inversion)
                    $1
                )
                (
                    (conjunction kw-and inversion)
                    (and-exp $1 $3)
                )
            )
            (inversion
                (
                    (kw-not inversion)
                    (not-exp $2)
                )
                (
                    (comparison)
                    $1
                )
            )
            (comparison
                (
                    (sum compare-op-sum-pairs)
                    (sum-compare-exp $1 $2)
                )
                (
                    (sum)
                    $1
                )
            )
            (compare-op-sum-pairs
                (
                    (compare-op-sum-pair)
                    $1
                )
                (
                    (compare-op-sum-pairs compare-op-sum-pair)
                    (multi-compare-exp $2)
                )
            )
            (compare-op-sum-pair
                (
                    (eq-sum)
                    $1
                )
                (
                    (lt-sum)
                    $1
                )
                (
                    (gt-sum)
                    $1
                )
            )
            (eq-sum
                (
                    (s-eq sum)
                    (eq-exp $2)
                )
            )
            (lt-sum
                (
                    (s-lt sum)
                    (lt-exp $2)
                )
            )
            (gt-sum
                (
                    (s-gt sum)
                    (gt-exp $2)
                )
            )
            (sum
                (
                    (sum s-plus term)
                    (sum-exp $1 $3)
                )
                (
                    (sum s-minus term)
                    (minus-exp $1 $3)
                )
                (
                    (term)
                    $1
                )
            )
            (term
                (
                    (term s-mult factor)
                    (mul-exp $1 $3)
                )
                (
                    (term s-div factor)
                    (div-exp $1 $3)
                )
                (
                    (factor)
                    $1
                )
            )
            (factor
                (
                    (s-plus factor)
                    (pos-num-exp $2)
                )
                (
                    (s-minus factor)
                    (neg-num-exp $2)
                )
                (
                    (power)
                    $1
                )
            )
            (power
                (
                    (atom s-pow factor)
                    (pow-exp $1 $3)
                )
                (
                    (primary)
                    $1
                )
            )
            (primary
                (
                    (atom)
                    $1
                )
                (
                    (primary s-open-brac expression s-close-brac)
                    (array-peak-exp $1 $3)
                )
                (
                    (primary s-open-par s-close-par)
                    (call-exp $1)
                )
                (
                    (primary s-open-par arguments s-close-par)
                    (call-arg-exp $1 $3)
                )
            )
            (arguments
                (
                    (expression)
                    $1
                )
                (
                    (arguments s-comma expression)
                    (arg-exp-exp $1 $3)
                )
            )
            (atom
                (
                    (identifier)
                    (id-exp $1)
                )
                (
                    (a-true)
                    (bool-exp $1)
                )
                (
                    (a-false)
                    (bool-exp $1)
                )
                (
                    (a-none)
                    (none-exp)
                )
                (
                    (a-number)
                    (num-exp $1)
                )
                (
                    (List)
                    $1
                )
            )
            (List
                (
                    (s-open-brac expressions s-close-brac)
                    (bracket-exp $2)
                )
                (
                    (s-open-brac s-close-brac)
                    (empty-bracket-exp)
                )
            )
            (expressions
                (
                    (expressions s-comma expression)
                    (comma-exp $1 $3)
                )
                (
                    (expression)
                    $1
                )
            )
        )
    )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a = 2;")))
(simple-math-parser my-lexer)
;(let ((parser-res (simple-math-parser my-lexer))) parser-res)

