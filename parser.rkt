#lang racket


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
                    ($1)
                )
                (
                    (statements statement s-semicolon)
                    (cons $1 $2)
                )
            )
            (statement
                (
                    (compound-stmt)
                    ($1)
                )
                (
                    (simple-stmt)
                    ($1)
                )
            )
            (simple-stmt
                (
                    (assignment)
                    ($1)
                )
                (
                    (return-stmt)
                    ($1)
                )
                (
                    (global-stmt)
                    ($1)
                )
                (
                    (kw-pass)
                    ('kw-pass)
                )
                (
                    (kw-break)
                    ('kw-pass)
                )
                (
                    (kw-continue)
                    ('kw-continue)
                )
            )
                (compound-stmt
                (
                    (function-def)
                    ($1)
                )
                (
                    (if-stmt)
                    ($1)
                )
                (
                    (for-stmt)
                    ($1)
                )
             )
            (assignment
                (
                    (identifier s-assign expression)
                    (list 'assingment $1 $3)
                )
            )
            (return-stmt
                (
                    (kw-return)
                    ('f)
                )
                (
                    (kw-return expression)
                    ('f)
                )
            )
            (global-stmt
                (
                    (kw-global identifier)
                    ('f)
                )
            )
            (function-def
                (
                    (kw-def identifier s-open-par params s-close-par s-colon statements)
                    ('f)
                )
                (
                    (kw-def identifier s-open-par s-close-par s-colon statements)
                    ('f)
                )
            )
            (params
                (
                    (param-with-default)
                    ('f)
                )
                (
                    (params s-comma param-with-default)
                    ('f)
                )
            )
            (param-with-default
                (
                    (identifier s-assign expression)
                    ('f)
                )
            )
            (if-stmt
                (
                    (kw-if expression s-colon statements else-block)
                    ('f)
                )
            )
            (else-block
                (
                    (kw-else s-colon statements)
                    ('f)
                )
            )
            (for-stmt
                (
                    (kw-for identifier kw-in expression s-colon statements)
                    ('f)
                )
            )
            (expression
                (
                    (disjunction)
                    ($1)
                )
            )
            (disjunction
                (
                    (conjunction)
                    ($1)
                )
                (
                    (disjunction kw-or conjunction)
                    ('f)
                )
            )
            (conjunction
                (
                    (inversion)
                    ($1)
                )
                (
                    (conjunction kw-and inversion)
                    ('f)
                )
            )
            (inversion
                (
                    (kw-not inversion)
                    ('f)
                )
                (
                    (comparison)
                    ($1)
                )
            )
            (comparison
                (
                    (sum compare-op-sum-pairs)
                    ('f)
                )
                (
                    (sum)
                    ($1)
                )
            )
            (compare-op-sum-pairs
                (
                    (compare-op-sum-pair)
                    ('f)
                )
                (
                    (compare-op-sum-pairs compare-op-sum-pair)
                    ('f)
                )
            )
            (compare-op-sum-pair
                (
                    (eq-sum)
                    ('f)
                )
                (
                    (lt-sum)
                    ('f)
                )
                (
                    (gt-sum)
                    ('f)
                )
            )
            (eq-sum
                (
                    (s-eq sum)
                    ('f)
                )
            )
            (lt-sum
                (
                    (s-lt sum)
                    ('f)
                )
            )
            (gt-sum
                (
                    (s-gt sum)
                    ('f)
                )
            )
            (sum
                (
                    (sum s-plus term)
                    (list 'sum $1 $3)
                )
                (
                    (sum s-minus term)
                    (list 'minus $1 $3)
                )
                (
                    (term)
                    ($1)
                )
            )
            (term
                (
                    (term s-mult factor)
                    ('f)
                )
                (
                    (term s-div factor)
                    ('f)
                )
                (
                    (factor)
                    ($1)
                )
            )
            (factor
                (
                    (s-plus factor)
                    ('f)
                )
                (
                    (s-minus factor)
                    ('f)
                )
                (
                    (power)
                    ($1)
                )
            )
            (power
                (
                    (atom s-pow factor)
                    ('f)
                )
                (
                    (primary)
                    ($1)
                )
            )
            (primary
                (
                    (atom)
                    ($1)
                )
                (
                    (primary s-open-brac expression s-close-brac)
                    ('f)
                )
                (
                    (primary s-open-par s-close-par)
                    ('f)
                )
                (
                    (primary s-open-par arguments s-close-par)
                    ('f)
                )
            )
            (arguments
                (
                    (expression)
                    ('f)
                )
                (
                    (arguments s-comma expression)
                    ('f)
                )
            )
            (atom
                (
                    (identifier)
                    (list 'identifier $1)
                )
                (
                    (a-true)
                    ('f)
                )
                (
                    (a-false)
                    ('f)
                )
                (
                    (a-none)
                    ('f)
                )
                (
                    (a-number)
                    (list 'num $1)
                )
                (
                    (List)
                    ('f)
                )
            )
            (List
                (
                    (s-open-brac expressions s-close-brac)
                    ('f)
                )
                (
                    (s-open-brac s-close-brac)
                    ('f)
                )
            )
            (expressions
                (
                    (expressions s-comma expression)
                    ('f)
                )
                (
                    (expression)
                    ('f)
                )
            )
        )
    )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "a = 2")))
(simple-math-parser my-lexer)
;(let ((parser-res (simple-math-parser my-lexer))) parser-res)

