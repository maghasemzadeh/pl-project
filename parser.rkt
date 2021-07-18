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
    s-colon s-comma kw-if kw-else kw-for kw-in kw-or kw-and kw-not s-eq s-lt s-gt s-plus s-minus s-mult s-div 
    s-pow s-open-brac s-close-brac a-true a-false a-none
    ))

(define simple-math-parser
    (parser
        (start program)
        (end EOF)
        (error void)
        (tokens a b)
        (grammar
            (program 
                (
                    (statements EOF)
                    ()
                )
            )
            (statements
                (
                    (statement s-semicolon)
                    ()
                )
                (
                    (statements statement s-semicolon)
                    ()
                )
            )
            (statement
                (
                    (compound-stmt)
                    ()
                )
                (
                    (simple-stmt)
                    ()
                )
            )
            (simple-stmt
                (
                    (assignment)
                    ()
                )
                (
                    (return-stmt)
                    ()
                )
                (
                    (global-stmt)
                    ()
                )
                (
                    (kw-pass)
                    ()
                )
                (
                    (kw-break)
                    ()
                )
                (
                    (kw-continue)
                    ()
                )
            )
            (compound-stmt
                (
                    (functiond-def)
                    ()
                )
                (
                    (if-stmt)
                    ()
                )
                (
                    (for-stmt)
                    ()
                )
            )
            (assignment
                (
                    (identifier s-assign expression)
                    ()
                )
            )
            (return-stmt
                (
                    (kw-return)
                    ()
                )
                (
                    (kw-return expression)
                    ()
                )
            )
            (global-stmt
                (
                    (kw-global identifier)
                    ()
                )
            )
            (functiond-def
                (
                    (kw-def identifier s-open-par params s-close-par s-colon statements)
                    ()
                )
                (
                    (kw-def identifier s-open-par s-close-par s-colon statements)
                    ()
                )
            )
            (params
                (
                    (param-with-default)
                    ()
                )
                (
                    (params s-comma param-with-default)
                    ()
                )
            )
            (param-with-default
                (
                    (identifier s-assign expression)
                    ()
                )
            )
            (if-stmt
                (
                    (kw-if expression s-colon statements else-block)
                    ()
                )
            )
            (else-block
                (
                    (kw-else s-colon statements)
                    ()
                )
            )
            (for-stmt
                (
                    (kw-for identifier kw-in expression s-colon statements)
                    ()
                )
            )
            (expression
                (
                    (disjunction)
                    ()
                )
            )
            (disjunction
                (
                    (conjunction)
                    ()
                )
                (
                    (disjunction kw-or conjunction)
                    ()
                )
            )
            (conjunction
                (
                    (inversion)
                    ()
                )
                (
                    (conjunction kw-and inversion)
                    ()
                )
            )
            (inversion
                (
                    (kw-not inversion)
                    ()
                )
                (
                    (comparison)
                    ()
                )
            )
            (comparison
                (
                    (sum compare-op-sum-pairs)
                    ()
                )
                (
                    (sum)
                    ()
                )
            )
            (compare-op-sum-pairs
                (
                    (compare-op-sum-pair)
                    ()
                )
                (
                    (compare-op-sum-pairs compare-op-sum-pair)
                    ()
                )
            )
            (compare-op-sum-pair
                (
                    (eq-sum)
                    ()
                )
                (
                    (lt-sum)
                    ()
                )
                (
                    (gt-sum)
                    ()
                )
            )
            (eq-sum
                (
                    (s-eq sum)
                    ()
                )
            )
            (lt-sum
                (
                    (s-lt sum)
                    ()
                )
            )
            (gt-sum
                (
                    (s-gt sum)
                    ()
                )
            )
            (sum
                (
                    (sum s-plus term)
                    ()
                )
                (
                    (sum s-minus term)
                    ()
                )
                (
                    (term)
                    ()
                )
            )
            (term
                (
                    (term s-mult factor)
                    ()
                )
                (
                    (term s-div factor)
                    ()
                )
                (
                    (factor)
                    ()
                )
            )
            (factor
                (
                    (s-plus factor)
                    ()
                )
                (
                    (s-minus factor)
                    ()
                )
                (
                    (power)
                    ()
                )
            )
            (power
                (
                    (atom s-pow factor)
                    ()
                )
                (
                    (primary)
                    ()
                )
            )
            (primary
                (
                    (atom)
                    ()
                )
                (
                    (primary s-open-brac expression s-close-brac)
                    ()
                )
                (
                    (primary s-open-par s-close-par)
                    ()
                )
                (
                    (primary s-open-par arguments s-close-par)
                    ()
                )
            )
            (arguments
                (
                    (expression)
                    ()
                )
                (
                    (arguments s-comma expression)
                    ()
                )
            )
            (atom
                (
                    (identifier)
                    ()
                )
                (
                    (a-true)
                    ()
                )
                (
                    (a-false)
                    ()
                )
                (
                    (a-none)
                    ()
                )
                (
                    (a-number)
                    ()
                )
                (
                    (List)
                    ()
                )
            )
            (List
                (
                    (s-open-brac expressions s-close-brac)
                    ()
                )
                (
                    (s-open-brac s-close-brac)
                    ()
                )
            )
            (expressions
                (
                    (expressions s-comma expression)
                    ()
                )
                (
                    (expression)
                    ()
                )
            )
        )
    )
)

;test
(define lex-this (lambda (lexer input) (lambda () (lexer input))))
(define my-lexer (lex-this simple-math-lexer (open-input-string "1+2+ 3 +   4")))
(let ((parser-res (simple-math-parser my-lexer))) parser-res)

