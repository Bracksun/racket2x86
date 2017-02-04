#lang racket

(require parser-tools/lex)
(require (prefix-in : parser-tools/lex-sre))


;; FIXME
(define output-endmarker? #t)
(define (for-all pred? list) (error "implement me!"))

(define (unget port)
  (file-position port (- (file-position port) 1)))

(define-lex-abbrev NEWLINE #\newline)
(define-lex-abbrev hash-comment (:: "#" (complement (:: any-string NEWLINE any-string)) NEWLINE))
(define-lex-abbrev nonzerodigit (:: (char-range "1" "9")))
(define-lex-abbrev digit numeric)
(define-lex-abbrev octdigit (:: (char-range "0" "7")))
(define-lex-abbrev hexdigit (:or digit (char-range "a" "f") (char-range "A" "F")))
(define-lex-abbrev bindigit (:or "0" "1"))
(define-lex-abbrev octinteger (:: "0" (:or "o" "O") (:+ octdigit)))
(define-lex-abbrev hexinteger (:: "0" (:or "x" "X") (:+ hexdigit)))
(define-lex-abbrev bininteger (:: "0" (:or "b" "B") (:+ bindigit)))
(define-lex-abbrev decimalinteger (:: nonzerodigit (:* digit)))
(define-lex-abbrev intpart (:+ digit))
(define-lex-abbrev fraction (:: "." intpart))
(define-lex-abbrev pointfloat (:or (:: (:? intpart) fraction) (:: intpart ".")))
(define-lex-abbrev exponent (:: (:or "e" "E") (:? "-") intpart)) 
(define-lex-abbrev exponentfloat (:: (:or intpart pointfloat) exponent))
(define-lex-abbrev floatnumber (:or pointfloat exponentfloat))
(define-lex-abbrev imagnumber (:: (:or floatnumber intpart) (:or "j" "J")))
;; FIXME: identifier need more consideration
(define-lex-abbrev identifier (:: id_start (:* id_continue)))
(define-lex-abbrev id_start (:or lower-case upper-case title-case "_"))
(define-lex-abbrev id_continue (:or id_start numeric))

(define-lex-abbrev string-quote (:: (:? (:or "r" "R" "f" "F")) #\" (complement (:: any-string #\" any-string)) #\"))
(define-lex-abbrev keyword (:or "False"      "class"      "finally"    "is"         "return"
                                "None"       "continue"   "for"        "lambda"     "try"
                                "True"       "def"        "from"       "nonlocal"   "while"
                                "and"        "del"        "global"     "not"        "with"
                                "as"         "elif"       "if"         "or"         "yield"
                                "assert"     "else"       "import"     "pass"
                                "break"      "except"     "in"         "raise"))
(define-lex-abbrev operator (:or "+"       "-"       "*"       "**"      "/"       "//"      "%"      "@"
                                 "<<"      ">>"      "&"       "|"       "^"       "~"
                                 "<"       ">"       "<="      ">="      "=="      "!="))
(define-lex-abbrev delimiter (:or "("       ")"       "["       "]"       "{"       "}"
                                  ","       ":"       "."       ";"       "@"       "="       "->"
                                  "+="      "-="      "*="      "/="      "//="     "%="      "@="
                                  "&="      "|="      "^="      ">>="     "<<="     "**="))

(define current-spaces 0)
(define (reset-spaces!) (set! current-spaces 0))
(define (inc-spaces!) (set! current-spaces (+ current-spaces 1)))
(define (inc-tab!) (set! current-spaces (+ current-spaces 4)))

(define indent-stack '(0))
(define (current-indent) (car indent-stack))
(define (push-indent! spaces) (set! indent-stack (cons spaces indent-stack)))
(define (pop-indent!) 
  (if (null? indent-stack)
    (error "indent-stack is empty")
    (set! indent-stack (cdr indent-stack))))
(define (measure-spaces!) (error "implement me!"))
(define (pop-indents!) (set! indent-stack '()))

(define paren-stack '())
(define (push-paren! char) (set! paren-stack (cons char paren-stack)))
(define (pop-paren! char) (error "implement me!"))

;(define (whitespace-ignored?) (error "implement me!"))
;(define (octal-digit? char) (error "implement me!"))
;(define (hex-digit? char) (error "implement me!"))
;(define unicode-name=>integer (error "implement me!"))
;(define (char-for-unicode-name name) (error "implement me!"))
;(define (char-for string) (error "implement me!"))
;(define (unescape-string string #:is-byte (is-byte #f)) (error "implement me!"))
;(define (lex-raw-string end-quote port rev-chars) (error "implement me!"))
;(define other-id-start-chars (error "implement me!"))
;(define other-id-continue-chars (error "implement me!"))
;(define (other-id-start? char) (error "implement me!"))
;(define (other-id-continue? char) (error "implement me!"))
;(define (id-start? char) (error "implement me!"))
;(define (id-continue? char) (error "implement me!"))
;(define (xid-start? char) (error "implement me!"))
;(define (xid-continue? char) (error "implement me!"))
;(define (id-lexer port rev-chars) (error "implement me!"))
;(define test (error "implement me!"))
;(define test-input (error "implement me!"))
;(define input (error "implement me!"))
;(define (port->list port) (error "implement me!"))
;(define (port->string port) (error "implement me!"))

;(match (current-command-line-arguments) 
;       [(vector "-n") (set! output-endmarker? #f) (set! input (current-input-port))] 
;       [(vector (or "--test" "--drracket")) (set! input test-input)] 
;       [(vector file-name) (set! input (open-input-file file-name))] 
;       [(vector) (set! input (current-input-port))])

;(set! input (open-input-string (port->string input)))
(define input (open-input-file "lexer.input"))

(define pylex
  (lexer
    ; end of file
    [(eof) '((ENDMARKER))]
    ; comment 
    [hash-comment (indent-lexer input-port)]
    ; newline
    [NEWLINE (indent-lexer input-port)]
    ; space
    [#\space (pylex input-port)]
    ; explicit lne joining \/n
    [(:: #\\ #\newline) (pylex input-port)]
    ; keyword
    [keyword (cons `(KEYWORD ,(string->symbol lexeme)) (pylex input-port))]
    ; operator
    [operator (cons `(PUNCT ,lexeme) (pylex input-port))]
    ; delimiter
    [delimiter (cons `(PUNCT ,lexeme) (pylex input-port))]
    ; identifier
    [identifier (cons `(ID ,lexeme) (pylex input-port))]
    ; LIT
    ;[string-quote (cons `(LIT ,lexeme) (pylex input-port))]
    [imagnumber (cons `(LIT ,(string->symbol (string-append "+" lexeme))) (pylex input-port))]
    [(:or octinteger hexinteger bininteger decimalinteger floatnumber)
     (cons `(LIT ,(string->number lexeme)) (pylex input-port))]
    ;; To be completed
    [any-char (pylex input-port)]
    ))


; a lexer for measuring indentation:
(define indent-lexer 
  (lexer
    ; end:
    [(eof) '((ENDMARKER))]
    ; skip newline:
    [NEWLINE (indent-lexer input-port)]
    ; space
    [#\space (begin
               (inc-spaces!)
               (indent-lexer input-port))]
    ; tab
    [#\tab (begin
             (inc-tab!)
             (indent-lexer input-port))]

    [any-char (begin
                (unget input-port)
                (cond
                  [(eq? (current-indent) current-spaces) (reset-spaces!) (pylex input-port)]
                  [(< (current-indent) current-spaces)
                   (push-indent! current-spaces) (reset-spaces!) (cons '(INDENT) (pylex input-port))]
                  [else (pop-indent!) (reset-spaces!) (cons '(DEDENT) (pylex input-port))]))]
    ))

    

;; driver
(define tokens (indent-lexer input))
(for ((token tokens)) (write token) (newline))
