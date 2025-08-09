#lang racket
(require data/either) ; import functional lib package

; input file name and parse it
(define (parse fileName)
  (let* ([lines (tokenfile fileName)]
         [tokens (append* lines)]
         [result (program? tokens)])
    (if (success? result)
        "Accept"
        (syntaxError lines (from-failure #f result)))))

; run in order given till succeed or failure
(define (or-then lst on-fail . funcs)
  (cond
    [(null? funcs) on-fail] 
    [else
     (let ([result ((car funcs) lst)]) 
       (if (success? result)
           result
           (apply or-then lst on-fail (cdr funcs))))])) 

; run in order given till succeed or failure
(define (and-then lst . funcs)
  (cond
    [(null? funcs) (success lst)] 
    [else
     (let ([result ((car funcs) lst)]) 
       (if (failure? result)
           result
           (apply and-then (from-success #f result) (cdr funcs))))])) 

; return the and then function for list of functions
(define (and-then-func . funcs)
  (lambda (lst)
    (let loop ((lst lst) (funcs funcs))
      (if (null? funcs)
          (success lst)
          (let ([result ((car funcs) lst)])
            (if (failure? result)
                result
                (loop (from-success #f result) (cdr funcs))))))))

; return parser list and success message
(define (passer lst)
  (success (rest lst)))

; check if element in list represent numeric expression
(define (num? lst)
  (and-then lst numsign? digits?))
; check if its valid expression tail
(define (etail? lst)
  (or-then lst (success lst) 
           (and-then-func (token "+") expr?)
           (and-then-func (token "-") expr?)
           (and-then-func (token "*") expr?)
           (and-then-func (token "/") expr?)))
;check if its valid expression
(define (expr? lst)
  (or-then lst (failure lst)
           (and-then-func id? etail?)
           (and-then-func num? etail?)
           (and-then-func (token "(") expr? (token ")"))))
;check if its valid boolean expression
(define (boolean? lst)
  (or-then lst (failure lst)
           (token "true")
           (token "false")
           (and-then-func expr? bool-op? expr?)))
; check if it is valid statement
(define (stmt? lst)
  (or-then lst (failure lst)
           (and-then-func id? (token "=") expr?)
           (and-then-func (token "if") (token "(") boolean? (token ")") stmt?)
           (and-then-func (token "while") (token "(") boolean?
                          (token ")") linelist? (token "endwhile"))
           (and-then-func (token "read") id?)
           (and-then-func (token "write") expr?)
           (and-then-func (token "goto") id?)
           (and-then-func (token "gosub") id?)
           (token "return")
           (token "break")
           (token "end")))

;checks if list represent valid program tail
(define (linetail? lst)
  (or-then lst (success lst)
           (and-then-func (token ";") stmt? linetail?)))
; check if list represent valid label
(define (label? lst)
  (or-then lst (success lst) 
           (and-then-func id? (token ":"))))
;checks string if valid line of code
(define (line? lst)
  (and-then lst label? stmt? linetail?))

(define (linelist? lst)
  (or-then lst (success lst) 
           (and-then-func line? linelist?)))
;check if list represnsets valid program
(define (program? lst)
  (and-then lst linelist? (token "$$")))




; check if string matches element in list
(define (token s) 
  (lambda (lst)
    (cond
      [(empty? lst) (failure lst)]
      [(equal? (first lst) s) (passer lst)]
      [else (failure lst)])))

;check if element of list contains digits
(define (digits? lst)
  (cond
    [(empty? lst) (failure lst)]
    [(regexp-match-exact? #rx"[0-9]+" (first lst)) (passer lst)]
    [else (failure lst)]))
;check if element of list contains numsigns
(define (numsign? lst)
  (cond
    [(empty? lst) (success lst)]
    [(regexp-match-exact? #rx"[+|-]" (first lst)) (passer lst)]
    [else (success lst)])) 

;check if element contains reserved word in list
(define (id? lst)
  (define reserved '("if" "while" "endwhile" "read" "write" "goto" "gosub" "return" "break" "end"))
  (cond
    [(empty? lst) (failure lst)]
    [(and (not (member (first lst) reserved)) 
          (regexp-match-exact? #rx"[a-zA-Z][a-zA-Z0-9]*" (first lst))) (passer lst)]
    [else (failure lst)]))

;check if element contains operation in list
(define (bool-op? lst)
  (cond
    [(empty? lst) (failure lst)]
    [(member (first lst) '("<" ">" "<=" ">=" "=" "<>")) (passer lst)]
    [else (failure lst)]))


; take string into token
(define (tokenize s)
  (flatten (map (λ (str) (regexp-split (pregexp "(?<!^)(\\b|(?=[\\(\\)])|(?<=[\\(\\)]))(?!$)") str))
                (string-split s))))

;tokenize file names
(define (tokenfile filename)
  (for/list ([line (file->lines filename)])
    (tokenize line)))

(define (matchLine orig-lst flat-lst)
  (define (loop flat-len lst)
    (let ([nextLine (length (car lst))]) ;; Get the length of the first list in `lst`
      (if (<= flat-len nextLine) ;; If the length of the first list is less than or equal to `flat-len`
          (length lst) ;; Return the length of `lst`
          (loop (- flat-len nextLine) (cdr lst))))) ;; Subtract the length of the first list from `flat-len`, and continue with the rest of the lists in `lst`

  (loop (length flat-lst) (reverse orig-lst))) ;; Reverse `orig-lst` and call `loop` with the length of `flat-lst` and the reversed `orig-lst`


; Syntax errors 
(define (syntaxError all-lines remaining-tokens)
  (define (format-error-message line-num token)
    (format "Syntax error found on line ~a: '~a'"
            line-num
            token))

  (let ([line-num (matchLine all-lines remaining-tokens)]
        [token (car remaining-tokens)])
    (format-error-message line-num token)))



; parse files
(parse "File01.txt") 
(parse "File02.txt") 
(parse "File03.txt") 
(parse "File04.txt") 
(parse "File05.txt") 
