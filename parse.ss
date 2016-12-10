(define (parse-next-token input)
	(let ((bp '()))

	(define (skip? char)
		(char-whitespace? char))

	(define operator?
		(lambda (char) (memq char '(#\+ #\- #\/ #\% #\( #\) #\{ #\} #\= #\; #\,))))

	(define parse-next-numerical 
		(lambda (a i) 
			(if (char-numeric? (car i)) 
				(parse-next-numerical (string-append a (string (car i))) (cdr i)) 
				(list i 'token-number (string->number a)))))

	(define parse-next-alphanum
		(lambda (a i) 
			(if (or (char-numeric? (car i)) (char-alphabetic? (car i))) 
				(parse-next-alphanum (string-append a (string (car i))) (cdr i)) 
				(list i 'token-symbol (string->symbol a)))))

	(define (parse-next-quoted . args) args)

	;they can be compound in theory, like operator -> in C
	(define parse-next-operator 
		(lambda (a i )
				(cond 	( (eq? a #\( ) (list i 'token-left-bracket))
				 		( (eq? a #\) ) (list i 'token-right-bracket))
						( (eq? a #\{ ) (list i 'token-left-curly-bracket))
						( (eq? a #\} ) (list i 'token-right-curly-bracket))
				  		( (eq? a #\+ ) (list i 'token-plus))
				  		( (eq? a #\- ) (list i 'token-minus))
				  		( (eq? a #\/ ) (list i 'token-div))
				  		( (eq? a #\% ) (list i 'token-mod))
				  		( (eq? a #\= ) (list i 'token-equals))
				  		( (eq? a #\; ) (list i 'token-semicolon))
				  		( (eq? a #\, ) (list i 'token-comma))
				  		(else (error "bad operator" a))
				)
			))
	
	(cond 	((null? input) 
				'())
			((skip? (car input)) 
				(parse-next-token (cdr input)))
			((char-alphabetic? (car input)) 
				(parse-next-alphanum (string (car input)) (cdr input)))
			((char-numeric? (car input)) 
				(parse-next-numerical (string (car input)) (cdr input)))
			((operator? (car input)) 
				(parse-next-operator (car input) (cdr input)))
			(else (error "unknown token " (car input))))))

(define tokenize
	(lambda (input)
		(let f ((token (parse-next-token input)))
				(if (null? token) 
					'()
					(cons (cdr token) (f (parse-next-token (car token))))))))

(define t (make-undefined-variable))

(define (make-token-provider tokens)
	(let ((hold tokens)
		  (tmp '()))
		(define (get-next) 
			(begin (set! tmp (car hold)) (set! hold (cdr hold)) tmp))
		(define (has-next arg)
			(if (eq? (caar hold) arg) (get-next) '()))
		(define (peek) 
			(car hold))
		(define (accept) 
			(set! tokens hold))
		(define (decline) 
			(set! hold tokens))
		(define (dispatch m) 
			(cond ((eq? m 'get-next) get-next)
				  ((eq? m 'accept) accept)
				  ((eq? m 'decline) decline)
				  ((eq? m 'has-next) has-next)
				  ((eq? m 'peek) peek)
				  (else (error "unknown call" m))))
		dispatch))

(define (get-next-token tokens)
	(if (or (variable-bound? t) (null? (variable-ref t)))
		(begin (variable-set! t (cdr (variable-ref t))) (car (variable-ref t)))
		(begin (variable-set! t tokens) (car (variable-ref t)))))

(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))