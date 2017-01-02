(define (parse-next-token input)
	(let ((bp '()))

	(define (skip? char)
		(char-whitespace? char))

	(define operator?
		(lambda (char) (memq char '(#\+ #\- #\/ #\% #\( #\) #\{ #\} #\= #\; #\, #\*))))

	(define quote?
		(lambda (char) (memq char '(#\"))))

	(define escape?
		(lambda (char) (eq? char #\\)))

	(define name?
		(lambda (char) (or (char-alphabetic? char) (eq? char #\_))))

	(define parse-next-numerical 
		(lambda (a i) 
			(if (char-numeric? (car i)) 
				(parse-next-numerical (string-append a (string (car i))) (cdr i)) 
				(list i 'token-number (string->number a)))))

	(define parse-next-alphanum
		(lambda (a i) 
			(if (or (char-numeric? (car i)) (name? (car i))) 
				(parse-next-alphanum (string-append a (string (car i))) (cdr i)) 
				(list i 'token-symbol (string->symbol a)))))

	(define parse-next-escape
		(lambda (a i)
			(if (or (eq? (car i) #\\) (eq? (car i) #\")) 
				(parse-next-quoted (string-append a (string (car i))) (cdr i))
				(error "unknown escaped char" (car i)))))

	(define parse-next-quoted 
		(lambda (a i) 
			(cond ((quote? (car i)) 
					(list i 'token-string a))
			      ((escape? (car i)) 
			      	(parse-next-escape (string-append a (string (car i))) (cdr i))) 
				  (else 
				  	(parse-next-quoted (string-append a (string (car i))) (cdr i))))))

	;they can be compound in theory, like operator -> in C
	(define parse-next-operator 
		(lambda (a i )
				(cond 	( (eq? a #\( ) (list i 'token-left-bracket))
				 		( (eq? a #\) ) (list i 'token-right-bracket))
						( (eq? a #\{ ) (list i 'token-left-curly-bracket))
						( (eq? a #\} ) (list i 'token-right-curly-bracket))
				  		( (eq? a #\+ ) (list i 'token-plus))
				  		( (eq? a #\- ) (list i 'token-minus))
				  		( (eq? a #\/ ) (list i 'token-divide))
				  		( (eq? a #\% ) (list i 'token-percent))
				  		( (eq? a #\= ) (list i 'token-equals))
				  		( (eq? a #\; ) (list i 'token-semicolon))
				  		( (eq? a #\, ) (list i 'token-comma))
				  		( (eq? a #\* ) (list i 'token-star))
				  		(else (error "bad operator" a))
				)
			))
	
	(cond 	((null? input) 
				'())
			((skip? (car input)) 
				(parse-next-token (cdr input)))
			((name? (car input)) 
				(parse-next-alphanum (string (car input)) (cdr input)))
			((char-numeric? (car input)) 
				(parse-next-numerical (string (car input)) (cdr input)))
			((quote? (car input))
				(parse-next-quoted "" (cdr input)))
			((operator? (car input)) 
				(parse-next-operator (car input) (cdr input)))
			(else (error "unknown token " (car input))))))

(define tokenize
	(lambda (input)
		(let f ((token (parse-next-token input)))
				(if (null? token) 
					'()
					(cons (cdr token) (f (parse-next-token (car token))))))))

(define (make-stack)
	(let ((stack '())
		  (tmp '()))
		(define (pop!)
			(if (null? stack) 
				(error "stack underflow")
				(begin
					(set! tmp (car stack))
					(set! stack (cdr stack))
					tmp)))
		(define (push! arg)
			(set! stack (append arg stack)))
		(define (dispatch msg)
			(cond 
				((eq? msg 'pop!) pop!)
				((eq? msg 'push!) push!)
				((eq? msg 'list) stack)
				(else (error "unknown call" msg))))
		dispatch))

(define (make-token-provider tokens)
	(let ((hold tokens)
		  (tmp '())
		  (save-stack (make-stack)))
		(define (get-next) 
			(begin (set! tmp (car hold)) (set! hold (cdr hold)) tmp))
		(define (has-next arg)
			(cond 
				((null? hold) '())
				((eq? (caar hold) arg) (get-next))
				(else '())))
		(define (peek) 
			(car hold))
		(define (accept) 
			((save-stack 'pop!)))
		(define (save)
			((save-stack 'push!) (list hold)))
		(define (restore) 
			(set! hold ((save-stack 'pop!))))
		(define (dispatch m) 
			(cond ((eq? m 'get-next) get-next)
				  ((eq? m 'accept) accept)
				  ((eq? m 'save) save)
				  ((eq? m 'restore) restore)
				  ((eq? m 'has-next) has-next)
				  ((eq? m 'peek) peek)
				  (else (error "unknown call" m))))
		dispatch))

(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))