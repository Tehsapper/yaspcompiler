(define opt-args 
	(lambda (args n default) 
		(if (< n (length args)) (list-ref args n) default)))

(define advance
	(lambda (l)
		(if (null? l) 
			'() 
			(if (= (length l) 1) (set! l '()) (begin 
				(set-car! l (car (cdr l))) 
				(set-cdr! l (cdr (cdr l))) 
				l)))))

(define car-advance
	(lambda (l)
		(let ((old (car l))) (begin (advance l) old))))

(define operator?
	(lambda (char) (memq char '(#\+ #\- #\/ #\% #\( #\) #\{ #\} #\= #\;))))

(define (get-next-token input)
	(let ((top #\s))

	(define (skip? char)
		(char-whitespace? char))

	(define get-next-numerical 
		(lambda (a i) (if (char-numeric? (car i)) (get-next-numerical (string-append a (string (car input))) (advance input)) a)))

	(define get-next-alphanum
		(lambda (a i) (if (or (char-numeric? (car i)) (char-alphabetic? (car i))) (get-next-alphanum (string-append a (string (car input))) (advance input)) a)))

	(define (get-next-quoted . args) args)

	;they can be compound in theory, like operator -> in C
	(define get-next-operator 
		(lambda (a i )
				(cond 	( (eq? a #\( ) 'token-left-bracket)
				 		( (eq? a #\) ) 'token-right-bracket)
						( (eq? a #\{ ) 'token-left-curly-bracket)
						( (eq? a #\} ) 'token-right-curly-bracket)
				  		( (eq? a #\+ ) 'token-plus)
				  		( (eq? a #\- ) 'token-minus)
				  		( (eq? a #\/ ) 'token-div)
				  		( (eq? a #\% ) 'token-mod)
				  		( (eq? a #\= ) 'token-equals)
				  		( (eq? a #\; ) 'token-semicolon)
				  		(else (error "bad operator" a))
				)
			))
	
	(define (save-car l) 
		(begin 
			(set! top (car-advance l)) 
			l))
	
	(cond 	((null? input) '())
			((skip? (car input)) (get-next-token (advance input)))
			((char-alphabetic? (car input)) (list 'token-symbol  (string->symbol (get-next-alphanum (string (car input)) (advance input)))))
			((char-numeric? (car input)) (list 'token-number (string->number (get-next-numerical (string (car input)) (advance input)))))
			((operator? (car input)) (get-next-operator (car input) (advance input)))
			(else (error "unknown token " (car input))))))

(define tokenize
	(lambda (input)
		(let f ((t (get-next-token input)))
				(if (null? t) 
				(begin (display "nil") '())
				(cons t (f (tokenize input)))))))

(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))