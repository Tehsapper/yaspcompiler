(define opt-args 
	(lambda (args n default) 
		(if (< n (length args)) (list-ref args n) default)))

(define advance
	(lambda (l)
		(if (null? l) 
			'() 
			(begin 
				(set-car! l (car (cdr l))) 
				(set-cdr! l (cdr (cdr l))) 
				l))))

(define car-advance
	(lambda (l)
		(let ((old (car l))) (begin (advance l) old))))

(define (get-next-token input)
	(let (;(input (opt-args args 0 #f))
		  ;(acc (opt-args args 1 ""))
		  (top #\s))

	(define (skip? char) 
	(or (eq? (car char) #\space) 
		(null? (car char)) 
		(eq? (car char) #\tab) 
		(eq? (car char) #\newline)))

	(define get-next-numerical 
		(lambda (i a))

	(define (get-next-alphanum . args) )

	(define (get-next-quoted . args) args)

	(define (get-next-operator . args) args)
	
	(define (save-car l) 
		(begin 
			(set! top (car-advance l)) 
			l))
	
	(cond 	((null? input) acc)
			((skip? input) (get-next-token (advance input)));(if (string-null? acc) (get-next-token (advance input)) acc))
			((char-alphabetic? (car input)) (get-next-alphanum (string (car input)) (advance input)));(string-append acc (string top))))
			((char-numeric? (car input)) (get-next-numerical (string (car input)) (advance input)))
			(else (if (string-null? acc) (string (car-advance input)) acc)))))


(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))