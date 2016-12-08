(define (type? x) (memq x '(string int double)))
(define (hasnull? x) (memq null x))

(define (analyze-type input)
	(let ((x (get-next-token input)))
		(if (type? x) x null)))

(define (analyze-arg input)
	(let ((type (get-next-token input))
		  (name (get-name input)))
		(if (and (type? type) (not (null? name))) '(type name) null)))

(define (analyze-args input)
	(let ((arg (analyze-arg input)))
		(if (null? arg) 
			null 
			(cons arg 
				(cond ((eq () 'token-comma))

(define (analyze-function input)
	(let ((rtype (analyze-type input))
		  (name (get-name input))
		  (lbracket (get-next-token input))
		  (args (analyze-args input))
		  (rbracket (get-next-token input))
		  (block (analyze-block input))
		  (ast (list 'function rtype name args block)))
		(cond ((hasnull? ast) null)
			  ((or (null? lbracket) (null? rbracket)) null)
			  (else ast)
			)))

(define (analyze input)
	(lambda (input) 
		(cons (analyze-function input) null)))