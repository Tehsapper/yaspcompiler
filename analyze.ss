(load "parse.ss")

(define (type? x) (memq x '(string int double)))
(define (hasnull? x) (memq '() x))

(define make-environment
	(lambda () (list 'env '() '())))

(define (analyze-type tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() 
			(if (type? (cadr x)) (cadr x) '() ))))

(define (analyze-name tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() (cadr x))))

(define (analyze-args tokens)
	(let 	((type (analyze-type tokens))
			(name (analyze-name tokens))
			(next ((tokens 'get-next))))
			
			(cond 	((or (null? type) (null? name)) (error "bad tokens at function args" type name))
					((eq? (car next) 'token-comma) (cons (list type name) (analyze-args tokens)))
					((eq? (car next) 'token-right-bracket) (cons (list type name) '())
					(else (error "bad tokens"))))))


(define make-function-ast
	(lambda (return_type name args lbracket block) (list 'function name return_type args block)))

(define (analyze-block tokens env)
	(let ((lbracket ((tokens 'has-next) 'token-left-curly-bracket))
		  (statements (analyze-statements tokens env))
		  (rbracket ((tokens 'has-next) 'token-right-curly-bracket)))))

(define (analyze-function tokens env)
	(let ((rtype (analyze-type tokens))
		  (name (analyze-name tokens))
		  (lbracket ((tokens 'has-next) 'token-left-bracket))
		  (args (analyze-args tokens)))
		  ;(rbracket ((tokens 'has-next) 'token-right-bracket))   )
		  ;(block (analyze-block tokens env)))
		
		(make-function-ast rtype name args lbracket '())))
		;(cond ((memq? ) null)
		;	  ((or (null? lbracket) (null? rbracket)) null)
		;	  (else ast)
		;	)))

(define (analyze token-list)
	(let ((env (make-environment))
		  (token-provider (make-token-provider token-list)))
	(let rec ((func (analyze-function token-provider env)))
		(if (null? func) 
			'()
			(cons func (rec (analyze-function token-provider env)))))))