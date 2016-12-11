(load "parse.ss")

(define (type? x) (memq x '(string int double void)))
(define (hasnull? x) (memq '() x))

(define (assign! list v)
	(set-car! list v) v)

(define make-environment
	(lambda () (list 'env '() '())))

(define (environment-local-vars env) (cadr env))
(define (environment-functions env) (caddr env))

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
					((eq? (car next) 'token-right-bracket) (cons (list type name) '()))
					(else (error "bad tokens")))))

(define (declare-function func env)
	(if (null? (look-up-function (car func) env)) 
			(if (null? (environment-functions env)) 
				(set-car! (cddr env) (list func)) 
				(append! (environment-functions env) (list func)))
			(error "function already declared" (car func))))

(define (declare-variable var env)
	(if (eq? (cadr var) 'void) 
		(error "variable cannot be void" var)
		(if (null? (look-up-variable (car var) env)) 
			(if (null? (environment-local-vars env)) 
				(set-car! (cdr env) (list var)) 
				(append! (environment-local-vars env) (list var)))
			(error "variable already declared" (car var)))))

(define (look-up-variable name env)
	(let loop ((var (environment-local-vars env)))
		(if (null? var) 
			'()
			(if (eq? (caar var) name) name (loop (cdr var))))))

(define (look-up-function name env)
	(let loop ((var (environment-functions env)))
		(if (null? var) 
			'()
			(if (eq? (caar var) name) name (loop (cdr var))))))

(define (analyze-declaration r tokens env)
	((tokens 'save))
	(let ((type (analyze-type tokens))
		  (name (analyze-name tokens))
		  (semicolon ((tokens 'has-next) 'token-semicolon)))
		(if (or (null? type) (null? name) (null? semicolon)) 
			(begin ((tokens 'restore)) '())
			(begin
				((tokens 'accept))
				(declare-variable (list name type) env) 
				(assign! r (list 'declaration name type))))))

(define (analyze-variable tokens env)
	(let ((name (analyze-name tokens)))
		(if (null? name) 
			'()
			(look-up-variable name env))))

(define (analyze-addition choices tokens env)
	((tokens 'save))
	(let ((arg1 (analyze-value choices tokens env)))
		(if (null? arg1)
			(begin 
				((tokens 'restore))
				'())
			(let ((plus ((tokens 'has-next) 'token-plus))
		  		  (arg2 (analyze-value default-value-choices tokens env)))

				  (if (or (null? plus) (null? arg2))
						(begin ((tokens 'restore)) '())
						(begin ((tokens 'accept)) (list 'add arg1 arg2)))))))

(define (analyze-multiply choices tokens env)
	((tokens 'save))
	(let ((arg1 (analyze-value choices tokens env)))
		(if (null? arg1)
			(begin 
				((tokens 'restore))
				'())
			(let ((star ((tokens 'has-next) 'token-star))
		  		  (arg2 (analyze-value default-value-choices tokens env)))
		 
				  (if (or (null? star) (null? arg2))
						(begin ((tokens 'restore)) '())
						(begin ((tokens 'accept)) (list 'multiply arg1 arg2)))))))

(define (analyze-brackets choices tokens env)
	((tokens 'save))
	; we try to find at least a partial match before delving into recursive hell
	(let ((lbracket ((tokens 'has-next) 'token-left-bracket)))
		(if (null? lbracket) 
			(begin 
				((tokens 'restore)) 
				'())				;we'll try other variants
			(let ((value (analyze-value default-value-choices tokens env))
		  		  (rbracket ((tokens 'has-next) 'token-right-bracket)))
				
				(if (or (null? value) (null? rbracket))
					(begin ((tokens 'restore)) '())
					(begin ((tokens 'accept)) value))))))

; can be string
(define (analyze-const-value choices tokens env)
	((tokens 'save))
	(let ((value ((tokens 'has-next) 'token-number)))

	(if (null? value)
		(begin 
			((tokens 'restore)) 
			'())
		(begin 
			((tokens 'accept)) 
			(list 'const-value (cadr value))))))

(define (analyze-var-value choices tokens env)
	((tokens 'save))
	(let ((var (analyze-variable tokens env)))

	(if (null? var)
		(begin 
			((tokens 'restore)) 
			'())
		(begin 
			((tokens 'accept)) 
			(list 'var-value var)))))

(define (analyze-func-call choices tokens env)
	((tokens 'save))
	(let ((name (analyze-name tokens)))

	(if (null? name)
		(begin 
			((tokens 'restore))
			'())
		(let ((lbracket ((tokens 'has-next) 'token-left-bracket))
			  (value (analyze-value default-value-choices tokens env)) ;multiple args soon
			  (rbracket ((tokens 'has-next) 'token-right-bracket)))
			(if (or (null? lbracket) (null? value) (null? rbracket))
				(begin
					((tokens 'restore))
					'())
				(begin 
					((tokens 'accept)) 
					(display "returning ") (display name) (newline) 
					(list 'function-call name value)))))))

(define default-value-choices (list analyze-addition 
									analyze-multiply
									analyze-brackets
									analyze-var-value
									analyze-func-call
									analyze-const-value 
									))

(define (analyze-value choices tokens env)
	(if (null? choices) 
		'()
		(let ((value ((car choices) (cdr choices) tokens env)))
			(if (null? value) 
				(analyze-value (cdr choices) tokens env) 
				value))))

(define (analyze-assignment r tokens env)
	((tokens 'save))
	(let ((var (analyze-variable tokens env)))
		(if (null? var) 
			(begin ((tokens 'restore)) '())
			(let ((eqv ((tokens 'has-next) 'token-equals))
		  		  (value (analyze-value default-value-choices tokens env))
		  		  (semicolon ((tokens 'has-next) 'token-semicolon)))
		
					(if (or (null? eqv) (null? value) (null? semicolon))
						(begin 
							((tokens 'restore)) 
							'())
						(begin 
							((tokens 'accept)) 
							(assign! r (list 'assignment var value))))))))

(define (analyze-return r tokens env)
	((tokens 'save))
	(let ((ret (analyze-name tokens)))
		(if (eq? ret 'return)
			(let ((value (analyze-value default-value-choices tokens env))
		  		  (semicolon ((tokens 'has-next) 'token-semicolon)))
		
				(if (or (null? value) (null? semicolon))
					(begin ((tokens 'restore)) '())
					(begin 
						((tokens 'accept)) 
						(assign! r (list 'return value)))))
			(begin ((tokens 'restore)) '()))))

(define (analyze-statement tokens env)
	(let ((r (list '())))
		(cond ((not (null? (analyze-declaration r tokens env))) (car r))
			  ((not (null? (analyze-assignment r tokens env))) (car r))
			  ((not (null? (analyze-return r tokens env))) (car r))
			  ((eq? (car ((tokens 'peek))) 'token-right-curly-bracket) '())
			  (else (error "unknown statement type starting with token" ((tokens 'peek))))
				)))

(define (analyze-statements tokens env)
	(let rec ((statement (analyze-statement tokens env)))
		(display statement) (newline)
		(if (null? statement)
			'() 
			(cons statement (rec (analyze-statement tokens env))))))

(define (analyze-block tokens env)
	(let ((lbracket ((tokens 'has-next) 'token-left-curly-bracket))
		  (statements (analyze-statements tokens env))
		  (rbracket ((tokens 'has-next) 'token-right-curly-bracket)))

		(if (or (null? lbracket) (null? statements) (null? rbracket)) 
			'() 
			statements)))

(define make-function-ast
	(lambda (return_type name args block env) 
		(declare-function (list name return_type args) env)
		(list 'function name return_type args block)))

(define (analyze-function tokens env)
	(let ((rtype (analyze-type tokens))
		  (name (analyze-name tokens))
		  (lbracket ((tokens 'has-next) 'token-left-bracket)))

		  (if (or (null? rtype) (null? name) (null? lbracket))
		  	'()
		  	(let ((args (analyze-args tokens)) ;args can be null and they eat the right bracket
		  		  (block (analyze-block tokens env)))
				(if (null? block) 
					'()
					(make-function-ast rtype name args block env))))))

(define (analyze token-list)
	(let ((env (make-environment))
		  (token-provider (make-token-provider token-list)))
	(let rec ((func (analyze-function token-provider env)))
		(if (null? func) 
			'()
			(cons func (rec (analyze-function token-provider env)))))))