(load "parse.ss")

(define (type? x) (memq x '(string int double void)))
(define (hasnull? x) (memq '() x))

(define (assign! list v)
	(set-car! list v) v)

(define make-environment
	(lambda () (list 'env (make-stack) '())))

;it's a stack of contexts, which hold variable lists to be pushed or popped off upon ctx switch
(define (environment-ctx-vars env) (cadr env)) 
(define (environment-local-vars env) (car ((environment-ctx-vars env) 'list)))
(define (environment-functions env) (caddr env))

(define (fail t) ((t 'restore)) '())
(define (success t p) ((t 'accept)) (p))

(define (analyze-type tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() 
			(if (type? (cadr x)) (cadr x) '() ))))

(define (analyze-name tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() (cadr x))))

(define (create-context env) (((environment-ctx-vars env) 'push!) (list '())))

(define (delete-context env) (((environment-ctx-vars env) 'pop!)))

(define (declare-variable var env)
	(if (eq? (cadr var) 'void) 
		(error "variable cannot be void" var)
		(if (null? (look-up-variable (car var) env)) 
			(if (null? (environment-local-vars env)) 
				(set-car! ((environment-ctx-vars env) 'list) (list var)) 
				(append! (environment-local-vars env) (list var)))
			(error "variable already declared" (car var)))))

(define (look-up-variable name env)
	(define (local-loop var)
		(if (null? var) 
			'()
			(if (eq? (caar var) name) name (local-loop (cdr var)))))
	(define (loop ctx)
		(if (null? ctx) 
			'()
			(let ((var (local-loop (car ctx))))
				(if (null? var)
					(loop (cdr ctx))
					var))))
	(loop ((environment-ctx-vars env) 'list)))

(define (declare-function func env)
	(if (null? (look-up-function (car func) env)) 
			(if (null? (environment-functions env)) 
				(set-car! (cddr env) (list func)) 
				(append! (environment-functions env) (list func)))
			(error "function already declared" (car func))))

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
			(fail tokens)
			(begin
				((tokens 'accept))
				(declare-variable (list name type) env) 
				(assign! r (list 'declaration name type))))))

(define (analyze-variable tokens env)
	(let ((name (analyze-name tokens)))
		(if (null? name) 
			'()
			(look-up-variable name env))))

(define (analyze-binop choices tokens env operator tag)
	((tokens 'save))
	(let ((arg1 (analyze-value choices tokens env)))
		(if (null? arg1)
			(fail tokens)
			(let ((op ((tokens 'has-next) operator))
		  		  (arg2 (analyze-value default-value-choices tokens env)))
		 
				  (if (or (null? op) (null? arg2))
						(fail tokens)
						(begin ((tokens 'accept)) (list tag arg1 arg2)))))))

(define (analyze-addition choices tokens env)
	(analyze-binop choices tokens env 'token-plus 'add))

(define (analyze-multiply choices tokens env)
	(analyze-binop choices tokens env 'token-star 'multiply))

(define (analyze-substraction choices tokens env)
	(analyze-binop choices tokens env 'token-minus 'substract))

(define (analyze-division choices tokens env)
	(analyze-binop choices tokens env 'token-divide 'divide))

(define (analyze-modulo choices tokens env)
	(analyze-binop choices tokens env 'token-percent 'modulo))


(define (analyze-brackets choices tokens env)
	((tokens 'save))
	; we try to find at least a partial match before delving into recursive hell
	(let ((lbracket ((tokens 'has-next) 'token-left-bracket)))
		(if (null? lbracket) 
			(fail tokens)			;we'll try other variants
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
		(fail tokens)
		(begin 
			((tokens 'accept)) 
			(list 'const-value (cadr value))))))

(define (analyze-var-value choices tokens env)
	((tokens 'save))
	(let ((var (analyze-variable tokens env)))

	(if (null? var)
		(fail tokens)
		(begin 
			((tokens 'accept)) 
			(list 'var-value var)))))

(define (analyze-func-call choices tokens env)
	((tokens 'save))
	(let ((name (analyze-name tokens)))

	(if (null? name)
		(fail tokens)
		(let ((lbracket ((tokens 'has-next) 'token-left-bracket))
			  (value (analyze-value default-value-choices tokens env)) ;multiple args soon
			  (rbracket ((tokens 'has-next) 'token-right-bracket)))
			(if (or (null? lbracket) (null? value) (null? rbracket))
				(fail tokens)
				(begin 
					((tokens 'accept)) 
					(list 'function-call name value)))))))

(define default-value-choices (list analyze-addition
									analyze-substraction 
									analyze-multiply
									;analyze-division
									;analyze-modulo
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
			(fail tokens)
			(let ((eqv ((tokens 'has-next) 'token-equals))
		  		  (value (analyze-value default-value-choices tokens env))
		  		  (semicolon ((tokens 'has-next) 'token-semicolon)))
		
					(if (or (null? eqv) (null? value) (null? semicolon))
						(fail tokens)
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
					(fail tokens)
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
	(create-context env)	; various blocks have their own scope
	(let ((lbracket ((tokens 'has-next) 'token-left-curly-bracket))
		  (statements (analyze-statements tokens env))
		  (rbracket ((tokens 'has-next) 'token-right-curly-bracket)))
		(delete-context env)
		(if (or (null? lbracket) (null? statements) (null? rbracket)) 
			'() 
			statements)))

(define make-function-ast
	(lambda (return_type name args block env) 
		(declare-function (list name return_type args) env)
		(list 'function name return_type args block)))

(define (analyze-args tokens env)
	(let 	((type (analyze-type tokens))
			(name (analyze-name tokens))
			(next ((tokens 'get-next))))
			
			(cond 	((or (null? type) (null? name)) (error "bad tokens at function args" type name))
					((eq? (car next) 'token-comma) (cons (list type name) (analyze-args tokens env)))
					((eq? (car next) 'token-right-bracket) (cons (list type name) '()))
					(else (error "bad tokens")))))

(define (analyze-function tokens env)
	(let ((rtype (analyze-type tokens))
		  (name (analyze-name tokens))
		  (lbracket ((tokens 'has-next) 'token-left-bracket)))

			(create-context env)
		  
		  (if (or (null? rtype) (null? name) (null? lbracket))
		  	(begin 
		  		(delete-context env)
		  		'())
		  	(let ((args (analyze-args tokens env)) ;args can (not yet) be null and they eat the right bracket
		  		  (block (analyze-block tokens env)))
				(delete-context env)
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