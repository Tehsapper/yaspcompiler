(load "common.ss")
(load "parse.ss")

(define make-environment
	(lambda () (list 'env (make-stack) '())))

;it's a stack of contexts, which hold variable lists to be pushed or popped off upon ctx switch
(define (environment-ctx-vars env) (cadr env)) 
(define (environment-local-vars env) (car ((environment-ctx-vars env) 'list)))
(define (environment-functions env) (caddr env))
(define (get-local-var-count env) (cdddr (car (reverse (environment-functions env)))))

(define (analyze-type tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() 
			(if (type? (cadr x)) (cadr x) '() ))))

(define (analyze-name tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() (cadr x))))

(define (create-context env) (((environment-ctx-vars env) 'push!) (list '())))
(define (delete-context env) (((environment-ctx-vars env) 'pop!)))

(define (var-type var) (cadr var))
(define (var-name var) (car var))

(define (func-return-type func) (cadr func))
(define (func-name func) (car func))
(define (func-args func) (caddr func))

(define (declare-variable var env)
	(if (eq? (var-type var) 'void) 
		(error "variable cannot be void" var)
		(if (null? (look-up-variable (var-name var) env)) 
			(begin
				(if (null? (environment-local-vars env)) 
					(set-car! ((environment-ctx-vars env) 'list) (list var)) 
					(append! (environment-local-vars env) (list var)))
				(set-car! (get-local-var-count env) (+ (car (get-local-var-count env)) 1)))
			(error "variable already declared" (var-name var)))))

(define (look-up-variable name env)
	(define (local-loop vars)
		(if (null? vars) 
			'()
			(if (eq? (var-name (car vars)) name) 
				(car vars)
				(local-loop (cdr vars)))))
	(define (loop ctx)
		(if (null? ctx) 
			'()
			(let ((var (local-loop (car ctx))))
				(if (null? var)
					(loop (cdr ctx))
					var))))
	(loop ((environment-ctx-vars env) 'list)))

(define (declare-function func env)
	(if (null? (look-up-function (func-name func) env)) 
			(if (null? (environment-functions env)) 
				(set-car! (cddr env) (list func)) 
				(append! (environment-functions env) (list func)))
			(error "function already declared" (func-name func))))

(define (look-up-function name env)
	(let loop ((funcs (environment-functions env)))
		(if (null? funcs) 
			'()
			(if (eq? (func-name (car funcs)) name) 
				(car funcs) 
				(loop (cdr funcs))))))

(define (analyze-variable tokens env)
	(let ((name (analyze-name tokens)))
		(if (null? name) 
			'()
			(look-up-variable name env))))

(define infix '((token-plus 10 add) 
				(token-minus 10 substract)
				(token-star 20 multiply)
				(token-divide 20 divide)
				(token-percent 20 modulo)
				(token-greater 5 greater)
				(token-greater-equals 5 greater-or-equals)
				(token-less 5 less)
				(token-less-equals 5 less-or-equals)
				(token-equals 5 equals)
				(token-logical-and 2 and)
				(token-logical-or 2 or)))

(define prefix '((token-minus 100 neg)
				 (token-bang 100 not)))

(define (analyze-unary t tokens env)
	(let [(pre (assq (car t) prefix))]
		(cond 
			(pre (list (caddr pre) (analyze-expression (cadr pre) tokens env)))
			((tagged? t 'token-left-bracket) (analyze-brackets t tokens env))
			((tagged? t 'token-plus) (analyze-expression 100 tokens env)) ;unary plus
			((tagged? t 'token-number) (list 'const-value (cadr t) 'int))
			((tagged? t 'token-string) (list 'const-value (cadr t) 'string))
			((tagged? t 'token-symbol) 
				(let [(var (look-up-variable (cadr t) env))]
					(if (null? var) 
						(let [(func (look-up-function (cadr t) env))]
							(if (null? func)
								(error "unknown named identifier: " (cadr t))
								(analyze-func-value func tokens env)))
						(list 'var-value (var-name var) (var-type var)))))
			(else (error "unknown identifier: " t)))))

(define (analyze-brackets t tokens env)
	(let [(expr (analyze-expression 0 tokens env))]
		(if (null? ((tokens 'has-next) 'token-right-bracket))
		  	(error "unbalanced parentheses in expression" ((tokens 'peek)))
		  	expr)))
		  	

(define (analyze-func-value func tokens env)
	(let [(call-list '())]

		(define (arg-loop)
			(let [(value (analyze-expression 0 tokens env))]
		  		(if (null? call-list)
		  			(set! call-list (list value))
		  			(append! call-list (list value)))
		  		(if (eq? (car ((tokens 'peek))) 'token-comma) 
		  			(begin
		  				((tokens 'advance))
		  				(arg-loop))
		  			call-list)))

		(if (null? ((tokens 'has-next) 'token-left-bracket))
			(error "missing parentheses in function call" (func-name func))
		  	(begin
		  		(if (eq? ((tokens 'peek)) 'token-right-bracket)
		  			(set! call-list '(void))
		  			(arg-loop))
		  		(if (null? ((tokens 'has-next) 'token-right-bracket))
		  			(error "mismatched parentheses in function call" (func-name func))
		  			(list 'function-call (func-name func) (func-return-type func) call-list ))))))
		  	

(define (analyze-cast left tokens env)
	(let [(type (analyze-type tokens))]
		(if (null? type)
			(error "bad cast to unknown type" type)
			(if (or (eq? 'void (get-type left env)) (eq? 'void type)) 
				(error "cannot cast void value or to void type")
				(list 'cast type (get-type left env) left)))))

(define (analyze-binary t left tokens env)
	(let [(in (assq (car t) infix))]
		(cond 
			(in (let [(right (analyze-expression (cadr in) tokens env))]
					(if (check-types left right env) 
						(list (caddr in) left right)
						(error "type mismatch" (get-type left env) (get-type right env)))))
			((tagged? t 'token-pointer) (analyze-cast left tokens env))
			(else (error "unknown binary operator" (car t))))))

(define (get-binding-power t)
	(let [(in (assq (car t) infix))
		  (pre (assq (car t) prefix))]
	(cond 
		(in (cadr in))
		(pre (cadr pre))
		((tagged? t 'token-left-bracket) 0)
		((tagged? t 'token-right-bracket) 0)
		((tagged? t 'token-comma) 0)
		((tagged? t 'token-pointer) 200)
		((tagged? t 'token-semicolon) 0))))

(define (analyze-value tokens env)
	(analyze-expression 0 tokens env))

(define (analyze-expression rbp tokens env)
	(let [(t ((tokens 'get-next)))
		  (left '())]
		
		(define (loop) 
			(if (< rbp (get-binding-power ((tokens 'peek)) ))
				(begin
					(set! t ((tokens 'get-next)))
					(set! left (analyze-binary t left tokens env))
					(loop)
				)))
		(set! left (analyze-unary t tokens env))
		(loop)
		left))

(define (check-signature sig call)
	(cond 
		((and (null? sig) (null? call)) #t)
		((not (eq? (car sig) (car call))) #f)
		(else (check-signature (cdr sig) (cdr call)))))

(define (get-type exp env)
	(cond
		((type? exp) exp)
		((null? exp) 'void)
		((binop? exp) (get-type (caddr exp) env))
		((const? exp) (caddr exp))
		((relop? exp) 'int)
		((cast? exp) (cadr exp))
		((var? exp) (var-type (look-up-variable (cadr exp) env)))
		((func-call? exp) (func-return-type (look-up-function (cadr exp) env)))))

(define (check-types exp1 exp2 env) (eq? (get-type exp1 env) (get-type exp2 env)))

(define (analyze-declaration choices tokens env)
	((tokens 'save))
	(let [(type (analyze-type tokens))
		  (name (analyze-name tokens))
		  (semicolon ((tokens 'has-next) 'token-semicolon))]
		(if (or (null? type) (null? name) (null? semicolon)) 
			(fail choices tokens env)
			(begin
				(declare-variable (list name type) env) 
				(success tokens (list 'declaration name type))))))

(define (analyze-assignment choices tokens env)
	((tokens 'save))
	(let [(var (analyze-variable tokens env))]
		(if (null? var) 
			(fail choices tokens env)
			(let [(eqv ((tokens 'has-next) 'token-equals))
		  		  (value (analyze-value tokens env))]
				(if (or (null? eqv) (null? value) (not ((tokens 'has-next) 'token-semicolon)))
					(fail choices tokens env)
					(if (check-types (var-type var) value env)
						(success tokens (list 'assignment (var-name var) value))
						(error "mismatched variable and assigned value types")))))))

(define (analyze-return choices tokens env)
	((tokens 'save))
	(let [(ret (analyze-name tokens))]
		(if (eq? ret 'return)
			(let [(value '())]
				(display ((tokens 'peek))) (newline)
				(if (not (eq? (car ((tokens 'peek))) 'token-semicolon))
					(set! value (analyze-value tokens env)))
				(display value) (newline)
				(if ((tokens 'has-next) 'token-semicolon)
					(if (check-types value (func-return-type (car (reverse (environment-functions env)))) env)
						(success tokens (list 'return value))
						(error "mismatched function and returned types"))
					(error "bad return")))
			(fail choices tokens env))))

(define (analyze-func-call choices tokens env)
	((tokens 'save))
	(let [(func (look-up-function (analyze-name tokens) env))]
		(if (null? func)
			(fail choices tokens env)
			(let [(value (analyze-func-value func tokens env))]
				(if ((tokens 'has-next) 'token-semicolon)
					(success tokens value)
					(fail choices tokens env))))))

(define (analyze-conditional choices tokens env)
	((tokens 'save))
	(let [(if-keyword (analyze-name tokens))]
		(if (and (eq? if-keyword 'if) ((tokens 'has-next) 'token-left-bracket))
			(let [(value (analyze-value tokens env))]
				(if ((tokens 'has-next) 'token-right-bracket)
					(let [(main-clause (analyze-block tokens env))]
						(if (eq? main-clause 'block-fail)
							(error "main clause block fail")
							(let [(else-keyword ((tokens 'peek)))
								  (else-clause '())]
								(if (and (eq? (car else-keyword) 'token-symbol) (eq? (cadr else-keyword) 'else))
									(begin
										((tokens 'advance))
										(set! else-clause (analyze-block tokens env))))
								(if (eq? else-clause 'block-fail) (error "else clause block fail"))
								(success tokens (list 'conditional value main-clause else-clause)))))
					(error "mismatched parentheses at if condition")))	; if we matched both if keyword and a left-bracket, we're assured
			(fail choices tokens env))))

(define (analyze-end-of-block choices tokens env)
	(if (eq? (car ((tokens 'peek))) 'token-right-curly-bracket)
		'()		;valid statement
		(error "unknown statement type starting with token" ((tokens 'peek)))))

(define statement-choices (list analyze-declaration
								analyze-assignment
								analyze-return
								analyze-func-call
								analyze-conditional
								analyze-end-of-block))

(define (fail choices tokens env)
	((tokens 'restore))
	((car choices) (cdr choices) tokens env))

(define (success tokens tree-node)
	((tokens 'accept)) 
	tree-node)

(define (analyze-statement tokens env)
	((car statement-choices) (cdr statement-choices) tokens env))

(define (analyze-statements tokens env)
	(let rec ((statement (analyze-statement tokens env)))
		(display statement) (newline)
		(if (null? statement)
			'() 
			(cons statement (rec (analyze-statement tokens env))))))

(define (analyze-block tokens env)
	(create-context env)	; various blocks have their own scope
	(let [(left-bracket ((tokens 'has-next) 'token-left-curly-bracket))
		  (statements (analyze-statements tokens env))
		  (right-bracket ((tokens 'has-next) 'token-right-curly-bracket))]
		(delete-context env)
		(if (or (null? left-bracket) (null? right-bracket)) 
			'block-fail
			statements)))

(define make-function-ast
	(lambda (return_type name args block env) 
		(list 'function name return_type args block (car (get-local-var-count env)))))

(define (analyze-function-args-list tokens env)
	((tokens 'save))
	(let   [(type (analyze-type tokens))
			(name (analyze-name tokens))]
			
			(if (or (null? type) (null? name))
				(begin 
					((tokens 'restore))
					'())
				(begin
					((tokens 'accept))
					(if (null? ((tokens 'has-next) 'token-comma))
						(if (null? ((tokens 'has-next) 'token-right-bracket)) 
							(error "analyze-function-args-list: bad token" ((tokens 'peek)))
							(cons (list name type) '()))
						(cons (list name type) (analyze-function-args-list tokens env)))))))

(define (analyze-function-args tokens env)
	(let ((arg-list (analyze-function-args-list tokens env)))
		(if (null? arg-list) 
			(if (null? ((tokens 'has-next) 'token-right-bracket)) 
				(error "analyze-function-args: bad token" ((tokens 'peek))) 
				(list 'void))
			arg-list)))

(define (analyze-function tokens env)
	((tokens 'save))
	(create-context env)
	(let   [(return-type (analyze-type tokens))
			(name (analyze-name tokens))
		 	(left-bracket ((tokens 'has-next) 'token-left-bracket))]

		  	(if (or (null? return-type) (null? name) (null? left-bracket))
		  		(begin 
		  			(delete-context env)
		  			((tokens 'restore))
		  			'())
		  		(let [(args (analyze-function-args tokens env))]

		  			(declare-function (list name return-type args 0) env)
		  			(if (not (eq? (car args) 'void)) (for-each (lambda (arg) (declare-variable (list (var-name arg) (var-type arg)) env)) args))
		  			(let [(block (analyze-block tokens env))]
						(delete-context env)
						(if (eq? block 'block-fail) ;block actually can be empty 
							(error "bad function block")
							(begin
								((tokens 'accept))
								(make-function-ast return-type name args block env))))))))

(define (declare-builtin env)
	(for-each (lambda (f) (declare-function f env)) builtin-funcs))

(define (analyze token-list)
	(let ((env (make-environment))
		  (token-provider (make-token-provider token-list)))
	(declare-builtin env)
	(let rec ((func (analyze-function token-provider env)))
		(if (null? func) 
			'()
			(cons func (rec (analyze-function token-provider env)))))))