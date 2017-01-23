(load "common.ss")
(load "parse.ss")

(define make-environment
	(lambda () (list 'env (make-stack) (make-stack))))

;it's a stack of contexts, which hold variable lists to be pushed or popped off upon ctx switch
(define (environment-ctx-vars env) (cadr env)) 
(define (environment-local-vars env) (car ((environment-ctx-vars env) 'list)))
(define (environment-functions env) (caddr env))
(define (environment-local-functions env) (car ((environment-functions env) 'list)))

(define (current-function env)
	(car (reverse (cadr ((environment-functions env) 'list)))))
(define (get-local-var-count env) (cdddr (current-function env)))

(define (analyze-type tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() 
			(if (type? (cadr x)) (cadr x) '() ))))

(define (analyze-name tokens)
	(let ((x ((tokens 'has-next) 'token-symbol)))
		(if (null? x) '() (cadr x))))

(define (create-context env)
	(((environment-ctx-vars env) 'push!) (list '())))

(define (delete-context env)
	(((environment-ctx-vars env) 'pop!)))

(define (var-type var) (cadr var))
(define (var-name var) (car var))

(define (func-return-type func) (cadr func))
(define (func-name func) (car func))
(define (func-params func) (caddr func))

(define (look-up-context name ctx)
	(define (loop c)
		(if (null? c)
			'()
			(let [(match (assoc name (car c)))]
				(if match match (loop (cdr c))))))
	(loop ctx))

(define (look-up-variable name env)
	(look-up-context name ((environment-ctx-vars env) 'list)))

(define (look-up-function name env)
	(look-up-context name ((environment-functions env) 'list)))

(define (declare-variable var env)
	(if (eq? (var-type var) 'void) 
		(error "variable declaration" "variable cannot be void" var)
		(if (null? (look-up-variable (var-name var) env)) 
			(begin
				(if (null? (environment-local-vars env)) 
					(set-car! ((environment-ctx-vars env) 'list) (list var)) 
					(append! (environment-local-vars env) (list var)))
				(set-car! (get-local-var-count env) (+ (car (get-local-var-count env)) 1)))
			(error "variable declaration" "variable already declared" (var-name var)))))

(define (declare-function func env)
	(if (null? (look-up-function (func-name func) env))
			(if (null? (environment-local-functions env))
				(set-car! ((environment-functions env) 'list) (list func))
				(append! (environment-local-functions env) (list func)))
			(error "function declaration" "function already declared" (func-name func))))

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
				(token-not-equals 5 not-equals)
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
			((tagged? t 'token-float-number) (list 'const-value (cadr t) 'double))
			((tagged? t 'token-string) (list 'const-value (cadr t) 'string))
			((tagged? t 'token-symbol) 
				(let [(var (look-up-variable (cadr t) env))]
					(if (null? var) 
						(let [(func (look-up-function (cadr t) env))]
							(if (null? func)
								(error "primary expression" "unknown named identifier" (cadr t))
								(analyze-func-value func tokens env)))
						(list 'var-value (var-name var) (var-type var)))))
			(else (error "primary expression" "unknown identifier" t)))))

(define (analyze-brackets t tokens env)
	(let [(expr (analyze-expression 0 tokens env))]
		(if (null? ((tokens 'has-next) 'token-right-bracket))
			(error "expression" "unbalanced parentheses in expression" ((tokens 'peek)))
			expr)))
		  	

(define (analyze-func-value func tokens env)
	(let [(arg-list '())]
		(define (arg-loop)
			(let [(value (analyze-expression 0 tokens env))]
				(if (null? arg-list)
					(set! arg-list (list value))
					(append! arg-list (list value)))
		  		(if (eq? (car ((tokens 'peek))) 'token-comma) 
		  			(begin
		  				((tokens 'advance))
		  				(arg-loop))
					arg-list)))

		(if (null? ((tokens 'has-next) 'token-left-bracket))
			(error "function call" "missing parentheses in function call" (func-name func))
		  	(begin
				(if (eq? (car ((tokens 'peek))) 'token-right-bracket)
					(set! arg-list '(void))
		  			(arg-loop))
		  		(if (null? ((tokens 'has-next) 'token-right-bracket))
					(error "function call" "mismatched parentheses in function call" (func-name func))
					(if (check-signature (if (eq? (car (func-params func)) 'void) '(void) (map cadr (func-params func))) (map get-type arg-list))
						(list 'function-call (func-name func) (func-return-type func) arg-list)
						(error "function call" "wrong argument types for function call")))))))
		  	

(define (analyze-cast left tokens env)
	(let [(type (analyze-type tokens))]
		(if (null? type)
			(error "analyze-cast" "bad cast to unknown type" type)
			(if (or (eq? 'void (get-type left)) (eq? 'void type))
				(error "analyze-cast" "cannot cast void value or to void type")
				(list 'cast type (get-type left) left)))))

(define (analyze-binary t left tokens env)
	(let [(in (assq (car t) infix))]
		(cond 
			(in (let [(right (analyze-expression (cadr in) tokens env))]
					(if (check-types left right)
						(list (caddr in) left right)
						(error "analyze-binary" "type mismatch" (get-type left) (get-type right)))))
			((tagged? t 'token-pointer) (analyze-cast left tokens env))
			(else (error "analyze-binary" "unknown binary operator" (car t))))))

(define (get-binding-power t)
	(cond 
		((assq (car t) infix) => cadr)
		((assq (car t) prefix) => cadr)
		((tagged? t 'token-pointer) 200)
		(else 0)))

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
					(if (check-types (var-type var) value)
						(success tokens (list 'assignment (var-name var) value))
						(error "assignment statement" "mismatched variable and assigned value types")))))))

(define (does-return? block)
	(let loop [(exp block)]
		(cond
			((null? exp) #f)
			((return? (car exp)) #t)
			((conditional? (car exp))
				(if (and (does-return? (caddr (car exp))) (does-return? (cadddr (car exp))))
					#t (loop (cdr exp))))
			((loop? (car exp))
				(if (does-return? (caddr (car exp))) #t (loop (cdr exp))))
			(else (loop (cdr exp))))))

(define (analyze-return choices tokens env)
	((tokens 'save))
	(let [(ret (analyze-name tokens))]
		(if (eq? ret 'return)
			(let [(value '())]
				(if (not (eq? (car ((tokens 'peek))) 'token-semicolon))
					(set! value (analyze-value tokens env)))
				(if ((tokens 'has-next) 'token-semicolon)
					(if (check-types value (func-return-type (current-function env)))
						(success tokens (list 'return value))
						(error "return statement" "mismatched function and returned types"))
					(error "return statement" "missing semicolon")))
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
							(error "if conditional" "main clause block fail")
							(let [(else-keyword ((tokens 'peek)))
								  (else-clause '())]
								(if (and (eq? (car else-keyword) 'token-symbol) (eq? (cadr else-keyword) 'else))
									(begin
										((tokens 'advance))
										(set! else-clause (analyze-block tokens env))))
								(if (eq? else-clause 'block-fail) (error "if conditional" "else clause block fail"))
								(success tokens (list 'conditional value main-clause else-clause)))))
					(error "if conditional" "mismatched parentheses at if condition")))	; if we matched both if keyword and a left-bracket, we're assured
			(fail choices tokens env))))

(define (analyze-while-loop choices tokens env)
	((tokens 'save))
	(let [(while-keyword (analyze-name tokens))]
		(if (and (eq? while-keyword 'while) ((tokens 'has-next) 'token-left-bracket))
			(let [(value (analyze-value tokens env))]
				(if ((tokens 'has-next) 'token-right-bracket)
					(let [(body (analyze-block tokens env))]
						(if (eq? body 'block-fail)
							(error "while body block fail")
							(success tokens (list 'while-loop value body))))
					(error "while loop" "mismatched parentheses at while loop condition")))
			(fail choices tokens env))))

(define (analyze-for-loop choices tokens env)
	((tokens 'save))
	(let [(for-keyword (analyze-name tokens))]
		(if (and (eq? for-keyword 'for) ((tokens 'has-next) 'token-left-bracket))
			(let [(initial (analyze-statement (list analyze-assignment analyze-func-call analyze-end-of-block) tokens env))
				  (condition (analyze-value tokens env))]
				(if ((tokens 'has-next) 'token-semicolon)
					(let [(post (analyze-statement (list analyze-assignment analyze-func-call analyze-end-of-block) tokens env))]
				  
						(if ((tokens 'has-next) 'token-right-bracket)
							(let [(body (analyze-block tokens env))]
								(if (eq? body 'block-fail)
									(error "for body block fail")
									(success tokens (list 'for-loop condition body initial post))))
							(error "for loop" "mismatched parentheses at for loop condition")))
					(error "for loop" "missing semicolon at for loop")))
			(fail choices tokens env))))

(define (analyze-break choices tokens env)
	((tokens 'save))
	(let [(keyword (analyze-name tokens))]
		(if (and (eq? keyword 'break) ((tokens 'has-next) 'token-semicolon))
			(success tokens (list 'break))
			(fail choices tokens env))))

(define (analyze-continue choices tokens env)
	((tokens 'save))
	(let [(keyword (analyze-name tokens))]
		(if (and (eq? keyword 'continue) ((tokens 'has-next) 'token-semicolon))
			(success tokens (list 'continue))
			(fail choices tokens env))))

(define (analyze-local-function choices tokens env)
	((tokens 'save))
	(let [(ast (analyze-function tokens env))]
		(if (null? ast)
			(fail choices tokens env)
			(success tokens ast))))

(define (analyze-end-of-block choices tokens env)
	(if (eq? (car ((tokens 'peek))) 'token-right-curly-bracket)
		'()		;valid statement
		(error "analyze" "unknown statement type starting with token" ((tokens 'peek)))))

(define statement-choices (list analyze-declaration
								analyze-local-function
								analyze-assignment
								analyze-return
								analyze-break
								analyze-continue
								analyze-func-call
								analyze-conditional
								analyze-while-loop
								analyze-for-loop
								analyze-end-of-block))

(define (fail choices tokens env)
	((tokens 'restore))
	((car choices) (cdr choices) tokens env))

(define (success tokens tree-node)
	((tokens 'accept)) 
	tree-node)

(define (analyze-statement choices tokens env)
	((car choices) (cdr choices) tokens env))

(define (analyze-statements tokens env)
	(let rec ((statement (analyze-statement statement-choices tokens env)))
		;(display statement) (newline)
		(if (null? statement)
			'() 
			(cons statement (rec (analyze-statement statement-choices tokens env))))))

(define (analyze-block tokens env)
	(create-context env)	; various blocks have their own scope
	(let [(left-bracket ((tokens 'has-next) 'token-left-curly-bracket))
		  (statements (analyze-statements tokens env))
		  (right-bracket ((tokens 'has-next) 'token-right-curly-bracket))]
		(delete-context env)
		(if (or (null? left-bracket) (null? right-bracket)) 
			'block-fail
			statements)))

(define (make-function-ast return_type name params block env)
	(let [(count (car (get-local-var-count env)))]
		(((environment-functions env) 'pop!))
		(list 'function name return_type params block count)))

(define (analyze-function-param-list tokens env)
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
							(error "analyze-function-param-list" "bad token" ((tokens 'peek)))
							(cons (list name type) '()))
						(cons (list name type) (analyze-function-param-list tokens env)))))))

(define (analyze-function-params tokens env)
	(let ((param-list (analyze-function-param-list tokens env)))
		(if (null? param-list)
			(if (null? ((tokens 'has-next) 'token-right-bracket)) 
				(error "analyze-function-params" "bad token" ((tokens 'peek)))
				(list 'void))
			param-list)))

(define (analyze-function tokens env)
	(let   [(return-type (analyze-type tokens))
			(name (analyze-name tokens))
		 	(left-bracket ((tokens 'has-next) 'token-left-bracket))]

		  	(if (or (null? return-type) (null? name) (null? left-bracket))
				'()
				(let [(params (analyze-function-params tokens env))]
					(declare-function (list name return-type params 0) env)
					(((environment-functions env) 'push!) (list '()))
					(create-context env)
					;function declaration form
					(if (null? ((tokens 'has-next) 'token-semicolon))
						(begin
							(if (not (eq? (car params) 'void))
							(for-each
									(lambda (param) (declare-variable (list (var-name param) (var-type param)) env))
									params))
							(let [(block (analyze-block tokens env))]
								(delete-context env)
								(if (eq? block 'block-fail) ;block actually can be empty
									(error "analyze-function" "bad function block" name)
									(if (does-return? block)
										(make-function-ast return-type name params block env)
										(error "analyze-function" "function does not return in all cases" name)))))
						(begin
							(((environment-functions env) 'pop!))
							(delete-context env)
							(analyze-function tokens env)))))))

(define (declare-builtin env)
	(for-each (lambda (f) (declare-function f env)) builtin-funcs))

(define (analyze token-list)
	(let ((env (make-environment))
		  (token-provider (make-token-provider token-list)))
	(((environment-functions env) 'push!) (list '()))
	(declare-builtin env)
	(let rec ((func (analyze-function token-provider env)))
		(if (null? func) 
			'()
			(cons func (rec (analyze-function token-provider env)))))))