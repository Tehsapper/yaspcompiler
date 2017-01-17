(define (type? x) (memq x '(string int double void)))
(define (has-null? x) (memq '() x))

(define (assign! list v)
	(set-car! list v) v)

(define (tagged? exp tag) (eq? (car exp) tag))

(define (assignment? exp) (tagged? exp 'assignment))

(define (conditional? exp) (tagged? exp 'conditional))

(define (declaration? exp) (tagged? exp 'declaration))

(define (var? exp) (tagged? exp 'var-value))

(define (return? exp) (tagged? exp 'return))

(define (const? exp) (tagged? exp 'const-value))

(define (cast? exp) (tagged? exp 'cast))

(define (func-call? exp) (tagged? exp 'function-call))

(define (loop? exp) (memq (car exp) '(for-loop while-loop do-loop)))

(define (arithm-op? exp) (memq (car exp) '(add divide modulo substract multiply neg)))
(define (rel-op? exp) (memq (car exp) '(equals not-equals greater less greater-or-equals less-or-equals)))
(define (logical-op? exp) (memq (car exp) '(and or not)))

(define (binary-op? exp) (memq (car exp) '(add divide modulo substract multiply equals not-equals greater less greater-or-equals less-or-equals and or)))
(define (unary-op? exp) (memq (car exp) '(neg not)))

(define builtin-funcs (list (list 'iprint 'void (list (list 'number 'int)) 1)
							(list 'sprint 'void (list (list 'str 'string)) 1)
							(list 'dprint 'void (list (list 'number 'double)) 1)))

(define (builtin-func? exp) (assq (cadr exp) builtin-funcs))

(define (check-signature sig call)
	(cond
		((and (null? sig) (null? call)) #t)
		((not (eq? (car sig) (car call))) #f)
		(else (check-signature (cdr sig) (cdr call)))))

(define (get-type exp)
	(cond
		((type? exp) exp)
		((null? exp) 'void)
		((rel-op? exp) 'int)
		((binary-op? exp) (get-type (caddr exp)))
		((unary-op? exp) (get-type (cadr exp)))
		((const? exp) (caddr exp))
		((cast? exp) (cadr exp))
		((var? exp) (caddr exp))
		((func-call? exp) (caddr exp))))

(define (check-types exp1 exp2) (eq? (get-type exp1) (get-type exp2)))

(define (index str pool fail)
	(let ((tail (member str (reverse pool))))
    	(if tail 
    		(length (cdr tail))
    		(fail))))

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

(define (make-table)
	(let [(local-table (list 'table))]
		(define (look-up first-key second-key)
			(let [(subtable (assoc first-key (cdr local-table)))]
				(if subtable
					(let [(record (assoc second-key (cdr subtable)))]
						(if record
							(cdr record)
							#f))
					#f)))
		(define (insert! first-key second-key value)
			(let [(subtable (assoc first-key (cdr local-table)))]
				(if subtable
					(let [(record (assoc second-key (cdr subtable)))]
						(if record
							(set-cdr! record value)
							(set-cdr! subtable
								(cons (cons second-key value)
									  (cdr subtable)))))
					(set-cdr! local-table
							  (cons (list first-key
							  			  (cons second-key value))
							  		(cdr local-table))))))
		(define (dispatch m)
			(cond ((eq? m 'look-up) look-up)
				  ((eq? m 'insert!) insert!)
				  (else (error "unknown table dispatch call: " m))))
		dispatch))

(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))