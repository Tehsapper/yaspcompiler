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

(define (binop? exp) (memq (car exp) '(add divide modulo substract multiply)))

(define (relop? exp) (memq (car exp) '(equals greater less greater-or-equal less-or-equal not)))



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

(define (read-file filename) 
	(call-with-input-file filename 
		(lambda (port)
			(let f ((x (read-char port)))
				(if (eof-object? x) 
					'()
					(cons x (f (read-char port))))))))