;(import (rnrs (6)))
(use-modules (ice-9 binary-ports))
(load "analyze.ss")
(load "parse.ss")

(define (tagged? exp tag) (if (eq? (car exp) tag) #t #f))

(define (const? exp)
	(cond 	((tagged? exp 'const-value) #t)
			(else #f)))

(define (lookup-var varname) #t)

(define currently-compiled-func 0)

(define (make-writer output)
	(let [(size 0)]
		(define (write-byte char)
			(put-u8 output char)
			(set! size (+ size 1)))
		(define (write-sequence . args)
			(for-each write-byte args))
		(define (write-int64 int)
			(put-u8 output (logand int #xFF))
			(put-u8 output (logand (ash int -8) #xFF))
			(put-u8 output (logand (ash int -16) #xFF))
			(put-u8 output (logand (ash int -24) #xFF))
			(put-u8 output (logand (ash int -32) #xFF))
			(put-u8 output (logand (ash int -40) #xFF))
			(put-u8 output (logand (ash int -48) #xFF))
			(put-u8 output (logand (ash int -56) #xFF))
			(set! size (+ size 8)))
		(define (write-int32 int)
			(put-u8 output (logand int #xFF))
			(put-u8 output (logand (ash int -8) #xFF))
			(put-u8 output (logand (ash int -16) #xFF))
			(put-u8 output (logand (ash int -24) #xFF))
			(set! size (+ size 4)))
		(define (write-string str)
			(for-each write-byte (map char->integer (string->list str))))
		(define (dispatch m) 
			(cond ((eq? m 'write-sequence) write-sequence)
				  ((eq? m 'write-int64) write-int64)
				  ((eq? m 'write-int32) write-int32)
				  ((eq? m 'write-string) write-string)
				  ((eq? m 'write-byte) write-byte)
				  ((eq? m 'size) size)
				  ((eq? m 'port) output)
				  (else (error "unknown call" m))))
	
		dispatch))

(define (make-meta-info)
	(let [(const-pool (make-const-pool))
		  (local-vars (make-const-pool))
		  (func-pool (make-const-pool))]
		(define (dispatch m)
			(cond 
				((eq? m 'const-pool) const-pool)
				((eq? m 'local-vars) local-vars)
				((eq? m 'func-pool) func-pool)))
		dispatch))

(define (index str pool fail)
	(let ((tail (member str (reverse pool))))
    	(if tail 
    		(length (cdr tail))
    		(fail))))

(define (make-const-pool)
	(let [(count 0)
		  (pool '())]
		(define (add str)
  			(let ((tail (member str (reverse pool))))
    			(if tail 
    				(length (cdr tail))
    				(begin
    					(set! pool (append pool (list str)))
    					(set! count (+ count 1))
    					(- count 1)))))
		(define (flush) 
			(set! pool '())
			(set! count 0))
		(define (find str)
			(index str pool (lambda () (error str " is not declared"))))
		(define (dispatch m)
			(cond ((eq? m 'add) add)
				  ((eq? m 'find) find)
				  ((eq? m 'flush) flush)
				  ((eq? m 'size) count)
				  ((eq? m 'list) pool)
				  (else (error "unknown call" m))))
		dispatch))

(define (var? exp)
	(cond 	((null? (lookup-var exp)) #f)
			(else #t)))

(define (assignment? exp) (tagged? exp 'assignment))

(define (conditional? exp) (tagged? exp 'conditional))

(define (declaration? exp) (tagged? exp 'declaration))

(define (return? exp) (tagged? exp 'return))

(define (function? exp) (tagged? exp 'function))

(define (binop? exp) (memq (car exp) '(add divide modulo substract multiply)))

(define (get-var-id id meta) 
	(((meta 'local-vars) 'add) id))

(define (compile-return exp writer meta)
	(compile-value (cadr exp) writer meta)
	(if (eq? currently-compiled-func 'main)
		((writer 'write-sequence) #x25)   ;STOP
		((writer 'write-sequence) #x27))) ;RETURN

(define (compile-const-value exp writer meta)
	((writer 'write-sequence) #x01)	;LOAD
	((writer 'write-int64) (cadr exp)))

(define (compile-var-value exp writer meta)
	((writer 'write-sequence) #x16) ;LOADVAR
	((writer 'write-int32) (get-var-id (cadr exp) meta))) 

(define (compile-func-call exp writer meta)
	;TO-DO: multiple args
	;push values onto stack
	(compile-value (caddr exp) writer meta)
	;call the function
	((writer 'write-sequence) #x26) ;CALL
	((writer 'write-int64) (((meta 'func-pool) 'find) (cadr exp))))

(define (compile-binop exp writer meta)
	(compile-value (cadr exp) writer meta)
	(compile-value (caddr exp) writer meta)
	(cond 
		((tagged? exp 'add) ((writer 'write-sequence) #x04))
		((tagged? exp 'multiply) ((writer 'write-sequence) #x08))
		(else (error "unimplemented binop " (cadr exp)))))


(define (compile-var-decl exp writer meta)
	(((meta 'local-vars) 'add) (cadr exp)))

(define (compile-value exp writer pool)
	(cond 
		((tagged? exp 'const-value) (compile-const-value exp writer pool))
		((tagged? exp 'var-value) (compile-var-value exp writer pool))
		((tagged? exp 'function-call) (compile-func-call exp writer pool))
		((binop? exp) (compile-binop exp writer pool))
		(else (error "unknown value type " (car exp)))))

(define (compile-assignment exp writer meta)
	(compile-value (caddr exp) writer meta)
	((writer 'write-sequence) #x19) ;STOREVAR
	((writer 'write-int32) (get-var-id (cadr exp) meta)) ;4-byte var ID
	)


(define (compile-conditional exp target link)
	'()
	)

(define (compile-expression exp writer meta)
	(cond 	((declaration? exp)
				(compile-var-decl exp writer meta))
			((assignment? exp)
				(compile-assignment exp writer meta))
			((return? exp)
				(compile-return exp writer meta))
			(else (error "unknown expression type " (car exp)))))

(define (compile-sequence exp writer meta)
	(for-each (lambda (e) (compile-expression e writer meta)) exp))

(define (compile-function exp writer meta)
	(define (arg-type-loop c)
		(if (or (> c (- (length (cadddr exp)) 1)) (eq? (car (cadddr exp)) 'void)) 
			((writer 'write-byte) #x00) 
			(cond 
				((eq? (cadr (list-ref (cadddr exp) c)) 'int) ((writer 'write-sequence) #x00))
				((eq? (cadr (list-ref (cadddr exp) c)) 'double) ((writer 'write-sequence) #x01))
				((eq? (cadr (list-ref (cadddr exp) c)) 'string) ((writer 'write-sequence) #x02))
				(else (error (cadr exp) ": unknown argument type " (cadr (list-ref (cadddr exp) c))))))
		(if (> c 0) (arg-type-loop (- c 1)) '()))

	(define (compile-func-bytecode block bytecode meta)
		(let [(arg-num 0)]
			(if (not (eq? (car (cadddr exp)) 'void))
				(for-each 
					(lambda (e) 
						((bytecode 'write-sequence) #x19) ;STOREVAR
						((bytecode 'write-int32) arg-num)
						(((meta 'local-vars) 'add) (car e))
						(set! arg-num (+ arg-num 1)))
					(cadddr exp)))
			(compile-sequence block bytecode meta)
			(if (eq? (car (cadddr exp)) 'void) ((bytecode 'write-sequence) #x27)) ;RETURN
			((writer 'write-int64) (bytecode 'size))
			((writer 'write-string) (get-output-string (bytecode 'port)))))
		
	;registering func id
	(((meta 'local-vars) 'flush))
	(((meta 'func-pool) 'add) (cadr exp))
	(set! currently-compiled-func (cadr exp))
	;returns interned id for name from pool
	((writer 'write-int64) (((meta 'const-pool) 'add) (cadr exp)))
	;local variable count
	((writer 'write-int64) (car (cddr (cdddr exp))))
	;exported -> yes?
	((writer 'write-int64) 0)
	;number of arguments
	(if (eq? (car (cadddr exp)) 'void) 
		((writer 'write-int64) 0)
		((writer 'write-int64) (length (cadddr exp))))
	(if (< 16 (length (cadddr exp))) 
		(error (cadr exp) ": no more than 16 arguments allowed")
		(arg-type-loop 15))
	
	(compile-func-bytecode (car (cdr (cdddr exp))) (make-writer (open-output-string)) meta)
	writer)

(define (compile-header ast writer meta)
	(define (write-magic)
		((writer 'write-sequence) #xBA #xBA))
	(define (write-version)
		((writer 'write-int64) 1))
	
	(define (write-functions)
		;number of functions
		((writer 'write-int64) (length ast)))

	(define (write-const-pool)
		;const-pool size
		((writer 'write-int64) (+ ((meta 'const-pool) 'size) 
								  (apply + (map string-length (map symbol->string ((meta 'const-pool) 'list))))))
		(for-each 
			(lambda (c) 
				((writer 'write-string) (symbol->string c)) 
				((writer 'write-sequence) #x00))
			((meta 'const-pool) 'list)))

	(write-magic)
	(write-version)
	(write-const-pool)
	(write-functions))

(define (compile-file input_file output_file)
	(let [(ast (analyze (tokenize (read-file input_file))))
		  (writer (make-writer (open-output-file output_file)))
		  (meta (make-meta-info))]

		(define (compile-functions ast)
			(let [(bytecode (make-writer (open-output-string)))]
				(if (null? ast)
					'()
					(cons (get-output-string ((compile-function (car ast) bytecode meta) 'port)) (compile-functions (cdr ast))))))

		(define (write-functions fs)
			(for-each (writer 'write-string) fs))

		(let [(func-bytecode (compile-functions ast))]
			(compile-header ast writer meta)
			(write-functions func-bytecode))
		(close-output-port (writer 'port))))