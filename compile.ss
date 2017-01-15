;(import (rnrs (6)))
(use-modules (ice-9 binary-ports))
(load "common.ss")
(load "analyze.ss")
(load "parse.ss")

(define currently-compiled-func 0)

(define op-list '(INVALID
				  LOAD LOADS
				  DADD IADD DSUB ISUB DMUL IMUL DDIV IDIV IMOD DNEG INEG 
				  IPRINT DPRINT SPRINT 
				  I2D D2I S2I 
				  SWAP POP LOADVAR LOADSVAR LOADCTXVAR STOREVAR STORECTXVAR
				  DCMP ICMP
				  JA IFICMPNE IFICMPE IFICMPG IFICMPGE IFICMPL IFICMPLE
				  DUMP STOP CALL RETURN BREAK))

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
		(define (write-int16 int)
			(put-u8 output (logand int #xFF))
			(put-u8 output (logand (ash int -8) #xFF))
			(set! size (+ size 2)))
		(define (write-string str)
			(for-each write-byte (map char->integer (string->list str))))
		(define (write-op op)
			(write-byte (index op op-list (lambda () (error "no such operand" op)))))
		(define (dispatch m) 
			(cond ((eq? m 'write-sequence) write-sequence)
				  ((eq? m 'write-int64) write-int64)
				  ((eq? m 'write-int32) write-int32)
				  ((eq? m 'write-int16) write-int16)
				  ((eq? m 'write-string) write-string)
				  ((eq? m 'write-byte) write-byte)
				  ((eq? m 'write-op) write-op)
				  ((eq? m 'size) size)
				  ((eq? m 'port) output)
				  (else (error "make-writer: unknown dispatch call" m))))
	
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

(define (get-var-id id meta) 
	(((meta 'local-vars) 'add) id))

(define (compile-return exp writer meta)
	(compile-value (cadr exp) writer meta)
	(if (eq? currently-compiled-func 'main)
		((writer 'write-op) 'STOP)
		((writer 'write-op) 'RETURN)))

(define (compile-const-value exp writer meta)
	(cond
		((eq? (caddr exp) 'int)
			(begin
				((writer 'write-op) 'LOAD)
				((writer 'write-int64) (cadr exp))))
		((eq? (caddr exp) 'string)
			(begin
				((writer 'write-op) 'LOADS)
				((writer 'write-int64) (((meta 'const-pool) 'add) (cadr exp)))))
		(else (error "unsupported const value compiling for type " (caddr exp)))))

(define (compile-var-value exp writer meta)
	((writer 'write-op) 'LOADVAR)
	((writer 'write-int32) (get-var-id (cadr exp) meta))) 

(define (compile-builtin-func exp writer meta)
	(cond 
		((eq? (cadr exp) 'iprint) ((writer 'write-op) 'IPRINT))
		((eq? (cadr exp) 'dprint) ((writer 'write-op) 'DPRINT))
		((eq? (cadr exp) 'sprint) ((writer 'write-op) 'SPRINT))
		(else (error "unimplemented builtin function " (cadr exp)))))

(define (compile-func-call exp writer meta)
	;push values onto stack
	(for-each (lambda (e) (compile-value e writer meta)) (cadddr exp))
	;call the function
	(if (builtin-func? exp)
		(compile-builtin-func exp writer meta)
		(begin
			((writer 'write-op) 'CALL)
			((writer 'write-int64) (((meta 'func-pool) 'find) (cadr exp))))))

(define (compile-binop exp writer meta)
	(compile-value (caddr exp) writer meta)
	(compile-value (cadr exp) writer meta)
	(cond 
		((tagged? exp 'add) ((writer 'write-op) 'IADD))
		((tagged? exp 'multiply) ((writer 'write-op) 'IMUL))
		((tagged? exp 'substract) ((writer 'write-op) 'ISUB))
		((tagged? exp 'modulo) ((writer 'write-op) 'IMOD))
		(else (error "unimplemented binop " (car exp)))))

(define (compile-relop exp writer meta)
	(compile-value (caddr exp) writer meta)
	(compile-value (cadr exp) writer meta)
	(cond 
		((tagged? exp 'greater)
			(begin
				((writer 'write-op) 'IFICMPG)
				((writer 'write-int16) 12)))
		((tagged? exp 'greater-or-equals)
			(begin
				((writer 'write-op) 'IFICMPGE)
				((writer 'write-int16) 12)))
		((tagged? exp 'less)
			(begin
				((writer 'write-op) 'IFICMPL)
				((writer 'write-int16) 12)))
		((tagged? exp 'less-or-equals)
			(begin
				((writer 'write-op) 'IFICMPLE)
				((writer 'write-int16) 12)))
		((tagged? exp 'equals)
			(begin
				((writer 'write-op) 'IFICMPE)
				((writer 'write-int16) 12)))
		((tagged? exp 'and)
			(begin
				((writer 'write-op) 'LOAD)
				((writer 'write-int64) 0)
				((writer 'write-op) 'IFICMPGE)
				((writer 'write-int16) 15)
				((writer 'write-op) 'LOAD)
				((writer 'write-int64) 0)
				((writer 'write-op) 'IFICMPGE)
				((writer 'write-int16) 3)
				((writer 'write-op) 'JA)
				((writer 'write-int16) 12)))
		((tagged? exp 'or)
			(begin
				((writer 'write-op) 'LOAD)
				((writer 'write-int64) 0)
				((writer 'write-op) 'IFICMPL)
				((writer 'write-int16) 24)
				((writer 'write-op) 'LOAD)
				((writer 'write-int64) 0)
				((writer 'write-op) 'IFICMPL)
				((writer 'write-int16) 12))))
	
	((writer 'write-op) 'LOAD)
	((writer 'write-int64) 0)
	((writer 'write-op) 'JA)
	((writer 'write-int16) 9)
	((writer 'write-op) 'LOAD)
	((writer 'write-int64) 1))

(define (compile-cast exp writer meta)
	(compile-value (cadddr exp) writer meta)

	(cond 
		((eq? (cadr exp) 'int)
			(if (eq? (caddr exp) 'double)
				((writer 'write-op) 'D2I)
				(error (caddr exp) " to " (cadr exp) " cast not implemented")))
		((eq? (cadr exp) 'double)
			(if (eq? (caddr exp) 'int)
				((writer 'write-op) 'I2D)
				(error (caddr exp) " to " (cadr exp) " cast not implemented")))
		(else (error (caddr exp) " to " (cadr exp) " cast not implemented")))) 

(define (compile-var-decl exp writer meta)
	(((meta 'local-vars) 'add) (cadr exp)))

(define (compile-value exp writer pool)
	(cond 
		((null? exp) '())
		((const? exp) (compile-const-value exp writer pool))
		((var? exp) (compile-var-value exp writer pool))
		((func-call? exp) (compile-func-call exp writer pool))
		((binop? exp) (compile-binop exp writer pool))
		((relop? exp) (compile-relop exp writer pool))
		((cast? exp) (compile-cast exp writer pool))
		(else (error "unknown value type " (car exp)))))

(define (compile-assignment exp writer meta)
	(compile-value (caddr exp) writer meta)
	((writer 'write-op) 'STOREVAR)
	((writer 'write-int32) (get-var-id (cadr exp) meta))) ;4-byte var ID

(define (compile-conditional exp writer meta)
	(let [(main-bytecode (make-writer (open-output-string)))
		  (else-bytecode (make-writer (open-output-string)))]

		(compile-sequence (caddr exp) main-bytecode meta)
		(compile-sequence (cadddr exp) else-bytecode meta)
		(if (not (null? (cadddr exp)))
			(begin 
				((main-bytecode 'write-op) 'JA)
				((main-bytecode 'write-int16) (else-bytecode 'size))))

	((writer 'write-op) 'LOAD)
	((writer 'write-int64) 0)
	(compile-value (cadr exp) writer meta)
	((writer 'write-op) 'IFICMPLE)
	((writer 'write-int16) (main-bytecode 'size))
	((writer 'write-string) (get-output-string (main-bytecode 'port)))
	((writer 'write-string) (get-output-string (else-bytecode 'port)))
	
	))

(define (compile-loop exp writer meta)
	(let [(bytecode (make-writer (open-output-string)))]

		(compile-sequence (caddr exp) bytecode meta)
		(if (tagged? exp 'for-loop)
			(compile-expression (cadr (cdddr exp)) bytecode meta))
		((bytecode 'write-op) 'JA)
		((bytecode 'write-int16) (- (bytecode 'size)))

		(if (tagged? exp 'for-loop)
			(compile-expression (cadddr exp) writer meta))

		((writer 'write-op) 'LOAD)
		((writer 'write-int64) 0)
		(compile-value (cadr exp) writer meta)
		((writer 'write-op) 'IFICMPLE)
		((writer 'write-int16) (bytecode 'size))
		((writer 'write-string) (get-output-string (bytecode 'port)))))

(define (compile-expression exp writer meta)
	(cond 	((declaration? exp)
				(compile-var-decl exp writer meta))
			((assignment? exp)
				(compile-assignment exp writer meta))
			((return? exp)
				(compile-return exp writer meta))
			((conditional? exp)
				(compile-conditional exp writer meta))
			((loop? exp)
				(compile-loop exp writer meta))
			((func-call? exp)
				(begin
					(compile-func-call exp writer meta)
					(if (not (eq? (caddr exp) 'void)) ((writer 'write-op) 'POP)))) ;we don't need the returned value on the stack, only the side effects
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
		(if (> c 0) (arg-type-loop (- c 1))))

	(define (compile-func-bytecode block bytecode meta)
		(let [(arg-num 0)]
			(if (not (eq? (car (cadddr exp)) 'void))
				(for-each 
					(lambda (e) 
						((bytecode 'write-op) 'STOREVAR)
						((bytecode 'write-int32) arg-num)
						(((meta 'local-vars) 'add) (car e))
						(set! arg-num (+ arg-num 1)))
					(cadddr exp)))
			(compile-sequence block bytecode meta)
			(if (eq? (car (cadddr exp)) 'void) ((bytecode 'write-op) 'RETURN))
			((writer 'write-int64) (bytecode 'size))
			((writer 'write-string) (get-output-string (bytecode 'port)))))
		
	;registering func id
	(((meta 'local-vars) 'flush))
	(((meta 'func-pool) 'add) (cadr exp))
	(set! currently-compiled-func (cadr exp))
	;returns interned id for name from pool
	((writer 'write-int64) (((meta 'const-pool) 'add) (symbol->string (cadr exp))))
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
								  (apply + (map string-length ((meta 'const-pool) 'list)))))
		(for-each 
			(lambda (c) 
				((writer 'write-string) c)
				((writer 'write-sequence) #x00))
			((meta 'const-pool) 'list)))

	(write-magic)
	(write-version)
	(write-const-pool)
	(write-functions))

(define (compile-dwarf-file input-file output-file)
	(let [(ast (analyze (tokenize (read-file input-file))))
		  (writer (make-writer (open-output-file output-file)))
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

(define (compile-dwarf output-file . input-files)
	(let [(meta (make-meta-info))
		  (writer (make-writer (open-output-file output-file)))
		  (functions (map (lambda (file) (compile-dwarf-file file meta)) input-files))]
		
		(compile-header functions writer meta)

		))