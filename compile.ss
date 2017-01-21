(import (rnrs (6)))
(load "common.ss")
(load "analyze.ss")
(load "parse.ss")

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
			(let [(bv (make-bytevector 8))]
				(bytevector-s64-native-set! bv 0 int)
				(write-bytevector bv)))
		(define (write-double double)
			(let [(bv (make-bytevector 8))]
				(bytevector-ieee-double-native-set! bv 0 double)
				(write-bytevector bv)))
		(define (write-int32 int)
			(let [(bv (make-bytevector 4))]
				(bytevector-s32-native-set! bv 0 int)
				(write-bytevector bv)))
		(define (write-int16 int)
			(let [(bv (make-bytevector 2))]
				(bytevector-s16-native-set! bv 0 int)
				(write-bytevector bv)))
		(define (write-string str)
			(for-each write-byte (map char->integer (string->list str))))
		(define (write-bytevector bv)
			(put-bytevector output bv)
			(set! size (+ size (bytevector-length bv))))
		(define (write-op op)
			(write-byte (index op op-list (lambda () (error "write-op" "no such operand" op)))))
		(define (dispatch m) 
			(cond ((eq? m 'write-sequence) write-sequence)
				  ((eq? m 'write-double) write-double)
				  ((eq? m 'write-int64) write-int64)
				  ((eq? m 'write-int32) write-int32)
				  ((eq? m 'write-int16) write-int16)
				  ((eq? m 'write-string) write-string)
				  ((eq? m 'write-bytevector) write-bytevector)
				  ((eq? m 'write-byte) write-byte)
				  ((eq? m 'write-op) write-op)
				  ((eq? m 'size) size)
				  (else (error "make-writer" "unknown dispatch call" m))))
	
		dispatch))

(define (make-meta-info)
	(let [(const-pool (make-const-pool))
		  (ctx-vars (make-ctx-vars))
		  (func-registry (make-function-registry))]
		(define (dispatch m)
			(cond 
				((eq? m 'const-pool) const-pool)
				((eq? m 'ctx-vars) ctx-vars)
				((eq? m 'func-registry) func-registry)
				(else (error "meta-info" "unknown dispatch call" m))))
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
			(index str pool (lambda () (error "pool" "undeclared identifier" str))))
		(define (dispatch m)
			(cond ((eq? m 'add) add)
				  ((eq? m 'find) find)
				  ((eq? m 'flush) flush)
				  ((eq? m 'size) count)
				  ((eq? m 'list) pool)
				  (else (error "unknown call" m))))
		dispatch))

(define (make-function-registry)
	(let [(registry '())]
		(define (declare name)
			(if (find name)
				(error "function-registry" "function already declared" name)
				(set! registry (append registry (list (list name '()))))))
		(define (find name)
			(define (loop l)
				(if (null? l) #f (if (eq? name (caar l)) l (loop (cdr l)))))
			(loop (reverse registry)))
		(define (get-id name)
			(cond ((find name) => (lambda (l) (length (cdr l))))
				  (else #f)))
		(define (definition name bytecode)
			(cond ((find name) => (lambda (l) (set-car! (cdr (car l)) bytecode)))
				  (else (error "function-registry" "undeclared function" name))))
		(define (dispatch m)
			(cond
				((eq? m 'declare) declare)
				((eq? m 'get-id) get-id)
				((eq? m 'definition) definition)
				((eq? m 'list) registry)
				((eq? m 'size) (length registry))
				(else (error "function-registry" "unknown dispatch call" m))))
	dispatch))

(define (make-ctx-vars)
	(let [(ctxs (make-stack))]
		(define (create! name)
			((ctxs 'push!) (list (list name))))
		(define (destroy!)
			((ctxs 'pop!)))
		(define (declare var)
			(set-cdr! (ctxs 'top) (cons var (cdr (ctxs 'top)))))
		(define (get-var-info var)
			(define (loop l)
				(cond ((null? l) #f)
					  ((member var (cdar l)) => (lambda (e) (cons (caar l) (length (cdr e)))))
					  (else (loop (cdr l)))))
			(loop (ctxs 'list)))
		(define (dispatch m)
			(cond
				((eq? m 'create!) create!)
				((eq? m 'destroy!) destroy!)
				((eq? m 'declare) declare)
				((eq? m 'get-var-info) get-var-info)
				((eq? m 'top) (ctxs 'top))
				(else (error "ctx-vars" "unknown dispatch call" m))))
		dispatch))

(define (currently-compiled-func meta) (car ((meta 'ctx-vars) 'top)))

(define (load-value writer value)
	((writer 'write-op) 'LOAD)
	((writer 'write-int64) value))
(define (cond-jump writer op offset)
	((writer 'write-op) op)
	((writer 'write-int16) offset))

(define (compile-return exp writer meta)
	(compile-value (cadr exp) writer meta)
	(if (eq? (currently-compiled-func meta) 'main)
		((writer 'write-op) 'STOP)
		((writer 'write-op) 'RETURN)))

(define (compile-const-value exp writer meta)
	(cond
		((eq? (caddr exp) 'int)
			(load-value writer (cadr exp)))
		((eq? (caddr exp) 'string)
			(begin
				((writer 'write-op) 'LOADS)
				((writer 'write-int64) (((meta 'const-pool) 'add) (cadr exp)))))
		((eq? (caddr exp) 'double)
			(begin
				((writer 'write-op) 'LOAD)
				((writer 'write-double) (cadr exp))))
		(else (error "unsupported const value compiling for type " (caddr exp)))))

(define (compile-var-value exp writer meta)
	(cond ((((meta 'ctx-vars) 'get-var-info) (cadr exp)) => 
		(lambda (var-info)
			(if (eq? (car var-info) (currently-compiled-func meta))
				(begin
					((writer 'write-op) 'LOADVAR)
					((writer 'write-int32) (cdr var-info)))
				(begin
					((writer 'write-op) 'LOADCTXVAR)
					((writer 'label) (car var-info))
					((writer 'write-int64) 0)
					((writer 'write-int32) (cdr var-info))))))
		(else (error "compile-var-value" "undeclared variable" (cadr exp)))))

(define (compile-builtin-func exp writer meta)
	(cond 
		((eq? (cadr exp) 'iprint) ((writer 'write-op) 'IPRINT))
		((eq? (cadr exp) 'dprint) ((writer 'write-op) 'DPRINT))
		((eq? (cadr exp) 'sprint) ((writer 'write-op) 'SPRINT))
		(else (error "compile-builtin-func" "unimplemented builtin function" (cadr exp)))))

(define (compile-func-call exp writer meta)
	;push values onto stack
	(for-each (lambda (e) (compile-value e writer meta)) (reverse (cadddr exp)))
	;call the function
	(if (builtin-func? exp)
		(compile-builtin-func exp writer meta)
		(begin
			((writer 'write-op) 'CALL)
			((writer 'label) (cadr exp))
			((writer 'write-int64) 0))))

(define operation-table (make-table))
(define get-op (operation-table 'look-up))
(define put-op (operation-table 'insert!))

(put-op 'int 'add 'IADD)
(put-op 'int 'multiply 'IMUL)
(put-op 'int 'substract 'ISUB)
(put-op 'int 'divide 'IDIV)
(put-op 'int 'modulo 'IMOD)
(put-op 'int 'neg 'INEG)
(put-op 'double 'add 'DADD)
(put-op 'double 'substract 'DSUB)
(put-op 'double 'multiply 'DMUL)
(put-op 'double 'divide 'DDIV)
(put-op 'double 'neg 'DNEG)

(define (compile-arithmetic-op exp writer meta)
	(if (binary-op? exp) (compile-value (caddr exp) writer meta))
	(compile-value (cadr exp) writer meta)

	(cond ((get-op (get-type exp) (car exp)) => (writer 'write-op))
		  (else (error "compile-arithmetic-op" "unimplemented operator for type" (car exp) (get-type exp)))))

(put-op 'int 'greater 'IFICMPG)
(put-op 'int 'greater-or-equals 'IFICMPGE)
(put-op 'int 'less 'IFICMPL)
(put-op 'int 'less-or-equals 'IFICMPLE)
(put-op 'int 'equals 'IFICMPE)
(put-op 'int 'not-equals 'IFICMPNE)
(put-op 'double 'greater '(IFICMPL 0))
(put-op 'double 'greater-or-equals '(IFICMPL -1))
(put-op 'double 'less '(IFICMPG 0))
(put-op 'double 'less-or-equals '(IFICMPG 1))
(put-op 'double 'equals '(IFICMPE 0))
(put-op 'double 'not-equals '(IFICMPNE 0))

(define (compile-relational-op exp writer meta)
	; relational operators are binary operators
	(compile-value (caddr exp) writer meta)
	(compile-value (cadr exp) writer meta)

	(let [(op (get-op (get-type (cadr exp)) (car exp)))]
		(if op
			(if (list? op)	
				(begin
					((writer 'write-op) 'DCMP) ;we're assuming that it's a double
					(load-value writer (cadr op))
					(cond-jump writer (car op) 12))
				(cond-jump writer op 12))
			(error "compile-relop" "unimplemented operator for type" (car exp) (get-type (cadr exp)))))
	
	(load-value writer 0)
	(cond-jump writer 'JA 9)
	(load-value writer 1))

(define (compile-logical-op exp writer meta)
	(if (binary-op? exp) (compile-value (caddr exp) writer meta))
	(compile-value (cadr exp) writer meta)

	(if (eq? (get-type (cadr exp)) 'int)
		(begin
			(cond 
				((tagged? exp 'and)
					(begin
						(load-value writer 0)
						(cond-jump writer 'IFICMPGE 15)
						(load-value writer 0)
						(cond-jump writer 'IFICMPGE 3)
						(cond-jump writer 'JA 12)))
				((tagged? exp 'or)
					(begin
						(load-value writer 0)
						(cond-jump writer 'IFICMPL 24)
						(load-value writer 0)
						(cond-jump writer 'IFICMPL 12)))
				((tagged? exp 'not)
					(begin
						(load-value writer 0)
						(cond-jump writer 'IFICMPGE 12)))
				(else (error "compile-logical-op" "unimplemented operator" (car exp))))
			(load-value writer 0)
			(cond-jump writer 'JA 9)
			(load-value writer 1))
		(error "compile-logical-op" "logical operators are only supported for int type")))

(define casts-table (make-table))
(define get-cast (casts-table 'look-up))
(define put-cast (casts-table 'insert!))

(put-cast 'int 'double 'I2D)
(put-cast 'double 'int 'D2I)
(put-cast 'string 'int 'S2I)

(define (compile-cast exp writer meta)
	(let [(cast (get-cast (caddr exp) (cadr exp)))]
		(if cast
			(begin
				(compile-value (cadddr exp) writer meta)
				((writer 'write-op) cast))
			(error "compile-cast" "unimplemented cast from ... to ..." (caddr exp) (cadr exp)))))

(define (compile-var-decl exp writer meta)
	(((meta 'ctx-vars) 'declare) (cadr exp)))

(define (compile-value exp writer pool)
	(cond 
		((null? exp) '())
		((const? exp) (compile-const-value exp writer pool))
		((var? exp) (compile-var-value exp writer pool))
		((func-call? exp) (compile-func-call exp writer pool))
		((arithm-op? exp) (compile-arithmetic-op exp writer pool))
		((rel-op? exp) (compile-relational-op exp writer pool))
		((logical-op? exp) (compile-logical-op exp writer pool))
		((cast? exp) (compile-cast exp writer pool))
		(else (error "compile-value" "unknown value type" (car exp)))))

(define (compile-assignment exp writer meta)
	(compile-value (caddr exp) writer meta)

	(cond ((((meta 'ctx-vars) 'get-var-info) (cadr exp)) => 
		(lambda (var-info)
			(if (eq? (car var-info) (currently-compiled-func meta))
				(begin
					((writer 'write-op) 'STOREVAR)
					((writer 'write-int32) (cdr var-info)))
				(begin
					((writer 'write-op) 'STORECTXVAR)
					((writer 'label) (car var-info))
					((writer 'write-int64) 0)
					((writer 'write-int32) (cdr var-info))))))
		(else (error "compile-assignment" "undeclared variable" (cadr exp)))))

(define (compile-conditional exp writer meta)
	(if (eq? (get-type (cadr exp)) 'int)
		(let [(main-bc (make-bytecode))
			  (else-bc (make-bytecode))]

			(compile-sequence (cadddr exp) else-bc meta)
			(compile-sequence (caddr exp) main-bc meta)

			(if (not (null? (cadddr exp)))
				(cond-jump main-bc 'JA (else-bc 'size)))

			(load-value writer 0)
			(compile-value (cadr exp) writer meta)
			(cond-jump writer 'IFICMPLE (main-bc 'size))

			((writer 'merge) main-bc)
			((writer 'merge) else-bc))
		(error "compile-conditional" "condition must have int type, got" (get-type (cadr exp)))))

(define (compile-loop exp writer meta)
	(if (eq? (get-type (cadr exp)) 'int)
		(let [(body-bc (make-bytecode))
			  (condition-bc (make-bytecode))
			  (cont-mark 0)]

			;initial statement
			(if (tagged? exp 'for-loop)
				(compile-statement (cadddr exp) writer meta))

			; loop condition
			(load-value condition-bc 0)
			(compile-value (cadr exp) condition-bc meta)
			((condition-bc 'write-op) 'IFICMPLE)

			; compiling loop body
			(compile-sequence (caddr exp) body-bc meta)
			; and post-loop statement
			(set! cont-mark (body-bc 'size))
			(if (tagged? exp 'for-loop)
				(compile-statement (cadr (cdddr exp)) body-bc meta))
			((body-bc 'write-op) 'JA)

			; offset to get out
			((condition-bc 'write-int16) (+ (body-bc 'size) 2))

			; assigning proper offsets for 'break' and 'continue' statements
			(let [(break-table (((body-bc 'get-reloc-table) 'look-up) '*loop-break*))
				  (continue-table (((body-bc 'get-reloc-table) 'look-up) '*loop-continue*))
				  (bv (body-bc 'get-bv))]

				(if (and break-table (not (null? (cdr break-table))))
					(for-each (lambda (b) (bytevector-s16-native-set! bv b (- (body-bc 'size) b))) (cdr break-table)))

				(if (and continue-table (not (null? (cdr continue-table))))
					(for-each (lambda (b) (bytevector-s16-native-set! bv b (- cont-mark b 2))) (cdr continue-table)))

				(((body-bc 'get-reloc-table) 'purge!) '*loop-break*)
				(((body-bc 'get-reloc-table) 'purge!) '*loop-continue*)
				((condition-bc 'merge-ready-bv) body-bc bv)
				; setting proper offset for the jump back
				((condition-bc 'write-int16) (- (+ (condition-bc 'size) 2)))
				((writer 'merge) condition-bc)))
		(error "compile-loop" "condition must have int type, got" (get-type (cadr exp)))))

(define (compile-break exp writer meta)
	(let [(reloc (writer 'get-reloc-table))]
		((writer 'write-op) 'JA)
		(if (tagged? exp 'break)
			((writer 'label) '*loop-break*)
			((writer 'label) '*loop-continue*))
		((writer 'write-int16) 0)))

(define (compile-statement exp writer meta)
	(cond 	((declaration? exp)
				(compile-var-decl exp writer meta))
			((assignment? exp)
				(compile-assignment exp writer meta))
			((return? exp)
				(compile-return exp writer meta))
			((break? exp)
				(compile-break exp writer meta))
			((function? exp)
				(compile-function exp (make-bytecode) meta))
			((conditional? exp)
				(compile-conditional exp writer meta))
			((loop? exp)
				(compile-loop exp writer meta))
			((func-call? exp)
				(begin
					(compile-func-call exp writer meta)
					(if (not (eq? (caddr exp) 'void)) ((writer 'write-op) 'POP)))) ;we don't need the returned value on the stack, only the side effects
			(else (error "compile-statement" "unknown statement type" (car exp)))))

(define (compile-sequence exp writer meta)
	(for-each (lambda (e) (compile-statement e writer meta)) exp))

(define (compile-function exp writer meta)
	(define (param-type-loop c)
		(if (or (> c (- (length (cadddr exp)) 1)) (eq? (car (cadddr exp)) 'void)) 
			((writer 'write-byte) #x00) 
			(cond 
				((eq? (cadr (list-ref (cadddr exp) c)) 'int) ((writer 'write-byte) #x00))
				((eq? (cadr (list-ref (cadddr exp) c)) 'double) ((writer 'write-byte) #x01))
				((eq? (cadr (list-ref (cadddr exp) c)) 'string) ((writer 'write-byte) #x02))
				(else (error (cadr exp) "unknown paramater type" (cadr (list-ref (cadddr exp) c))))))
		(if (> c 0) (param-type-loop (- c 1))))

	(define (compile-func-bytecode block bytecode meta)
		(let [(param-num 0)]
			(if (not (eq? (car (cadddr exp)) 'void))
				(for-each 
					(lambda (e) 
						((bytecode 'write-op) 'STOREVAR)
						((bytecode 'write-int32) param-num)
						(((meta 'ctx-vars) 'declare) (car e))
						(set! param-num (+ param-num 1)))
					(cadddr exp)))
			(compile-sequence block bytecode meta)
			((writer 'write-int64) (bytecode 'size))
			((writer 'merge) bytecode)))
		
	;compiling function header:
	;registering function id and creating context for local variables
	(((meta 'ctx-vars) 'create!) (cadr exp))
	(((meta 'func-registry) 'declare) (cadr exp))
	;interned id for function name string from const pool
	((writer 'write-int64) (((meta 'const-pool) 'add) (symbol->string (cadr exp))))
	;local variable count
	((writer 'write-int64) (car (cddr (cdddr exp))))
	;exported -> yes?
	((writer 'write-int64) 0)
	;number of parameters
	(if (eq? (car (cadddr exp)) 'void) 
		((writer 'write-int64) 0)
		((writer 'write-int64) (length (cadddr exp))))
	(if (< 16 (length (cadddr exp))) 
		(error (cadr exp) "no more than 16 parameters allowed")
		(param-type-loop 15))

	;now we're writing compiled function bytecode and putting it into function registry
	(compile-func-bytecode (car (cdr (cdddr exp))) (make-bytecode) meta)
	(((meta 'func-registry) 'definition) (cadr exp) writer)
	(((meta 'ctx-vars) 'destroy!))
	writer)

(define (compile-header writer meta)
	(define (write-magic)
		((writer 'write-sequence) #xBA #xBA))
	(define (write-version)
		((writer 'write-int64) 1))
	(define (write-functions)
		;number of functions
		((writer 'write-int64) ((meta 'func-registry) 'size)))
	(define (write-const-pool)
		;const-pool size
		((writer 'write-int64) (+ ((meta 'const-pool) 'size) 
								  (apply + (map string-length ((meta 'const-pool) 'list)))))
		(for-each
			(lambda (c)
				((writer 'write-string) c)
				((writer 'write-byte) #x00))
			((meta 'const-pool) 'list)))

	(write-magic)
	(write-version)
	(write-const-pool)
	(write-functions))

(define (make-bytecode)
	(let [(writer #f)
		  (get-bv-proc #f)
		  (reloc-table (make-reloc-table))]

	(define (merge-ready-bv bc bv)
		((reloc-table 'merge) (bc 'get-reloc-table) (writer 'size))
		((writer 'write-bytevector) bv))

	(define (merge bc)
		(merge-ready-bv bc (bc 'get-bv)))

	(define (label name)
		((reloc-table 'insert!) name (writer 'size)))

	(define (dispatch m)
		(cond
			((eq? m 'merge) merge)
			((eq? m 'merge-ready-bv) merge-ready-bv)
			((eq? m 'label) label)
			((eq? m 'get-reloc-table) reloc-table)
			((eq? m 'get-bv) (get-bv-proc))
			(else (writer m))))

	(call-with-values open-bytevector-output-port (lambda (p g) (set! writer (make-writer p)) (set! get-bv-proc g)))
	dispatch))


(define (compile-dwarf-file input-file meta)
	(let [(ast (analyze (tokenize (read-file input-file))))]
		(display "compiling file ") (display input-file) (newline)
		(for-each (lambda (node) (compile-function node (make-bytecode) meta)) ast)))

(define (compile-dwarf output-file . input-files)
	(let*  [(meta (make-meta-info))
		   (port (open-file-output-port output-file (file-options no-fail)))
		   (writer (make-writer port))]

		(define (write-function func)
			(let [(reloc (((func 'get-reloc-table) 'get-list)))
				  (bv (func 'get-bv))]

				;assigning proper func-call ids
				(for-each
					(lambda (r)
						(let [(func-id (((meta 'func-registry) 'get-id) (car r)))]
							(for-each
								(lambda (offset) (bytevector-u64-native-set! bv offset func-id))
								(cdr r))))
					reloc)
				((writer 'write-bytevector) bv)))

		(for-each (lambda (file) (compile-dwarf-file file meta)) input-files)
		(compile-header writer meta)
		(for-each (lambda (func) (write-function (cadr func))) ((meta 'func-registry) 'list))
		(close-output-port port)))