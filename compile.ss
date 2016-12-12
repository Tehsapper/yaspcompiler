(load "analyze.ss")
(load "parse.ss")

(define (tagged? exp tag) (if (eq? (car exp) tag) #t #f))

(define (const? exp)
	(cond 	((tagged? exp 'const-value) #t)
			(else #f)))

(define (lookup-var varname) #t)

(define (make-ast-writer ast output)
	(define (write-sequence . args)
		(if (null? args)
			'()
			(begin
				(write-char (car args) output)
				(write-sequence (cdr args)))))
	(define (dispatch m) 
			(cond ((eq? m 'write-sequence) write-sequence)
				  (else (error "unknown call" m))))
	dispatch)

(define (var? exp)
	(cond 	((null? (lookup-var exp)) #f)
			(else #t)))

(define (assignment? exp) '())

(define (conditional? exp) '())

(define (block? exp) '())

(define (function? exp) '())

(define (compile-const exp target link)
	(make-instruction-sequence '())
	)

(define (compile-var exp target link)
	(make-instruction-sequence '())
	)

(define (compile-assignment exp target link)
	(make-instruction-sequence '())
	)

(define (compile-conditional exp target link)
	(make-instruction-sequence '())
	)

(define (compile-sequence exp target link)
	(make-instruction-sequence '())
	)

(define (compile-header ast writer)
	(define (write-magic)
		((writer 'write-sequence) #xBA #xBA))
	(define (write-version)
		((writer 'write-sequence) #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x01))
	(write-magic)
	(write-version)
	; const pool etc
	)

(define (compile-file input_file output_file)
	(let ((ast (analyze (tokenize (read-file input_file))))
		  (writer (make-ast-writer ast output_file)))
		(define (loop tree) 
			(if (null? tree) 
				(display "end of file") 
				(begin (compile-expression (car tree) writer) (loop (cdr tree)))))
		(compile-header ast writer)
		(loop ast)
	))


(define (compile-expression exp writer)
	(cond 	((const? exp)
				(compile-const exp target link))
			((var? exp)
				(compile-var exp target link))
			((assignment? exp)
				(compile-assignment exp target link))
			((conditional? exp)
				(compile-conditional exp target link))
			((block? exp)
				(compile-block exp writer))
			((function? exp)
				(compile-function exp target link))
			))