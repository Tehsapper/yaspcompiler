(define false #f)
(define true #t)

(load "analyze.ss")
(load "parse.ss")

(define (const? exp)
	(cond 	((number? exp) true)
			((string? exp) true)
			(else false)	
		))

(define (lookup-var varname) #t)

(define (var? exp)
	(cond 	((not (eq? (lookup-var exp) null)) true)
			(else false)	
		))

(define (assignment? exp)
	(cond 	((not (eq? (lookup-var exp) null)) true)
			(else false)	
		))

(define (conditional? exp)
	(cond 	((not (eq? (lookup-var exp) null)) true)
			(else false)	
		))

(define (block? exp)
	(cond 	((not (eq? (lookup-var exp) null)) true)
			(else false)	
		))

(define (application? exp)
	(cond 	((not (eq? (lookup-var exp) null)) true)
			(else false)	
		))

(define (make-instruction-sequence data) (list data))

(define (compile-self-eval exp target link)
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

(define (compile-application exp target link)
	(make-instruction-sequence '())
	)

(define (compile-file input_file output_file)
	(let ((input (read-file input_file))))
	)


(define (compile exp target link)
	(cond 	((const? exp)
				(compile-const exp target link))
			((var? exp)
				(compile-var exp target link))
			((assignment? exp)
				(compile-assignment exp target link))
			((conditional? exp)
				(compile-conditional exp target link))
			((block? exp)
				(compile-sequence (begin-actions exp) target linkage))
			((function? exp)
				(compile-function exp target link))
			((application? exp)
				(compile-application exp target linkage))
			))