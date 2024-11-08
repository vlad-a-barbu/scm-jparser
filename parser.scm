#|

Vlad A. Barbu - 2 Nov 2024

- a collection of simple text parsing utilities
- developed with ChezScheme (build src @ 95ee804d741e5af9c41f59145b4f42877fa35ae5)

|#



;; yields a pair of (char pos) or (#f len) when the traversal is completed
;; @str - the string value to be traversed
;; @start - the start index
(define (string-make-iterator str start)
  (unless (and (>= start 0) (<= start (string-length str)))
    (error 'string-make-iterator "invalid start position"))
  (let ((len (string-length str))
	(pos start))
    (lambda () (if (= pos len)
		   (cons #f len)
		   (let ((char (string-ref str pos))
			 (char-pos pos))
		     (begin
		       (set! pos (+ pos 1))
		       (cons char char-pos)))))))

;; string-make-iterator demo
(define (demo-string-make-iterator it)
  (let* ((res (it))
	 (char (car res))
	 (pos (cdr res)))
    (if char
	(begin
	  (display (format "[~s] ~s" pos char))
	  (newline)
	  (demo-string-make-iterator it)))
    #f))

(demo-string-make-iterator (string-make-iterator "some random text" 0))



;; accumulates the chars yielded by a string iterator into a state shaped by a given reducer function
;; the accumulation process ends when the reducer does not accept a given character
;; a list of the accumulated state and the last iterated position is returned
;; @iterator - the string iterator
;; @reducer - a function that accepts a state and a character and yields a new state or #f
;; @state - the initial state
(define (string-reduce iterator reducer state)
  (unless (procedure? reducer)
    (error 'string-reduce "reducer must be a procedure"))
  (let* ((res (iterator))
	 (char (car res))
	 (pos (cdr res)))
    (if char
	(let ((next-state (reducer state char)))
	  (if next-state
	      (string-reduce iterator reducer next-state)
	      (cons state pos)))
	(cons state pos))))



;; given a string, a list of reducers and an initial state
;; returns (accumulated-state . next-position) on success and (initial state . 0) on failure
;; @str - the string value to be traversed
;; @reducers - the list of reducers
;; @state - the initial state
(define (string-reduce-first str reducers state)
  (unless (not (null? reducers))
    (error 'string-reduce-first "at least one reducer is required"))
  (let reduce ((pos 0)
	       (reducers reducers)
	       (state state))
    (let* ((res (string-reduce (string-make-iterator str pos)
			       (car reducers)
			       state))
	   (new-pos (cdr res)))
      (cond [(or (> new-pos pos)
		 (null? (cdr reducers))) res]
	    [else (reduce pos (cdr reducers) state)]))))

(define (whitespace? c)
  ((char-one-of '(#\newline #\space #\tab)) c))

(define (whitespace-reducer state char)
  (if (whitespace? char)
      state
      #f))

(define (numeric-reducer state char)
  (if (char-numeric? char)
      (string-append state (string char))
      #f))
