(define (make-char-at str)
  (let ((len (string-length str)))
    (lambda (pos)
      (if (>= pos len)
	  #f
	  (string-ref str pos)))))

(define (make-char-it str)
  (let ((char-at (make-char-at str))
	(pos 0))
    (lambda ()
      (begin
	(set! pos (+ pos 1))
	(char-at (- pos 1))))))

(define it (make-char-it "hello"))


