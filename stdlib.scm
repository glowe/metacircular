;;
;; Here we define procedures that eliminate the need for providing
;; certain primitives.
;;

(define (map f lst)
  (if (null? lst) '()
      (cons (f (car lst)) (map f (cdr lst)))))

(define (assoc symbol alist)
  (cond ((null? alist) false)
        ((equal? (car (car alist)) symbol) (cdr (car alist)))
        (else (assoc symbol (cdr alist)))))

(define (fold f z lst)
  (if (null? lst) z
      (fold f (f (car lst) z) (cdr lst))))

(define (foldr f z lst)
 (if (null? lst) z
     (f (car lst) (foldr f z (cdr lst)))))

(define (length lst)
  (if (not (list? lst))
      (error "Argument to length is not a list:" lst)
  (fold (lambda (x accum) (+ 1 accum)) 0 lst)))

(define (list . a)
  (foldr cons '() a))

(define (list? a)
  (cond ((null? a) true)
        ((not (pair? a)) false)
        (else (list? (cdr a)))))

(define (append a b)
  (foldr cons b a))

(define (null? x)
  (eq? x '()))

(define (>= a b . c)
  (define (helper x rest)
    (if (null? rest)
        true
        (let ((y (car rest)))
          (and (or (> x y)
                   (= x y))
               (helper y (cdr rest))))))
  (and (or (> a b)
           (= a b))
       (helper b c)))

(define (< a b . c)
  (define (helper x rest)
    (if (null? rest)
        true
        (let ((y (car rest)))
          (and (not (>= x y))
               (helper y (cdr rest))))))
  (and (not (>= a b))
       (helper b c)))
