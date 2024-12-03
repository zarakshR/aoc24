;TODO: O(n) part two
(define data
  (map (lambda (s) (map string->number (string-split s)))
       (file->lines "inputs/2.txt")))

(define test-data
  (map (lambda (s) (map string->number (string-split s)))
       (file->lines "sample")))

(define/match (deriv l)
  [((list-rest x '())) '()]
  [((list-rest x y rest)) (cons (- y x) (deriv (cons y rest)))])

(define (back-deriv l) (map - (deriv l)))

(define/match (safe l)
  [((list-rest x y z rest)) '()]
  [((list x y)) '()])

(define (allperms l)
  (for/list ([i (in-range (length l))])
    (match-define-values (left right) (split-at l i))
    (append left (cdr right))))

(define (safe l)
  (define (monotonic l)
    (or (andmap positive? l)
        (andmap negative? l)))
  (define (inrange l)

    (and (andmap (lambda (n) (not (= n 0))) l)
         (andmap (lambda (n) (<= (abs n) 3)) l)))

  (and (monotonic l) (inrange l)))

(define (d2 x)
  (for/list ([i (deriv x)]
             [j (cdr (deriv x))])
    (+ i j)))

(define (diff x)
    (for/list ([i (deriv x)]
               [j (d2 x)])
    (if (< i 0)
        (<= j i)
        (>= j i))))

(define part-one
  (length (filter safe (map deriv data))))

(define (test-working line)
  (not (= 0 (length (filter safe (map deriv (allperms line)))))))

(define part-two
  (length (filter (lambda (n) (not (= n 0))) (map (lambda (l) (length (filter safe (map deriv l)))) (map allperms data)))))

(define (valid n) (and (> n 0) (< n 4)))

;; (define sol (length (filter safe (map deriv data))))
;; (define test-sol (length (filter safe (map deriv test-data))))

(define/match (zip l r)
  [('() '()) '()]
  [((list-rest x xs) (list-rest y ys)) (cons (cons x y) (zip xs ys))])
;; (define inspect (filter (lambda (x) (not (equal? (car (car x)) (cdr (car x))))) (zip (zip fs ds) data)))
