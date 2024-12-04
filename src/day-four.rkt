(define data (map string->list (file->lines "inputs/4.txt")))

(define (shear-is mat)
    (for/list ([i (length mat)]
               [j (length (car mat))])
      (for/list ([k (+ i 1)])
        (cons (- i k) k))))

(define (shear mat)
  (append (get-shears mat) (reverse (take (get-shears (map reverse (reverse (transpose mat)))) (- (length mat) 1)))))

(define (get-shears mat)
  (define is (shear-is mat))
  (for/list ([is is])
    (for/list ([js is])
      (list-ref (list-ref mat (cdr js)) (car js)))))

(define (transpose mat)
    (if (andmap null? mat)
        '()
    (let ([c1 (map first mat)])
          (cons c1 (transpose (map rest mat))))))

(define (occurs line)
    (match line
      [(list-rest #\S #\A #\M #\X rest) (+ 1 (occurs (cons #\A (cons #\M (cons #\X rest)))))]
      [(list-rest #\X #\M #\A #\S rest) (+ 1 (occurs (cons #\M (cons #\A (cons #\S rest)))))]
      [(list-rest _ rest) (occurs rest)]
      ['() 0]))

(define (occurs2 n line)
    (match line
      [(list-rest #\M #\A #\S rest) (cons n (occurs2 (+ n 1) (cons #\A (cons #\S rest))))]
      [(list-rest #\S #\A #\M rest) (cons n (occurs2 (+ n 1) (cons #\A (cons #\M rest))))]
      [(list-rest _ rest) (occurs2 (+ n 1) rest)]
      ['() '()]))

(define (occurst mat)
  (map (lambda (l) (occurs2 1 l)) mat))

(define (occurs-matrix data)
  (for/list ([l (occurst data)]
             [n (length data)])
    (map (lambda (m) (cons n m)) l)))

(define (left-diag-to-ind h mat)
  (define (a x y)
    (define xa (min (- h 1) x))
    (define ya (if (>= x h) (- x (- h 1)) 0))
    (cons (- xa y) (+ ya y)))

  (map (lambda (k) (map (lambda (l) (a (car l) (cdr l))) k)) mat))

(define (right-diag-to-ind w mat)
  (define (a x y)
    (define xa (if (>= x w)
                   (+ y (- x (- w 1)))
                   y))
    (define ya (+ (- (- w 1) x) xa))
    (cons xa ya))

  (map (lambda (k) (map (lambda (l) (a (car l) (cdr l))) k)) mat))

(define (flatten-once list)
  (match list
    ['() '()]
    [(list-rest x xs) (append x (flatten-once xs))]))

(define (in-list coord l)
  (match l
    ['() #f]
    [(list-rest c xs) (if (and (equal? (car coord) (car c))
                               (equal? (cdr coord) (cdr c)))
                          #t
                          (in-list coord xs))]))

(define (solve-p2 data)
  (define h (length data))
  (define w (length (car data)))
  (define left-shear (map reverse (shear data)))
  (define right-shear (shear (map reverse data)))
  (define left-diag-occurs (filter (lambda (l) (not (null? l))) (occurs-matrix (map reverse (shear data)))))
  (define right-diag-occurs (filter (lambda (l) (not (null? l))) (occurs-matrix right-shear)))
  (define norm-left-occurs (left-diag-to-ind h left-diag-occurs))
  (define norm-right-occurs (right-diag-to-ind w right-diag-occurs))
  (define norm-norm-left-occurs (flatten-once norm-left-occurs))
  (define norm-norm-right-occurs (flatten-once norm-right-occurs))
  (define commoncoord
    (for/list ([c norm-norm-left-occurs]
               #:when (in-list c norm-norm-right-occurs))
      c))

  (length commoncoord))

(define (sumoccurs mat)
  (define (sumoccur mat)
    (for/sum ([i (map occurs mat)]) i))
  (+ (+ (sumoccur mat)
        (sumoccur (transpose mat)))
     (+ (sumoccur (shear mat))
        (sumoccur (shear (map reverse mat))))))
