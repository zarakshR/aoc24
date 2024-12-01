(module day-one racket
  (require racket/treelist)

  (provide part-one
           part-two)

  (define input-file-name "inputs/1.txt")

  (define (process-input input-port)
    (define (process-input_ lefts rights right-counts)
      (let ([line (read-line input-port)])
        (if (eof-object? line)
            ; TODO: maintain `lefts`, `rights` in sorted order,
            ;   to avoid O(n logn) sorting here
            (values (treelist-sort lefts <)
                    (treelist-sort rights <)
                    right-counts)
            (match-let ([(list l r) (map string->number (string-split line))])
              (process-input_ (treelist-add lefts l)
                              (treelist-add rights r)
                              (hash-update right-counts r add1 0))))))

    (process-input_ (treelist) (treelist) (hash)))

  (define (solve-part-one lefts rights)
    (let ([sum 0])
      (for/list ([l (in-treelist lefts)]
                 [r (in-treelist rights)])
        (set! sum (+ sum (abs (- l r)))))

      sum))

  (define (solve-part-two lefts right-counts)
    (let ([similarity-score 0])
      (for/list ([l (in-treelist lefts)])
        (set! similarity-score
              (+ similarity-score (* l (hash-ref right-counts l 0)))))

      similarity-score))

  ; lefts : treelist?
  ;   the left list of location IDs
  ; rights : treelist?
  ;   the right list of location IDs
  ; right-counts : hash?
  ;   maps all numbers in `rights` to the number of times they appear
  (match-define-values (lefts rights right-counts)
    (call-with-input-file input-file-name process-input))

  ; cache solutions
  (define part-one
    (let ([solution (delay (solve-part-one lefts rights))])
      (thunk (force solution))))

  (define part-two
    (let ([solution (delay (solve-part-two lefts right-counts))])
      (thunk (force solution)))))
