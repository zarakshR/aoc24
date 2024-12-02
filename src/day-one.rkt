(module day-one racket
  (require racket/treelist)

  (provide part-one
           part-two)

  (define input-file-name "inputs/1.txt")

  ; TODO: use buffers and string views for fast reading
  ;   this currently takes ~ 10 seconds for big boy inputs
  (define (process-lines input-port)
    (define (parse-line line)
      (map string->number (string-split line)))
    (define (increment-count ht k)
      (hash-update ht k add1 0))

    (for/fold ([lefts (treelist)]
               [rights (treelist)]
               [counts (hasheq)]
               #:result (values (treelist-sort lefts <)
                                (treelist-sort rights <)
                                counts))
              ([line (port->lines input-port)])
      (match (parse-line line)
        [(list l r) (values (treelist-add lefts l)
                            (treelist-add rights r)
                            (increment-count counts r))])))

  (define (solve-part-one lefts rights)
    (let ([total-distance 0])
      (for ([l (in-treelist lefts)]
            [r (in-treelist rights)])
        (set! total-distance (+ total-distance (abs (- l r)))))
      total-distance))

  (define (solve-part-two lefts counts)
    (let ([similarity-score 0])
      (for ([l (in-treelist lefts)])
        (set! similarity-score
              (+ similarity-score (* l (hash-ref counts l 0)))))
      similarity-score))

  ; lefts : treelist?
  ;   the left list of location IDs
  ; rights : treelist?
  ;   the right list of location IDs
  ; counts : hash?
  ;   maps all numbers in `rights` to the number of times they appear
  (match-define-values (lefts rights counts)
    (call-with-input-file input-file-name process-lines))

  ; cache solutions
  (define part-one
    (let ([solution (delay (solve-part-one lefts rights))])
      (thunk (force solution))))

  (define part-two
    (let ([solution (delay (solve-part-two lefts counts))])
      (thunk (force solution)))))
