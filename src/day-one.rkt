(module day-one racket
  (require racket/treelist)

  (provide load-data)

  ; TODO: use buffers and string views for fast reading this currently takes
  ;   ~ 10 seconds for big boy inputs
  (define (process-lines input-port)
    (define (parse-line line)
      (map string->number (string-split line)))
    (define (increment-count ht k)
      (hash-update ht k add1 0))

    (for/fold ([lefts (treelist)]
               [rights (treelist)]
               [counts (hasheq)]
               ; TODO: maintain `lefts`, `rights` in sorted order to avoid
               ;  O(n logn) sorting here
               #:result
               (values (treelist-sort lefts <) (treelist-sort rights <) counts))
              ([line (port->lines input-port)])
      (match (parse-line line)
        [(list l r)
         (values (treelist-add lefts l)
                 (treelist-add rights r)
                 (increment-count counts r))])))

  (define (solve-part-one lefts rights)
    (for/sum ([l (in-treelist lefts)]
              [r (in-treelist rights)])
      (abs (- l r))))

  (define (solve-part-two lefts counts)
    (for/sum ([l (in-treelist lefts)])
      (* l (hash-ref counts l 0))))

  (define (load-data input-port)
    ; lefts : treelist?
    ;   the left list of location IDs
    ; rights : treelist?
    ;   the right list of location IDs
    ; counts : hash?
    ;   maps all numbers in `rights` to the number of times they appear
    (match-define-values (lefts rights counts) (process-lines input-port))

    ; cache solutions
    (define part-one
      (let ([solution (delay (solve-part-one lefts rights))])
        (thunk (force solution))))

    (define part-two
      (let ([solution (delay (solve-part-two lefts counts))])
        (thunk (force solution))))

    (values part-one part-two)))
