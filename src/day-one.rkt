(module day-one racket
  (require racket/treelist)

  (provide part-one
           part-two)

  (define input-file-name "inputs/1.txt")

  (define (process-lines input-port)
    (define (parse-line line)
      (map string->number (string-split line)))

    (let process-line ([lefts (treelist)]
                       [rights (treelist)]
                       [counts (hasheq)])
      (match (read-line input-port)
        [(? eof-object?)
         (values (treelist-sort lefts <) (treelist-sort rights <) counts)]
        [(app parse-line (list l r))
         (process-line (treelist-add lefts l)
                       (treelist-add rights r)
                       (hash-update counts r add1 0))])))

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
