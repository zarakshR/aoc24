;TODO: hand-write parser
(module day-three racket
  (provide load-data)

  (define (parse line)
    (define instr-re (pregexp "mul\\(\\d{1,3},\\d{1,3}\\)|do\\(\\)|don't\\(\\)"))

    (define/match (extract-instr ins)
      [((regexp #rx"^don")) 'dont]
      [((regexp #rx"^do")) 'do]
      [((regexp #rx"^mul"))
       (let ([nums (regexp-match* #px"\\d{1,3}" ins)])
         (cons 'mul (map string->number nums)))])

    (map extract-instr (regexp-match* instr-re line)))

  (define (interpret instrs)
    (let loop ([enabled #t]
               [instrs instrs]
               [sum 0])
      (match instrs
        ['() sum]
        [(list-rest 'do cdr) (loop #t cdr sum)]
        [(list-rest 'dont cdr) (loop #f cdr sum)]
        [(list-rest (list 'mul x y) cdr)
         (loop enabled cdr (if enabled (+ sum (* x y)) sum))])))

  ;> ,time (call-with-input-file "inputs/3.txt" load-data)
  ;  cpu time: 2ms = 2ms + 0ms gc; real time: 2ms
  (define (load-data input-port)
    (define data (foldr append '() (map parse (port->lines input-port))))

    ;> ,time (part-one)
    ;  cpu time: 0ms = 0ms + 0ms gc; real time: 0ms
    (define part-one
      (let* ([mul? (lambda (ins) (and (list? ins) (equal? 'mul (car ins))))]
             [solution (delay (interpret (filter mul? data)))])
        (thunk (force solution))))

    ;> ,time (part-two)
    ;  cpu time: 0ms = 0ms + 0ms gc; real time: 0ms
    (define part-two
      (let ([solution (delay (interpret data))])
        (thunk (force solution))))

    (values part-one part-two)))
