(module day-three racket
  (provide load-data)

  ; data Instr = Mul Integer Integer | Do | Dont
  (define (parse source)
    (define instr-re
      (pregexp "(mul)\\((\\d{1,3}),(\\d{1,3})\\)|(do)\\(\\)|(don't)\\(\\)"))
    (define/match (parse-instr instr-text)
      [((list "mul" (app string->number x) (app string->number y) #f #f))
       (list 'mul x y)]
      [((list #f #f #f #f "don't")) 'dont]
      [((list #f #f #f "do" #f)) 'do])

    (map parse-instr (regexp-match* instr-re source #:match-select cdr)))

  (define (interpret-part-one program)
    (for/sum ([i program] #:when (not (or (equal? 'do i) (equal? 'dont i)))
                          #:do [(define mul-result (* (second i) (third i)))])
             mul-result))

  (define (interpret-part-two program)
    (for/fold ([to-skip #f]
               [sum 0]
               #:result sum)
              ([i program])
      (match i
        ['do (values #f sum)]
        ['dont (values #t sum)]
        [(list 'mul x y)
         (if to-skip
             (values to-skip sum)
             (values to-skip (+ sum (* x y))))])))

  ;> ,time (call-with-input-file "inputs/3.txt" load-data)
  ;  cpu time: 4ms = 4ms + 0ms gc; real time: 4ms
  (define (load-data input-port)
    (define data (parse (port->string input-port)))

    ;> ,time (part-one)
    ;  cpu time: 0ms = 0ms + 0ms gc; real time: 0ms
    (define part-one
      (let ([solution (delay (interpret-part-one data))])
        (thunk (force solution))))

    ;> ,time (part-two)
    ;  cpu time: 0ms = 0ms + 0ms gc; real time: 0ms
    (define part-two
      (let ([solution (delay (interpret-part-two data))])
        (thunk (force solution))))

    (values part-one part-two)))
