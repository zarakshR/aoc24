(module day-five racket
  (require racket/treelist)
  (provide load-data)

  (define (middle-of l)
    (let chase ([tortoise l]
                [hare l])
      (if (or (null? hare) (null? (cdr hare)))
          (car tortoise)
          (chase (cdr tortoise) (cdr (cdr hare))))))

  ; topological order of the subgraph of `graph composed of nodes in `nodes
  (define (topological-order graph nodes)
    ; add a dummy node that has edges leading to every other node, this connects
    ; the graph without inducing any incompatible topological orderings
    (define connected-graph (hash-set graph 'dummy (list->set (hash-keys graph))))

    (define (visit node visited)
      (for/fold ([ordering (treelist)]
                 [visited visited]
                 #:result (values (treelist-cons ordering node)
                                  (set-add visited node)))
                ([child (hash-ref connected-graph node empty-sequence)]
                 ; only consider pages mentioned in the update, to avoid cycles
                 #:when (set-member? nodes child)
                 #:unless (set-member? visited child)
                 #:do [(define-values (child-ordering child-visited)
                         (visit child visited))])
        (values (treelist-append child-ordering ordering)
                (set-union child-visited visited))))

    (define-values (ordering _) (visit 'dummy (seteq)))

    ; remove the dummy value; it must be at the head since it is an ancestor of
    ; all nodes
    (cdr (treelist->list ordering)))

  (define (correctly-ordered? top-order update)
    (let check-page ([befores (seteq)]
                     [top-order top-order]
                     [update update])
      (cond
        ; we've seen all pages in the update and none were out of order
        [(null? update) #t]
        ; the current page is out of order
        [(set-member? befores (car update)) #f]
        ; consume pages from top-order, adding them to `befores until and
        ; including the current page
        [else
         (for/fold ([befores befores]
                    [top-order top-order]
                    #:result (check-page befores top-order (cdr update)))
                   ([next top-order]
                    #:final (= (car update) next))
           (values (set-add befores next) (cdr top-order)))])))

  (define (load-data input-port)
    (define-values (rules-text updates-text)
      (splitf-at (port->lines input-port) non-empty-string?))

    (define (build-rules rules-text)
      (define (parse-rule rule-text)
        (map string->number (string-split rule-text "|")))

      (for/fold ([rules-table (hasheq)]) ([rule-text rules-text])
        (match-let ([(list before after) (parse-rule rule-text)])
          (hash-update rules-table
                       before
                       (lambda (afters) (set-add afters after))
                       (seteq)))))

    (define (parse-update update-text)
      (map string->number (string-split update-text ",")))

    (define-values (rules updates)
      (values (build-rules rules-text)
              ; (cdr updates-text) to skip the "" separator
              (map parse-update (cdr updates-text))))

    ; valids : list?
    ;   updates that are in a valid order
    ; fixed-up : list?
    ;   list of invalid updates that have been fixed up
    (define-values (valids fixed-up)
      (for/fold ([valids '()]
                 [fixed-up '()])
                ([update updates])
        (define top-order (topological-order rules (list->set update)))
        (if (correctly-ordered? top-order update)
            (values (cons update valids) fixed-up)
            (values valids (cons top-order fixed-up)))))

    (define part-one
      (let ([solution (delay (for/sum ([i (map middle-of valids)]) i))])
        (thunk (force solution))))

    (define part-two
      (let ([solution (delay (for/sum ([i (map middle-of fixed-up)]) i))])
        (thunk (force solution))))

    (values part-one part-two)))
