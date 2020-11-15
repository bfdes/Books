(define (tree-map func tree)
    (cond
        ((null? tree) tree)
        ((not (pair? tree)) (func tree))
        (else 
            (cons 
                (tree-map func (car tree))
                (tree-map func (cdr tree))))))

(define (square-tree tree)
    (tree-map (lambda (x) (square x)) tree))
