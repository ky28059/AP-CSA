; Given abstraction
(define (make-bintree datum left right)
  (lambda (msg)
    (cond ((= msg 0) datum)
          ((= msg 1) left)
          ((= msg 2) right))))

(define (datum bintree) (bintree 0))
(define (left bintree) (bintree 1))
(define (right bintree) (bintree 2))

(define tet 'toasterhead)
(define (empty-tree? tree) 
  (eq? tree tet))
(define (leaf? bintree)
  (and (not (empty-tree? bintree))
       (empty-tree? (left bintree))
       (empty-tree? (right bintree))))

(define bst
  (make-bintree 15
                (make-bintree 6
                              (make-bintree 2 tet tet)
                              tet)
                (make-bintree 22
                              (make-bintree 17
                                            (make-bintree 16 tet tet)
                                            (make-bintree 19 tet tet))
                              (make-bintree 24 tet tet))))

; Define contains?
(define (contains? tree searchfor)
  (cond {(empty-tree? tree) #f}
        {(< searchfor (datum tree)) (contains? (left tree) searchfor)}
        {(> searchfor (datum tree)) (contains? (right tree) searchfor)}
        {else #t}))

; Define inorder, which turns a binary search tree into a list of ascending numbers
(define (inorder tree)
  (cond {(leaf? tree) (list (datum tree))}
        {(empty-tree? tree) '()}
        {else (append (inorder (left tree)) (list (datum tree)) (inorder (right tree)))}))

; Define count-nodes
(define (count-nodes tree)
  (cond {(leaf? tree) 1}
        {(empty-tree? tree) 0}
        {else (+ (count-nodes (left tree)) 1 (count-nodes (right tree)))}))

; Define square-tree, which squares every datum in a tree
(define (square-tree tree)
  (cond {(leaf? tree) (make-bintree (* (datum tree) (datum tree)) tet tet)}
        {(empty-tree? tree) tet}
        {else (make-bintree (* (datum tree) (datum tree)) (square-tree (left tree)) (square-tree (right tree)))}))

; Define first-n, which takes in a list and n and returns a sublist containing the first n elements of the list
(define (first-n lyst n)
  (if (= n 0) '()
      (append (list (car lyst)) (first-n (cdr lyst) (- n 1)))))

; Define part1, which gets the first half of the list
(define (part1 lyst)
  (first-n lyst (floor (/ (length lyst) 2))))

; Define rest, which gets everything excluded by part1
(define (rest lyst)
  ((repeated cdr (floor (/ (length lyst) 2))) lyst))

; Define middle-datum, which gets the middle element of the list
(define (middle-datum lyst)
  (car (rest lyst)))

; Define part2, which gets everything excluded by part1 and middle-datum
(define (part2 lyst)
  (cdr (rest lyst)))

; Define list->tree, which turns a list of ascending numbers into a weight-balanced binary search tree
(define (list->tree lyst)
  (cond {(empty? lyst) tet}
        {else
         (make-bintree
          (middle-datum lyst)
          (list->tree (part1 lyst))
          (list->tree (part2 lyst)))}))



