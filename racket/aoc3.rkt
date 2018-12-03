#lang racket

(require racket/file)
(require math)

(struct claim (id lOff tOff w h))

(define (parse-id id-str)
  (string-replace id-str "#" ""))

(define (parse-offsets off-str)
  (map string->number (string-split (string-replace off-str ":" "") ",")))

(define (parse-size size-str)
  (map string->number (string-split size-str "x")))

(define (parse-row row)
  (let ([srow (string-split row)])
    (let (
          [id (parse-id (list-ref srow 0))] 
          [offsets (parse-offsets (list-ref srow 2))] 
          [size (parse-size (list-ref srow 3))])
      (claim id (car offsets) (car (cdr offsets)) (car size) (car (cdr size))))))

(define (test-input)
  (file->lines "../inputs/input-3.0.txt"))

(define (input)
  (file->lines "../inputs/input-3.1.txt"))

(define (parsed-test-input)
  (map parse-row (test-input)))

(define (parsed-input)
  (map parse-row (input)))

(define (max-claim-field-by claims max-fn) 
  (apply max (map max-fn claims)))

(define (max-loffset claims)
  (apply max (map claim-lOff claims)))

(define (apply-claim arr claim)
  (let (
        [left-offset (claim-lOff claim)] 
        [top-offset (claim-tOff claim)]
        [width (claim-w claim)]
        [height (claim-h claim)])
      (let ([indices (get-claim-area-indices left-offset top-offset width height)])
        (map (lambda (idx) (apply-claim-portion arr idx)) indices))))

(define (apply-claim-portion arr indices)
  (let ([cur-val (array-ref arr indices)])
    (array-set! arr indices (+ 1 cur-val))))

(define (get-claim-area-indices x y w h)
  (let ([indices (apply append (for/list ([i (in-range x (+ x w))])
                          (for/list ([j (in-range y (+ y h))])
                            (vector i j))))])
    indices))

(define (pairs x y)
  (map (λ (i j) (vector i j)) x y))

(define (part-one input)
  (let ([max-lOff (max-claim-field-by input claim-lOff)]
        [max-tOff (max-claim-field-by input claim-tOff)]
        [max-w (max-claim-field-by input claim-w)]
        [max-h (max-claim-field-by input claim-h)])
    (let (
          [arr 
           (array->mutable-array (build-array 
                                  (quasiquote #((unquote (+ max-lOff max-w)) (unquote (+ max-tOff max-h)))) 
                                  (lambda (x) 0)
                                  ))
           ]
          )
      (map (lambda (claim) (apply-claim arr claim)) input)

      (sum (map (lambda (x) (if (> x 1) 1 0)) (array->list arr)))
)))

