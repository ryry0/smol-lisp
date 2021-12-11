(define append
  (lambda (x y)
    (if (null? x) y (cons (car x) (append (cdr x) y)))))

(define pair
  (lambda (x y)
    (list x y)))

(define caar (\ (x) (car (car x))))
(define cadr (\ (x) (car (cdr x))))
(define cdar (\ (x) (cdr (car x))))
(define cddr (\ (x) (cdr (cdr x))))
(define cadar (\ (x) (cadr (car x))))
(define caddr (\ (x) (cadr (cdr x))))
(define caddar (\ (x) (caddr (car x))))

(define pairlis
  (lambda (x y)
    (if (null? x)
      (q ())
      (cons (pair (car x) (car y)) (pairlis (cdr x) (cdr y))))))

(define lookup
    (lambda (x l)
      (if (== (caar l) x) (cadar l) (lookup x (cdr l)))))

(def environment (pairlis (list 1 2 3) (list 4 5 6)))

(define eval
  (lambda (expr env)
    (cond
      ((atom? expr) (lookup expr env))
      ((atom? (car expr))
        (cond
        ((== (car expr) (' car))    (car (eval (cadr expr) env)))
        ((== (car expr) (' cdr))    (cdr (eval (cadr expr) env)))
        ((== (car expr) (' cons))   (cons (eval (cadr expr) env) (eval (caddr expr) env)))
        ((== (car expr) (' atom?))  (atom? (eval (cadr expr) env)))
        ((== (car expr) (' eq?))    (== (eval (cadr expr) env) (eval (caddr expr) env)))
        ((== (car expr) (' quote))  (cadr expr))
        ((== (car expr) (' q))  (cadr expr))
        ((== (car expr) (' cond))   (evcond (cdr expr) env))
        (true                       (eval (cons (lookup (car expr) env) (cdr expr)) env))))
      ((== (caar expr) (' lambda))
        (eval (caddar expr)
          (append (pairlis (cadar expr) (evlis (cdr expr) env)) env))))))


(define evcond
  (lambda (c env)
    (cond
      ((eval (caar c) env) (eval (cadar c) env))
      (true              (evcond (cdr c env))))))

(define evlis
  (lambda (m env)
    (cond
      ((null? m) (q ()))
      (true      (cons (eval (car m) env) (evlis (cdr m) env))))))

(define assert-equal (lambda (x y) (== x y)))

(define assert-not-equal (lambda (x y) (not (assert-equal x y))))

(assert-equal (eval (q x) (q ((x test-value))))
      (q test-value))

(assert-equal (eval (q y) (q ((y (1 2 3)))))
          (q (1 2 3)))

(assert-not-equal (eval (q z) (q ((z ((1) 2 3)))))
          (q (1 2 3)))

(assert-equal (eval (q (quote 7)) (q ()))
          (q 7))

(assert-equal (eval (q (atom? (q (1 2)))) (q ()))
          false)

(assert-equal (eval (q (eq? 1 1)) (q ((1 1))))
          true)

(assert-equal (eval (q (eq? 1 2)) (q ((1 1) (2 2))))
          false)

(assert-equal (eval (q (eq? 1 1)) (q ((1 1))))
          true)

(assert-equal (eval (q (car (q (3 2)))) (q ()))
          (q 3))

(assert-equal (eval (q (cdr (q (1 2 3)))) (q ()))
          (q (2 3)))

(assert-not-equal (eval (q (cdr (q (1 (2 3) 4)))) (q ()))
          (q (2 3 4)))

(assert-equal (eval (q (cons 1 (q (2 3)))) (q ((1 1)(2 2)(3 3))))
          (q (1 2 3)))

(assert-equal (eval (q (cond
  ((atom? x) (q x-atomic))
  ((atom? y) (q y-atomic))
  ((q true) (q nonatomic))))
    (q ((x 1) (y (34))))) (q x-atomic))

(assert-equal (eval (q ((lambda (x) (car (cdr x))) (q (1 2 3 4)))) (q ())) 2)

