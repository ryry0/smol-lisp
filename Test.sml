; some issues evaluating these two - for some reason multiple things are passed
; to cdr

(assert-equal (eval (q
  (cond
    ((atom? x) (q x-atomic))
    ((atom? y) (q y-atomic))
    ((q true) (q nonatomic))))
        (q ((x (12)) (y 3)))) (q y-atomic))

(assert-equal (eval (q (cond ((atom? x) (q x-atomic))
    ((atom? y) (q y-atomic))
    ((q true) (q nonatomic))))
    (q ((x (1 2))(y (3 4)))))
    (q nonatomic))
