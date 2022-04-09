# Readme

A smol lisp written in f# (`Smol.fs`), and a smoller interpreter (`Eval.sml`) written in that lisp.

Based on Nielsen's
[tiddlylisp](https://michaelnielsen.org/ddi/lisp-as-the-maxwells-equations-of-software/)
and Norvig's [lis.py](http://norvig.com/lispy.html). Some other implementations
that were very helpful to reference were [fslispy](https://github.com/jbevain/flispy) and [hasp](https://github.com/aldld/hasp)!

One fun thing that this interpreter is capable of is stateful closures! This
makes code like the following possible:

```scheme
(define make-account
  (lambda (balance)
      (lambda (amt)
            (begin (set! balance (+ balance amt))
                         balance))))

(define account1 (make-account 100.00))
(account1 -20.00) ;returns 80.0
(account1 -10.00) ;returns 70.0

(define account2 (make-account 500.00))
(account2 -20.00) ;returns 480.0
(account2 -20.00) ;returns 460
```

Todo:
* come up with ideas for a language.
* There were some issues parsing the two tests in `Test.sml`. I couldn't tell if
  it was due to the f# interpreter or the lisp interpreter.
