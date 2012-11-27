### Compatibility

This interpreter will run on mzscheme v372. 

### Design

I used 2 ideas from Sussman and Abelson's Meta-Circular Evaluator (Structure and Interpretation of Computer Programs): expressing
the environment as stack/list of frames and using syntactic
transformations. The first idea is necessary to properly implement the rules of nested defines properly (i.e., you should not be able to redefine a name in your existing scope). The second idea helps to reduce the amount of code necessary to support special forms. Like Sussman and Abelson's interpreter, my interpreter translates `cond` expressions to equivalent `if` expressions. I extend this idea to both `letrec`, `let*`, and even `and` and `or`.

I've also made a conscious effort to limit the number of primitives registered in the global environment; instead, many of
these procedures can be found in a standalone file called `stdlib.scm`. As such, it may be necessary to load this file before attempting to use any of the normal builtins. Here's a list of the procedures `stdlib.scm` provides:

 * `<`
 * `>=`
 * `append`
 * `assoc`
 * `fold`
 * `foldr`
 * `length`
 * `list`
 * `list?`
 * `map`
 * `null?`

__IMPORTANT:__ Because `interpreter.scm` needs `stdlib.scm` to
bootstrap itself, it loads that file automatically. However, if
you'd like to use any of the procedures defined in `stdlib.scm` in a standalone fashion (e.g., after starting the repl), then you'll need to load it yourself.

### Demo instructions

Here's a sample session of how to exercise the interpreter.


    ;; Load interpreter into Scheme
    (load "interpreter.scm")
    ;; Start up the read-eval-print-loop
    (repl)
    
    ;; Now we test the macro facility
    ;; Load stdlib.scm for "list" procedure
    (load "stdlib.scm")
    
    ;; Define the macro example from the course website
    (define-macro (strange-mult a b) (list '* a (list '- b 1)))
    
    ;; Set up some variables that will be necessary for evaulating our
    ;; macro
    (define y 3) (let ((x 5)) (strange-mult (+ 3 2) (+ x y)))
    
    
    ;; Now let's test self-evaluation
    (load "interpreter.scm")
    ;; ... Wait a couple of minutes
    
    ;; Change the prompt for this invocation of repl, so that it's
    ;; easier to distinguish which interpreter we are really in
    (define *prompt* "*meta* ]=> ")
    
    ;; Now start the read-eval-printloop
    (repl)
    
    ;; Load stdlib.scm again, because we're going to test something
    standalone. Load takes a while (be patient)
    
    (load "stdlib.scm")
    
    ;; After load finishes, we define a factorial function
    
    (define (fac x)
      (if (= x 0)
          1
          (* x (fac (- x 1)))))
    
    ;; Now let's test it. This will take about a minute or two.
    
    (fac 4)
    
    ;; Exit second interpreter
    
    (exit)
    
    ;; Exit first interpreter
    
    (exit)
