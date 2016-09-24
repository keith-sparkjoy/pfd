# pfd

Experimenting with process flow diagrams in Clojure.

## experimental navigation in the REPL

```
(begin) ;; create a simple PFD, show the beginning boundary and the first step
(ok) ;; use this to simulate "doing" a directive step. Moves you to the next step.
(y) ;; use this to answer affirmative to a predicate and move to the next step.
(n) ;; same as (y), just answers in the negative
```

