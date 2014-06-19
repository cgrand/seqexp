# Seqexp

Seqexp: regular expressions for sequences (and other sequables)!

Seqexp salient features are:

* linear runtime
* lazy-friendly (doesn't consume the whole seq when possible)
* named groups using `as` (in addition to default groups: `:match` and `:rest`)
* greedy (`*`, `+`, `?`, `repeat`) and reluctant quantifiers (`*?`, `+?`, `??`, `repeat?`)
* short and sweet codebase (~200LOC)

## Usage

Add this dependency to your project.

```clj
[net.cgrand/seqexp "0.1.0"]
```

Require it as:

```clj
(ns playground
  (:require [net.cgrand.seqexp :as se]))
```

And have fun:

```clj
;; match as many odd numbers as possible and ends the match with 7
=> (se/exec
     (se/cat (se/* odd?) 7)
     [1 3 3 7 7 9])

{:rest (9), :match (1 3 3 7 7)}
```

(`exec` returns either nil on failure or a map of
   group names to matched sub-sequences. They are two special groups: :match
   and :rest, corresponding to the matched sub sequence and the rest of the
   input sequence.)

Named groups are introduced with `as`:

```clj
=> (se/exec
        (se/cat (se/as :odds-before-7 (se/* odd?)) 7)
        [1 3 3 7 7 9])

{:rest (9), :odds-before-7 (1 3 3 7), :match (1 3 3 7 7)}
```

What about the difference between greedy and reluctant quantifiers?

```clj
; greedy *
=> (se/exec
        (se/* (se/cat (se/as :odds-before-7 (se/* odd?)) 7))
        [1 3 3 7 1 5 5 7])

{:rest (), :odds-before-7 (1 3 3 7 1 5 5), :match (1 3 3 7 1 5 5 7)}

; reluctant *?
=> (se/exec
        (se/* (se/cat (se/as :odds-before-7 (se/*? odd?)) 7))
        [1 3 3 7 1 5 5 7])

{:rest (), :odds-before-7 (1 5 5), :match (1 3 3 7 1 5 5 7)}
```

In the above example we see that in the first case the inner `*` matches as much as possible while in the second case `*?` matches as little as possible.

Greediness affects only submatches and never changes the whole match.



Now, a more complex example:

```clj
;; match a defn-like body (including name, optional docstring, optional metadata
;; and multiple arities)
=> (def args+body (se/cat vector? (se/* se/_)))

#'playground/args+body

=> (se/exec
     (se/cat
       (se/as :name symbol?)
       (se/? (se/as :docstring string?))
       (se/? (se/as :meta map?))
       (se/|
         (se/as :body args+body)
         (se/as :bodies
           (se/+ (partial se/exec args+body)))))
     '(fn-name "some-doc" {:meta :data}
        ([a] ...)
        ([a b] ...)))

{:rest (), :match (fn-name "some-doc" {:meta :data} ([a] ...) ([a b] ...)),
 :bodies (([a] ...) ([a b] ...)),
 :name (fn-name),
 :docstring ("some-doc"),
 :meta ({:meta :data})}
```

Lazy-friendliness:

```clj
;; proof that the whole infinite seq is not consumed
=> (:match (se/exec (se/* #(< % 10)) (range)))

(0 1 2 3 4 5 6 7 8 9)
```

## Implementation details

Seqexp uses a stackless VM with 5 opcodes, all taking one argument: PRED, JUMP, FORK>, FORK< AND SAVE.

Each thread is only identified by its PC (program counter) and its set of registers (two for each named group). At any time ther can't be more than one thread with the same PC.

This approach is used by Pike and Janson in the sam editor.

### PRED pred-fn
Its argument is a predicate function which gets called on the current element.

### JUMP address
Performs a relative jump, its argument is the relative address.

### FORK> address and FORK< address
Forks the current thread, one will continue to the next instruction while the other will performs a relative jump.

The difference between the two forks is the relative priority of the two resulting threads.

### SAVE register
Saves the current position to the specified register. 


## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
