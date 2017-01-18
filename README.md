# Seqexp

Seqexp: regular expressions for sequences (and other sequables)!

Seqexp salient features are:

* linear runtime
* lazy-friendly (doesn't consume the whole seq when possible)
* named groups using `as` (in addition to default groups: `:match` and `:rest`)
* greedy (`*`, `+`, `?`, `repeat`) and reluctant quantifiers (`*?`, `+?`, `??`, `repeat?`)
* separated quantifiers (`*'`, `+'` and `repeat'` -- also available in reluctant versions): they take an additional separator expression
* short and sweet codebase (~220LOC)

## Usage

Add this dependency to your project.

```clj
[net.cgrand/seqexp "0.6.2"]
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

Greediness affects only submatches and never changes the whole match: flipping a quantifier from greedy to reluctant (or the other way round) is never going to change what is returned under `:match` and `:rest`, it only affects groups.



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

Seqexp uses a stackless VM with 5 opcodes (`PRED`, `JUMP`, `FORK>`, `FORK<` and `SAVE`), all taking one argument.

The VM is multithreaded (these are not real threads) and all threads are run in lockstep.

The state of a thread consists only of its PC (program counter) and its set of registers (two for each named group). At any time there can't be more than one thread with the same PC.

This approach is used by Pike and Janson in the *sam* editor and is documented [here](http://swtch.com/~rsc/regexp/regexp2.html#ahu74).

### PRED pred-fn
`pred-fn` is a predicate function which gets called on the current element.

If the predicate succeeds, the thread continues to the next instruction and to the next element of the input sequence. When the predicate fails the thread is terminated.

### JUMP address
Performs a relative jump, `address` is the relative address.

### FORK> address and FORK< address
Forks the current thread in two threads, one thread will continue to the next instruction while the other will performs a relative jump to the specified `address`.

The difference between `FORK>` and `FORK<` is the relative priority of the two resulting threads: `FORK>` gives priority to the continuing thread while `FORK<` gives priority to the jumping thread.

It should be noted that the only effect of this *priority* is submatch selection: when a match is found, the highest-priority thread amongst the successful threads gets to pick the submatches (groups). It follows that priority can't change the whole match.

### SAVE register-name
Saves the current position to the specified register.

### Example

For example, `(se/cat (se/* odd?) 7)` compiles down to:

```
FORK> 3
PRED  odd?
JUMP  -2
PRED  #(= 7 %)
```


### Lookahead support
As of 0.6.0, the [Pike and Janson's algorithm](http://swtch.com/~rsc/regexp/regexp2.html#ahu74) has been extended to support lookaheads.

Two versions of the VM now exists (one could do with only one): the top-level VM which is the `grouping` VM and the nested `accepting` VM which is simpler because it ignores registers (ie it doesn't track match or submatches: it just tells whether an accept state has been reached or not). Not having to track submatches mean than thread priority is not needed any more so the whole state of a nested VM is the set of its threads ids.

Previously threads were identified by their PC alone, now with lookahead support, each thread is identified by a pair: its PC and a nested VM state. Nested VMs support lookaheads too (so you can put lookaheads in your lookaheads) so nested thread ids are pairs of PC and a nested VM state.

```
thread-id = [pc, nested-vm-state]
nested-vm-state = set of thread-id
top-level-vm-state = (prioritized) map of thread-id to register-bank 
```

Despite the above definitions _the state space is still bounded_ because the nesting depth is bounded by construction (pathological code exists but can't be produced by a regex/seqexp) so the complexity is still linear with the size of the input.

One new opcode is added: `NLA`.

`NLA addr` spawns thread `[(inc pc) #{}]` in the nested VM while the current VM jumps at `addr`.

The nested VM is used to perform _negative_ lookahead: if it reaches an accept state (PC at the end of the program and empty nested VM state) then the current thread is killed.

A given thread only needs one nested VM even if it traverses several negative lookaheads because:

```
main AND NOT nla1 AND NOT nla2 = main AND NOT (nla1 OR nla2)
```

Surprisingly, _positive lookahead_ comes for free from the decision to support nested lookaheads:

```
main AND pla = main AND NOT (NOT pla)
main AND pla = main AND NOT (true AND NOT pla)
```

So a positive lookahead is just a negative lookahead inside another negative lookahead!

## Changes

### 0.6.0

* Lookahead operators `?!` and `?=`.

### 0.5.1

* Dynamic cycle detection:

```clj
;; before
=> (exec (* (*? _))
    [:c :a :b :b :a :c :a :b :b :b :a])
StackOverflowError   net.cgrand.seqexp/add-thread (seqexp.clj:254)
```

```clj
;; now
=> (exec (* (*? _))
    [:c :a :b :b :a :c :a :b :b :b :a])
{:rest nil, :match (:c :a :b :b :a :c :a :b :b :b :a)}
```

### 0.5.0

* `lift-tree` renamed to `exec-tree` and the original return value is now nested under a `:match` key. Similarly to `exec`, there's now a `:rest` key along the `:match` key.

### 0.4.0

* replacement of the `Register` protocol by the `RegisterBank` one, which allows to implement `lift-tree`
* `lift-tree` which in conjuction with hierarchical group names allows to recreate a tree out of groups:
```clj
=> (se/lift-tree
     (se/* 
       (se/as [:sections]
         (se/cat :h1
           (se/as [:sections :ps]
             (se/+ :p)))))
     [:h1 :p :h1 :p :p :p])

[{:match (:h1 :p :h1 :p :p :p),
  :sections
  [{:match (:h1 :p), :ps [{:match (:p)}]}
   {:match (:h1 :p :p :p), :ps [{:match (:p :p :p)}]}]}]
```

### 0.2.0

* `(repeat n e)` now matches `e` repeated *exactly* n times (and not upto n times)
* separated variants of `*`, `+` and `repeat`: `*'`, `+'` and `repeat'` (reluctant versions: `*'?`, `+'?` and `repeat'?`).

## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
