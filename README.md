# Seqexp

Seqexp: regular expressions for sequences (and other sequables)!

Seqexp salient features are:

* linear runtime
* lazy-friendly (doesn't consume the whole seq when possible)
* named groups using `as` (in addtion to default groups: `:match` and `:rest`)
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
=> (se/exec
     (se/cat (se/* odd?) 7)
     [1 3 3 7 9])
{:rest (9), :match (1 3 3 7)}
```

A more complex example:

```clj
=> (se/exec
     (se/cat
       (se/as :name symbol?)
       (se/? (se/as :docstring string?))
       (se/? (se/as :meta map?))
       (se/|
         (se/as :body vector? (se/* se/_))
         (se/as :bodies
           (se/+ (partial se/exec (se/cat vector? (se/* se/_)))))))
     '(fn-name "some-doc" {:meta :data}
        ([a] ...)
        ([a b] ...)))
{:rest (), :match (fn-name "some-doc" {:meta :data} ([a] ...) ([a b] ...)),
 :bodies (([a] ...) ([a b] ...)),
 :name (fn-name),
 :docstring ("some-doc"),
 :meta ({:meta :data})}
```

## License

Copyright © 2014 Christophe Grand

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
