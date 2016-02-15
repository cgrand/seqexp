(ns net.cgrand.seqexp
  "Regular expressions for sequences."
  (:refer-clojure :exclude [+ * repeat +' *'])
  (require [clojure.walk :as walk]))

(defprotocol ^:private Regex
  (instructions [re]))

(defn- link
  "Resolve labels to addresses.
   (idempotent)"
  [instructions]
  (let [[insts labels] (reduce (fn [[insts labels] [op arg :as inst]]
                                 (if (= :label op)
                                   [insts (assoc labels arg (count insts))]
                                   [(conj insts inst) labels]))
                         [[] {}] instructions)]
    (mapv (fn [[op arg :as inst] pc]
            (case op
              (:fork> :fork< :jump) [op (if-let [dest (labels arg)] (- dest pc) arg)]
              inst))
      insts
      (range))))

(defmacro ^:private asm [& exprs]
  (let [gen (gensym 'gen)
        exprs (partition 2 exprs)]
    `(let [~gen (memoize gensym)]
       (concat
         ~@(map
             (fn [[op arg]]
               (case op
                 (label jump fork> fork<) [[(keyword op) (list gen (keyword arg))]]
                 include `(instructions ~arg)
                 (pred save0 save1 sub) [[(keyword op) arg]]))
             exprs)))))

(extend-protocol Regex
  Object
  (instructions [x]
    (instructions #(= x %)))
  clojure.lang.AFn
  (instructions [f]
    (asm
      pred f)))

(defrecord ^:private Pattern [ops]
  Regex
  (instructions [pat] ops))

(defmacro ^:private asmpat [& exprs]
  `(->Pattern (link (asm ~@exprs))))

(defn cat
  "Concatenates several seqexps into one."
  [e & es]
  (->Pattern (mapcat instructions (cons e es))))

(defmacro ^:private decline [decls & forms]
  `(do
     ~@forms
     ~@(walk/postwalk #(decls % %) forms)))

(decline {* *? + +? ? ?? repeat repeat?
          *' *'? +' +'? repeat' repeat'?
          fork> fork< fork< fork> :fork> :fork< :fork< :fork>}
  (defn *
    "Matches its body zero or more times.
     Exists in greedy (*) and reluctant (*?) variants."
    [e & es]
    (asmpat
      label   start
      fork>   end
      include (apply cat e es)
      jump    start
      label   end))

  (defn +
    "Matches its body one or more times.
     Exists in greedy (+) and reluctant (+?) variants."
    [e & es]
    (asmpat
      label   start
      include (apply cat e es)
      fork<   start))

  (defn ?
    "Matches its body zero or once.
     Exists in greedy (?) and reluctant (??) variants."
    [e & es]
    (asmpat
      fork>   end
      include (apply cat e es)
      label   end))

  (defn repeat
    "Matches its body min to max times (inclusive).
     Exists in greedy (repeat) and reluctant (repeat?) variants."
    ([n e]
      (repeat n n e))
    ([min max e]
      (cond
        (pos? min) (cat (apply cat (clojure.core/repeat min e)) (repeat 0 (- max min) e))
        (pos? max) (? e (repeat 0 (dec max) e))
        :else (asmpat))))

  (defn +'
    "Matches its body one or more times separated by sep.
     Exists in greedy (+') and reluctant (+'?) variants."
    [sep e & es]
    (asmpat
      jump    start
      label   loop
      include sep
      label   start
      include (apply cat e es)
      fork<   loop))

  (defn *'
    "Matches its body zero or more times, separated by sep.
     Exists in greedy (*') and reluctant (*'?) variants."
    [sep e & es]
    (? (apply +' sep e es)))

  (defn repeat'
    "Matches its body min to max times (inclusive) separated by sep.
     Exists in greedy (repeat') and reluctant (repeat'?) variants."
    ([n sep e]
      (repeat' n n e))
    ([min max sep e]
      (cond
        (pos? min) (cat e (repeat (dec min) (dec max) (cat sep e)))
        (pos? max) (? (repeat' 1 max sep e))
        :else (asmpat)))))

(defn |
  "Matches either of its arguments."
  ([e] e)
  ([e & es]
    (asmpat
      fork>  l1
      include e
      jump    l2
      label   l1
      include (apply | es)
      label   l2)))

(defn as
  "Like cat but saves the match as a group under the specified name.
   (:match and :rest are reserved names)."
  [name e & es]
  (asmpat
    save0    name
    include (apply cat e es)
    save1    name))

(def _ "Matches anything" (constantly true))

(def ^:private ^:const no-threads [{} []])

(defprotocol Register
  (save0 [reg v])
  (save1 [reg v])
  (fetch [reg]))

(defprotocol RegisterBank
  (store0 [bank id v])
  (store1 [bank id v])
  (fetch-all [bank]))

(defn register [f init]
  (letfn [(reg1 [acc v0]
            (reify Register
              (save0 [reg v0] (reg1 acc v0))
              (save1 [reg v1] (reg2 acc v0 v1))
              (fetch [reg] acc)))
         (reg2 [acc v0 v1]
           (reify Register
             (save0 [reg v0] (reg1 (fetch reg) v0))
             (save1 [reg v1] (reg2 acc v0 v1))
             (fetch [reg]
               (f acc v0 v1))))]
    (reify Register
      (save0 [reg v0] (reg1 init v0))
      (save1 [reg v1] (reg2 init nil v1))
      (fetch [reg] init))))

(defn reduce-occurrences [f init]
  (register (fn [acc [from & s] [to]]
              (f acc (take (- to from) s))) init))

(def last-occurrence (reduce-occurrences (fn [_ x] x) nil))

(def all-occurrences (reduce-occurrences conj []))

(def unmatched-rest (register (fn [_ _ [_ & s]] s) nil))

(extend-protocol RegisterBank
  clojure.lang.APersistentMap
  (store0 [bank id v]
    (assoc bank id (save0 (bank id last-occurrence) v)))
  (store1 [bank id v]
    (assoc bank id (save1 (bank id last-occurrence) v)))
  (fetch-all [bank]
    (reduce-kv (fn [groups name reg] (assoc groups name (fetch reg)))
      bank bank)))

(defn- add-thread [[ctxs pcs :as threads] pc pos registers insts]
  (if (ctxs pc)
    threads
    (let [[op arg] (nth insts pc nil)]
      (case op
        :jump (recur threads (clojure.core/+ pc arg) pos registers insts)
        :fork> (-> threads
                 (add-thread (inc pc) pos registers insts)
                 (add-thread (clojure.core/+ pc arg) pos registers insts))
        :fork< (-> threads
                  (add-thread (clojure.core/+ pc arg) pos registers insts)
                  (add-thread (inc pc) pos registers insts))
        :save0 (recur threads (inc pc) pos
                 (store0 registers arg pos)
                 insts)
        :save1 (recur threads (inc pc) pos
                 (store1 registers arg pos)
                 insts)
        (:pred :sub nil) [(assoc ctxs pc registers) (conj pcs pc)]))))

(defn- run [[insts idx xs [ctxs pcs]]]
  (let [N (count insts)]
    (loop [idx idx, xs (seq xs)
           [ctxs pcs] [(dissoc ctxs N)
                       (filterv #(< % N) pcs)]]
      (if (and xs (seq pcs) (not (contains? ctxs N)))
        (let [[x & xs] xs, idx (inc idx)]
          (recur idx xs
            (reduce (fn [threads pc]
                      (let [[op arg] (nth insts pc)
                            registers (ctxs pc)]
                        (case op
                          :pred (if (arg x)
                                  (add-thread threads (inc pc) (cons idx xs) registers insts)
                                  threads)
                          :sub (if-let [registers (arg x registers)]
                                 (add-thread threads (inc pc) (cons idx xs) registers insts)
                                 threads))))
              no-threads pcs)))
        [insts idx xs [ctxs pcs]]))))

(defn- success [[insts _ _ [ctxs]]]
  (ctxs (count insts)))

(defn- init-state [insts coll regs]
  [insts 0 coll (add-thread no-threads 0 (cons 0 coll) regs insts)])

(defn- longest-match [insts coll regs]
  (loop [state (init-state insts coll regs)
         regs (success state)]
    (let [state (run state)]
      (if-let [regs (success state)]
        (recur state regs)
        regs))))

(defn sub [guard & es]
  (asmpat
    sub (let [insts (link (instructions (apply cat es)))]
          (fn [x regs]
            (when-let [coll (guard x)]
              (longest-match insts coll regs))))))

(defn- groups [bank]
  (when bank (fetch-all bank)))

(defn exec
  "Executes the regular expression, returns either nil on failure or a map of
   group names to matched sub-sequences. They are two special groups: :match
   and :rest, corresponding to the matched sub sequence and the rest of the
   input sequence."
  [re coll & {grps :groups}]
  (groups (longest-match (link (asm
                                 include (as :match re)
                                 save1 :rest))
            coll (into {:rest unmatched-rest} grps))))
