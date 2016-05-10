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
                 (pred save0 save1) [[(keyword op) arg]]))
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

(defprotocol RegisterBank
  (save0 [bank id v])
  (save1 [bank id v])
  (fetch [bank]))

(defn register [f init]
  (letfn [(reg1 [acc v0]
            (reify RegisterBank
              (save0 [reg _ v0] (reg1 acc v0))
              (save1 [reg _ v1] (reg2 acc v0 v1))
              (fetch [reg] acc)))
         (reg2 [acc v0 v1]
           (reify RegisterBank
             (save0 [reg _ v0] (reg1 (fetch reg) v0))
             (save1 [reg _ v1] (reg2 acc v0 v1))
             (fetch [reg]
               (f acc v0 v1))))]
    (reify RegisterBank
      (save0 [reg _ v0] (reg1 init v0))
      (save1 [reg _ v1] (reg2 init nil v1))
      (fetch [reg] init))))

(defn reduce-occurrences [f init]
  (register (fn [acc [from & s] [to]]
              (f acc (take (- to from) s))) init))

(def last-occurrence (reduce-occurrences (fn [_ x] x) nil))

(def all-occurrences (reduce-occurrences conj []))

(def unmatched-rest (register (fn [_ _ [_ & s]] s) nil))

(extend-protocol RegisterBank
  nil
  (fetch [m] nil)
  clojure.lang.APersistentMap
  (save0 [m id v]
    (assoc m id (save0 (m id last-occurrence) nil v)))
  (save1 [m id v]
    (assoc m id (save1 (m id last-occurrence) nil v)))
  (fetch [m]
    (reduce-kv (fn [groups name reg] (assoc groups name (fetch reg))) m m)))

(defn hierarchical-bank [f init]
  (letfn [(bank0 []
            (reify RegisterBank
              (save0 [bank path v0] (bank1 {} init v0))
              #_(save1 [bank path v1] (bank2 {} init nil v1))
              (fetch [bank] init)))
          (bank1 [children acc v0']
            (reify RegisterBank
              (save0 [bank path v0]
                (if-some [[x & xs] (seq path)]
                  (bank1 (assoc children x (save0 (or (children x) (bank0)) xs v0)) acc v0')
                  (bank1 {} acc v0)))
              (save1 [bank path v1]
                (if-some [[x & xs] (seq path)]
                  (bank1 (assoc children x (save1 (children x) xs v1)) acc v0')
                  (bank2 children acc v0' v1)))
              (fetch [reg] acc)))
          (bank2 [children acc v0' v1']
            (reify RegisterBank
              (save0 [bank path v0]
                (if-some [[x & xs] (seq path)]
                  (bank1 (assoc children x (save0 (or (children x) (bank0)) xs v0)) acc v0')
                  (bank1 {} (fetch bank) v0)))
              (save1 [bank path v1]
                (if-some [[x & xs] (seq path)]
                  (bank2 (assoc children x (save1 (children x) xs v1)) acc v0' v1')
                  (bank2 children acc v0' v1)))
              (fetch [reg]
                (f acc v0' v1' (reduce-kv (fn [m k bank]
                                            (assoc m k (fetch bank))) children children)))))]
    (reify RegisterBank
      (save0 [bank path v0] (bank1 {} init v0))
      #_(save1 [bank path v1] (bank2 {} init nil v1))
      (fetch [bank] init))))

(defn comp-bank [banks]
  (reify RegisterBank 
    (save0 [bank [k & ks] v]
      (comp-bank (update banks k save0 ks v)))
    (save1 [bank [k & ks] v]
      (comp-bank (update banks k save1 ks v)))
    (fetch [bank]
      (reduce-kv (fn [m k bank]
                   (assoc m k (fetch bank))) banks
        banks))))

(defn tree-bank [mk-node]
  (hierarchical-bank (fn [acc [from & s] [to] children]
                       (conj acc (mk-node (take (- to from) s) children))) []))

(defn- add-thread [threads pc pos registers insts]
  (letfn [(add-thread [[ctxs pcs :as threads] pc pos registers visited-pcs]
            (if (or (ctxs pc) (visited-pcs pc))
              threads
              (let [visited-pcs (conj visited-pcs pc)
                    [op arg] (nth insts pc nil)]
                (case op
                  :jump (recur threads (clojure.core/+ pc arg) pos registers visited-pcs)
                  :fork> (-> threads
                           (add-thread (inc pc) pos registers visited-pcs)
                           (add-thread (clojure.core/+ pc arg) pos registers visited-pcs))
                  :fork< (-> threads
                           (add-thread (clojure.core/+ pc arg) pos registers visited-pcs)
                           (add-thread (inc pc) pos registers visited-pcs))
                  :save0 (recur threads (inc pc) pos
                           (save0 registers arg pos)
                           visited-pcs)
                  :save1 (recur threads (inc pc) pos
                           (save1 registers arg pos)
                           visited-pcs)
                  (:pred nil) [(assoc ctxs pc registers) (conj pcs pc)]))))]
    (add-thread threads pc pos registers #{})))

(defn- run
  "Runs a regex until one of these 3 conditions is met:
 * input is exhausted
 * accept state reached
 * failed (an accept state can't be reached even with additional input)."
  [[insts idx xs [ctxs pcs]]]
  (let [N (count insts)]
    (loop [idx idx, xs (seq xs)
           [ctxs pcs] [(dissoc ctxs N)
                       (filterv #(< % N) pcs)]]
      (if (and xs (seq pcs) (not (contains? ctxs N)))
        (let [[x & xs] xs, idx (inc idx)]
          (recur idx xs
            (reduce (fn [threads pc]
                      ; because of (not (contains? ctxs N)) guard above all pcs refers to :pred
                      (let [[_ pred] (nth insts pc)]
                        (if (pred x)
                          (add-thread threads (inc pc) (cons idx xs) (ctxs pc) insts)
                          threads)))
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

(defn exec
  "Executes the regular expression, returns either nil on failure or a map of
   group names to matched sub-sequences. They are two special groups: :match
   and :rest, corresponding to the matched sub sequence and the rest of the
   input sequence."
  [re coll & {grps :groups}]
  (fetch (longest-match (link (asm
                                    include (as :match re)
                                    save1 :rest))
               coll (into {:rest unmatched-rest} grps))))

(defn- map-registers [re f]
  (->Pattern
    (into [] 
      (map (fn [[op arg :as inst]]
             (case op
               (:save0 :save1) [op (f arg)]
               inst)))
     (instructions re))))

(defn exec-tree
  "Executes the regular expression, returns either nil on failure or a map with two keys:
   :match and :rest. Under :match is found a tree built out of named groups.
   Group names MUST be vectors."
  ([re coll]
    (exec-tree #(assoc %2 :match %1) re coll))
  ([mk-node re coll]
    (fetch
      (longest-match (link (asm
                            include (map-registers (as [] re) #(into [:match] %))
                            save1 [:rest]))
       coll (comp-bank
              {:rest unmatched-rest
               :match (tree-bank mk-node)})))))


