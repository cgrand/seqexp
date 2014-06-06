(ns regex2.core)

(defprotocol Regex
  (instructions [re]))

(defmacro ^:private asm [& exprs]
  (let [gen (gensym 'gen)
        exprs (partition 2 exprs)]
    `(let [~gen (memoize gensym)]
       (concat
         ~@(map
             (fn [[op arg]]
               (case op
                 (label jump split split!) [[(keyword op) (list gen (keyword arg))]]
                 include `(instructions ~arg)
                 (pred) [[(keyword op) arg]]))
             exprs)))))

(defn star [e]
  (reify Regex
    (instructions [e*]
      (asm
        label   l1
        split   l2
        include e 
        jump    l1
        label   l2))))

(defn cat [e1 e2]
  (reify Regex
    (instructions [e1e2]
      (asm
        include e1
        include e2))))

(defn alt [e1 e2]
  (reify Regex
    (instructions [e1|e2]
      (asm
        split   l1
        include e1
        jump     l2
        label     l1
        include e2
        label     l2))))

(extend-protocol Regex
  Object
  (instructions [f]
    (asm
      pred f)))

(defn link [instructions]
  (let [[insts labels] (reduce (fn [[insts labels] [op arg :as inst]]
                               (if (= :label op)
                                 [insts (assoc labels arg (count insts))]
                                 [(conj insts inst) labels]))
                         [[] {}] instructions)]
    (mapv (fn [[op arg :as inst]]
            (case op
              (:split :split! :jump) [op (labels arg)]
              inst)) insts)))

(defn add-thread [threads pc insts]
  (let [[op arg] (nth insts pc nil)]
    (case op
      :jump (recur threads arg insts)
      :split (-> threads (add-thread (inc pc) insts) (add-thread arg insts))
      :split! (-> threads (add-thread arg insts) (add-thread (inc pc) insts))
      (conj threads pc))))

(defn compile [insts]
  (let [jump-targets (reduce-kv (fn [pcs pc [op arg]]
                               (case op
                                 :jump (conj pcs arg)
                                 :split (conj pcs (inc pc) arg)
                                 :split! (conj pcs arg (inc pc))
                                 (conj pcs (inc pc))))
                     #{0} insts)
        resolved-targets (into {}
                           (for [pc jump-targets]
                             [pc (add-thread #{} pc insts)]))
        reachable-pcs (disj (set (mapcat val resolved-targets))
                        (count insts))
        renum (zipmap (sort reachable-pcs) (range))
        init-threads (->> 0 resolved-targets (map renum) set)
        success (renum (count insts))
        threads (gensym 'threads)
        x (gensym 'x)]
    `(fn [coll#]
       (contains?
         (reduce 
           (fn [cthreads#  ~x]
             (if (and (seq cthreads#) (not (contains? cthreads# ~(count insts))))
               (reduce
                 (fn [~threads pc#]
                   (case (int pc#)
                     ~@(mapcat 
                         (fn [[pc pc']]
                           (let [[op arg] (nth insts pc)]
                             (case op
                               :pred (list pc' `(if (~arg ~x)
                                                   (-> ~threads ~@(map #(list `conj %) (->> pc inc resolved-targets (map renum) set)))
                                                   ~threads)))))
                         renum)))
                 #{} cthreads#)
               (reduced cthreads#)))
          ~init-threads coll#)
         ~success))))

(defn run [insts coll]
  (let [N (count insts)]
    (contains?
      (reduce
        (fn [cthreads x]
          (if (and (seq cthreads) (not (contains? cthreads N)))
            (reduce (fn [threads pc]
                      (let [[op arg] (nth insts pc)]
                        (case op
                          :pred (if (arg x)
                                   (add-thread threads (inc pc) insts)
                                   threads))))
              #{} cthreads)
            (reduced cthreads)))
       (add-thread #{} 0 insts) coll)
      N)))
