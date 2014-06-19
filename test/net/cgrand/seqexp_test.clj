(ns net.cgrand.seqexp-test
  (:use clojure.test)
  (:require [net.cgrand.seqexp :as se]))

(deftest values
  (are [se s m]
    (= (:match (se/exec se s)) m)
    3 [3] [3]
    3 [3 3] [3]
    3 [3 4] [3]
    3 [4 3] nil))

(deftest predicates
  (are [se s m]
    (= (:match (se/exec se s)) m)
    odd? [3] [3]
    odd? [3 3] [3]
    odd? [3 4] [3]
    odd? [4 3] nil))

(deftest star
  (are [se s m]
    (= (:match (se/exec se s)) m)
    (se/* odd?) [3] [3]
    (se/* odd?) [3 3] [3 3]
    (se/* odd?) [3 4] [3]
    (se/* odd?) [4 3] ()))

(deftest plus
  (are [se s m]
    (= (:match (se/exec se s)) m)
    (se/+ odd?) [3] [3]
    (se/+ odd?) [3 3] [3 3]
    (se/+ odd?) [3 4] [3]
    (se/+ odd?) [4 3] nil))

(deftest optional
  (are [se s m]
    (= (:match (se/exec se s)) m)
    (se/? odd?) [3] [3]
    (se/? odd?) [3 3] [3]
    (se/? odd?) [3 4] [3]
    (se/? odd?) [4 3] ()))

(deftest choice
  (are [se s m]
    (= (:match (se/exec se s)) m)
    (se/| odd? 4) [3] [3]
    (se/| odd? 4) [3 3] [3]
    (se/| odd? 4) [3 4] [3]
    (se/| odd? 4) [4 3] [4]
    (se/| odd? 4) [2 3] nil))

(deftest cat
  (are [se s m]
    (= (:match (se/exec se s)) m)
    (se/cat odd? 4) [3] nil
    (se/cat odd? 4) [3 3] nil
    (se/cat odd? 4) [3 4] [3 4]
    (se/cat odd? 4) [4 3] nil))

(deftest repetition
  (doseq [n (range 0 3)]
    (is (nil? (se/exec (se/repeat 3 7 :x) (repeat n :x)))))
  (doseq [n (range 3 8)]
    (is (= ((juxt :match :rest) (se/exec (se/repeat 3 7 :x) (repeat n :x)))
          [(repeat n :x) ()])))
  (doseq [n (range 8 16)]
    (is (= ((juxt :match :rest) (se/exec (se/repeat 3 7 :x) (repeat n :x)))
          [(repeat 7 :x) (repeat (- n 7) :x)]))))

(deftest reluctancy
  (are [se s m m']
    (= ((juxt :a :b) (se/exec se s)) [m m'])
    (se/cat (se/as :a (se/+ 3)) (se/as :b (se/+ 3))) [3 3 3 3] [3 3 3] [3]
    (se/cat (se/as :a (se/+? 3)) (se/as :b (se/+ 3))) [3 3 3 3] [3] [3 3 3]))


