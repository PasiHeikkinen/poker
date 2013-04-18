(ns poker.functional
  (:require [clojure.math.combinatorics :as combo]))

(def suits (range 1 (inc 4)))
(def ranks (range 1 (inc 13)))

(def deck (for [s suits r ranks] {:suit s :rank r}))

(defn all-hands
  []
  (combo/combinations deck 5))

(def all-straight-ranks
  (set (map set (cons (cons 1 (range 10 (inc 13))) (partition 5 1 ranks)))))

(defn straight-ranks?
  [ranks]
  (contains? all-straight-ranks (set ranks)))

(defn flush-suits?
  [suits]
  (apply = suits))

(defn straight-flush-hand?
  [hand]
  (and (straight-ranks? (map :rank hand))
       (flush-suits? (map :suit hand))))

(defn straight-hand?
  [hand]
  (and (straight-ranks? (map :rank hand))
       (not (flush-suits? (map :suit hand)))))

(defn flush-hand?
  [hand]
  (and (flush-suits? (map :suit hand))
       (not (straight-ranks? (map :rank hand)))))

(defn hand-rank-frequency-values
  [hand]
  (frequencies (vals (frequencies (map :rank hand)))))

(defn n-of-a-kind-hand?
  [n hand]
  (= {n 1, 1 (- 5 n)} (hand-rank-frequency-values hand)))

(defn four-of-a-kind-hand?
  [hand]
  (n-of-a-kind-hand? 4 hand))

(defn three-of-a-kind-hand?
  [hand]
  (n-of-a-kind-hand? 3 hand))

(defn full-house-hand?
  [hand]
  (= {3 1, 2 1} (hand-rank-frequency-values hand)))

(defn two-pair-hand?
  [hand]
  (= {2 2, 1 1} (hand-rank-frequency-values hand)))

(defn one-pair-hand?
  [hand]
  (n-of-a-kind-hand? 2 hand))

(defn high-card-hand?
  [hand]
  (and (= {1 5} (hand-rank-frequency-values hand))
       (not (straight-ranks? (map :rank hand)))
       (not (flush-suits? (map :suit hand)))))

(defn all-straight-hands
  []
  (filter straight-hand? (all-hands)))
