(ns poker.examples
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd])
  (:use [poker.logic]))

(comment
  ;; ## Cards
  
  ;; Ranks
  (run* [q] (ranko q))

  ;; Suits
  (run* [q] (suito q))

  ;; Cards
  (count (run* [q] (cardo (lvar) (lvar) q)))

  ;; ## Hands
  
  (run 3 [q] (hando q))

  ;; Total number of hands: 2,598,960 (going through all takes several minutes)
  (time (count (run 10000 [q] (hando q))))

  ;; ## Straight flush (40)
  (count (run* [q] (hand-straight-flusho q (lvar))))
  (run* [q a]
    (full-hand (cards 1 1, 2 1, 3 1, 4 1, 5 a) q)
    (hand-straight-flusho q (lvar)))

  ;; ## Four of a kind (624)
  (time (count (run* [q] (hand-four-of-kindo q (lvar) (lvar)))))
  (run* [q]
    (full-hand (cards 1 1, 1 2, 1 3, 1 4, 2 1) q)
    (hand-four-of-kindo q (lvar) (lvar)))
  
  ;; ## Full house (3744)
  (time (count (run* [q] (hand-full-houseo q (lvar) (lvar)))))
  (run* [q]
    (full-hand (cards 10 1, 10 3, 10 4, 9 2, 9 3) q)
    (hand-full-houseo q (lvar) (lvar)))
  
  ;; ## Flush (5,108)
  (time (count (run* [q] (hand-flusho q (lvar)))))
  (run* [q]
    (full-hand (cards 10 1, 11 1, 9 1, 5 1, 1 1) q)
    (hand-flusho q (lvar)))

  ;; flush suits: 4
  (count (run* [q] (suits-flusho q)))
  
  ;; ranks that are not straight: 1277
  (time (count (run* [q] (non-straight-increasing-rankso q))))

  ;; so number of flush hands is:
  (= 5108 (* 4 1277))

  ;; ## Straight (10,200), takes about 20 seconds
  (time (count (run* [q] (hand-straighto q (lvar)))))
  (run* [q]
    (full-hand (cards 2 1, 3 2, 4 3, 5 4, 1 1) q)
    (hand-straighto q (lvar)))

  ;; straight ranks (10)
  (run* [q] (ranks-straighto q))

  ;; non-flush suits (1020)
  (time (count (run* [q] (== q (lvars 5)) (non-flush-suitso q))))

  ;; number of flush hands:
  (= 1020 (* 10 1020))

  ;; ## Three of a kind (54,912)
  (time (count (run* [q] (hand-three-of-a-kindo q (lvar) (lvar)))))
  (run* [q]
    (full-hand (cards 10 1, 10 3, 10 4, 9 2, 8 3) q)
    (hand-three-of-a-kindo q (lvar) (lvar)))

  ;; ## Two Pair (123,552)
  (time (count (run 10000 [q] (hand-two-pairo q (lvar) (lvar) (lvar)))))
  (run* [q]
    (full-hand (cards 10 1, 10 3, 9 4, 9 2, 8 3) q)
    (hand-two-pairo q (lvar) (lvar) (lvar)))

  ;; ## Pair (1,098,240)
  (time (count (run 10000 [q] (hand-pairo q (lvar) (lvar)))))
  (count (run* [a b]
           (fresh [q]
             (full-hand (cards 10 1, a b, 7 4, 9 2, 6 3) q)
             (hand-pairo q (lvar) (lvar)))))

  ;; ## High Card (1,302,540)
  (time (count (run 1000 [q] (hand-high-cardo q (lvar)))))
  (time (count (run* [q]
                 (full-hand (cards 10 1, 11 3, 1 4, 7 2, 9 3) q)
                 (hand-high-cardo q (lvar))))))
