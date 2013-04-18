(ns poker.logic
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd])
  (:use [poker.logic-util :only [mapg every-memberg every-pairg strictly-monotonically-increasing concatenateo fd-sum]]))

;; ## Cards

(def rank-interval (fd/interval 1 13))
(def suit-interval (fd/interval 1 4))

(defn ranko
  [x]
  (fd/dom x rank-interval))

(defn suito
  [x]
  (fd/dom x suit-interval))

(defne cardo
  [rank suit card]
  ([r s {:rank r :suit s}]
     (ranko r)
     (suito s)))

(defn card<
  [a b]
  (fresh [a-rank a-suit b-rank b-suit]
    (cardo a-rank a-suit a)
    (cardo b-rank b-suit b)
    (conde
     [(fd/== a-rank b-rank)
      (fd/< a-suit b-suit)]
     [(fd/< a-rank b-rank)])))

(defn card-ranko
  [card rank]
  (fresh [suit]
    (cardo rank suit card)))

(defn card-suito
  [card suit]
  (fresh [rank]
    (cardo rank suit card)))

;; ## Hand

(defn hando
  [hand]
  (fresh [a b c d e]
    (== [a b c d e] hand)
    (card< a b)
    (card< b c)
    (card< c d)
    (card< d e)))

(defn cards-rankso
  [cards ranks]
  (mapg card-ranko cards ranks))

(defn hand-rankso
  [hand ranks]
  (all
   (hando hand)
   (cards-rankso hand ranks)))

(defn cards-suitso
  [cards suits]
  (mapg card-suito cards suits))

(defn hand-suitso
  [hand suits]
  (all
   (hando hand)
   (cards-suitso hand suits)))

(defne cards-ranks-suits
  [cards ranks suits]
  ([() () ()])
  ([[c . cardr] [rank . ranksr] [suit . suitsr]]
     (cardo rank suit c)
     (cards-ranks-suits cardr ranksr suitsr)))

;; ## Suit Analysis

(defne suit-counto
  [suits results]
  ([() _] (everyg #(== 0 %) results))
  ([(h . t) _]
     (fresh [ta tb tc td]
       (suit-counto t [ta tb tc td])
       (conde
        [(== h 1)
         (fresh [a]
           (fd/+ ta 1 a)
           (== results [a tb tc td]))]
        [(== h 2)
         (fresh [b]
           (fd/+ tb 1 b)
           (== results [ta b tc td]))]
        [(== h 3)
         (fresh [c]
           (fd/+ tc 1 c)
           (== results [ta tb c td]))]
        [(== h 4)
         (fresh [d]
           (fd/+ td 1 d)
           (== results [ta tb tc d]))]))))

(defn non-flush-suitso
  [suits]
  (fresh [a b c d]
    (fd/in a b c d (fd/interval 0 4))
    (suit-counto suits [a b c d])))

(defn suits-flusho
  [suits]
  (fresh [suit]
    (suito suit)
    (== suits (repeat 5 suit))))

(defn hand-suits-flusho
  [hand]
  (fresh [suits]
    (hand-suitso hand suits)
    (suits-flusho suits)))

;; ## Straight Analysis

(defne ranks-straighto
  [ranks]
  ([[a b c d e]]
     (everyg ranko [a b c d e])
     (conde
      [(fd/+ a 1 b)]
      [(fd/== a 1)
       (fd/== e 13)])
     (fd/+ b 1 c)
     (fd/+ c 1 d)
     (fd/+ d 1 e)))

(defn hand-ranks-straighto
  [hand ranks]
  (all
   (hand-rankso hand ranks)
   (ranks-straighto ranks)))

;; ## Non-Straight Analysis

(defn increasing-rankso
  [ranks]
  (all
   (every-memberg ranko ranks)
   (strictly-monotonically-increasing ranks)))

(defne non-straight-increasing-rankso
  [ranks]
  ([[a b c d e]]
     (everyg ranko [a b c d e])
     (fd/< a b)
     (fd/< b c)
     (fd/< c d)
     (fd/< d e)
     (fresh [ae]
       (fd/+ a 4 ae)
       (fd/< ae e))
     (!= [1 10 11 12 13] [a b c d e])))

;; ## Straight Flush

(defn hand-straight-flusho
  [hand ranks]
  (all
   (hand-ranks-straighto hand ranks)
   (hand-suits-flusho hand)))

;; ## N of a Kind

(defn n-of-kind-ranks
  [n ranks rank kicker-ranks]
  (fresh [preciding-ranks following-ranks]
    (concatenateo [preciding-ranks (repeat n rank) following-ranks] ranks)
    (every-memberg #(fd/< % rank) preciding-ranks)
    (every-memberg #(fd/> % rank) following-ranks)
    (everyg increasing-rankso [preciding-ranks following-ranks])
    (appendo preciding-ranks following-ranks kicker-ranks)))

(defn hand-n-of-kindo
  [n hand rank kicker-ranks]
  (fresh [ranks]
    (hand-rankso hand ranks)
    (n-of-kind-ranks n ranks rank kicker-ranks)))

(defn hand-four-of-kindo
  [hand rank kicker-ranks]
  (hand-n-of-kindo 4 hand rank kicker-ranks))

(defn hand-three-of-a-kindo
  [hand rank kicker-ranks]
  (hand-n-of-kindo 3 hand rank kicker-ranks))

(defn hand-pairo
  [hand rank kicker-ranks]
  (hand-n-of-kindo 2 hand rank kicker-ranks))

;; ## Flush

(defn hand-flusho
  [hand ranks]
  (fresh [suits]
    (hando hand)
    (cards-rankso hand ranks)
    (cards-suitso hand suits)
    (non-straight-increasing-rankso ranks)
    (suits-flusho suits)))

;; ## Straight

(defn hand-straighto
  [hand ranks]
  (fresh [suits]
    (== hand (lvars 5))
    (cards-ranks-suits hand ranks suits)
    (ranks-straighto ranks)
    (non-flush-suitso suits)))

;; ## Full House

(defne ranks-full-houseo
  [ranks three-of-a-kind-rank pair-rank]
  ([[a a a b b] _ _] (everyg ranko [a b]) (== three-of-a-kind-rank a) (== pair-rank b))
  ([[a a b b b] _ _] (everyg ranko [a b]) (== pair-rank a) (== three-of-a-kind-rank b)))

(defn hand-full-houseo
  [x three-of-a-kind-rank pair-rank]
  (fresh [ranks]
    (hand-rankso x ranks)
    (ranks-full-houseo ranks three-of-a-kind-rank pair-rank)))

;; ## Two Pair

(defn ranks-two-pairo
  [ranks lower-pair-rank higher-pair-rank kicker]
  (let [lower-pair (repeat 2 lower-pair-rank)
        higher-pair (repeat 2 higher-pair-rank)
        sole-kicker [kicker]]
    (conde
     [(increasing-rankso [lower-pair-rank higher-pair-rank kicker])
      (== ranks (concat lower-pair higher-pair sole-kicker))]
     [(increasing-rankso [lower-pair-rank kicker higher-pair-rank])
      (== ranks (concat lower-pair sole-kicker higher-pair))]
     [(increasing-rankso [kicker lower-pair-rank higher-pair-rank])
      (== ranks (concat sole-kicker lower-pair higher-pair))])))

(defn hand-two-pairo
  [hand lower-pair-rank higher-pair-rank kicker]
  (fresh [ranks]
    (hand-rankso hand ranks)
    (ranks-two-pairo ranks lower-pair-rank higher-pair-rank kicker)))

;; ## High Card

(defn hand-high-cardo
  [hand ranks]
  (fresh [suits]
    (== hand (lvars 5))
    (== ranks (lvars 5))
    (== suits (lvars 5))
    (non-straight-increasing-rankso ranks)
    (non-flush-suitso suits)
    (cards-ranks-suits hand ranks suits)))

;; ## Helpers

(defn full-hand
  "A goal that takes a collection of cards so that each card is member
  of hand. Cards can have 0 to 5 elements."
  [cards hand]
  (all
   (hando hand)
   (everyg #(membero % hand) cards)))

(defn cards
  [& xs]
  (map (fn [[r s]] {:rank r :suit s}) (partition 2 xs)))
