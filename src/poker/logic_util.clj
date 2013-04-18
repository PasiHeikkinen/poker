(ns poker.logic-util
  (:refer-clojure :exclude [==])
  (:use clojure.core.logic)
  (:require [clojure.core.logic.fd :as fd]))

(defne mapg
  [g x y]
  ([_ () ()])
  ([_ (xh . xt) (yh . yt)]
     (g xh yh)
     (mapg g xt yt)))

(defne every-memberg
  [f xs]
  ([_ ()])
  ([_ [h . t]]
     (f h)
     (every-memberg f t)))

(defne every-pairg
  [f xs]
  ([_ ()])
  ([_ [x]])
  ([_ [x . xst]]
     (fresh [y]
       (firsto xst y)
       (f x y))
     (every-pairg f xst)))

(defn strictly-monotonically-increasing
  [xs]
  (every-pairg fd/< xs))

(defne concatenateo
  [colls result]
  ([() ()])
  ([[h . t]]
     (fresh [a]
       (appendo h a result)
       (concatenateo t a))))

(defne fd-sum
  [xs result]
  ([() _] (== result 0))
  ([(h . t) _]
     (fresh [a]
       (fd-sum t a)
       (fd/+ a h result))))
