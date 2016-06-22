(ns uncomplicate.neanderthal.native.ops
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.native.util :refer [mtype]]
            [uncomplicate.fluokitten.core :refer [fmap fmap!]]))

;; TODO: implement (count) for RealBlockVector

(defmulti add! (fn [a1 a2] [(mtype a1) (mtype a2)]))

(defmulti sub! (fn [a1 a2] [(mtype a1) (mtype a2)]))

(defmulti add (fn [a1 a2] [(mtype a1) (mtype a2)]))

(defmulti sub (fn [a1 a2] [(mtype a1) (mtype a2)]))

(defmethod add! [:vector :scalar] [v a]
  (let [f (fn ^double [^double x] (+ x a))]
    (fmap! f v)
    v))

(defmethod add! [:scalar :vector] [a v]
  (add! v a))

(defmethod add! [:vector :vector] [v1 v2]
  (let [f (fn ^double [^double x ^double y] (+ x y))]
    (fmap! f v1 v2)
    v1))

(defmethod sub! [:vector :scalar] [v a]
  (add! v (- a)))

(defmethod sub! [:scalar :vector] [a v]
  (let [f (fn ^double [^double x] (- a x))]
    (fmap! f v)
    v))

(defmethod sub! [:vector :vector] [v1 v2]
  (let [f (fn ^double [^double x ^double y] (- x y))]
    (fmap! f v1 v2)
    v1))

(defmethod add [:vector :scalar] [v a]
  (add! (c/copy v) a))

(defmethod add [:scalar :vector] [a v]
  (add! (c/copy v) a))

(defmethod add [:vector :vector] [v1 v2]
  (add! (c/copy v1) v2))

(defmethod sub [:vector :scalar] [v a]
  (sub! (c/copy v) a))

(defmethod sub [:scalar :vector] [a v]
  (sub! a (c/copy v)))

(defmethod sub [:vector :vector] [v1 v2]
  (sub! (c/copy v1) v2))

(defn mean
  "Mean of entries in v."
  [v]
  (/ (c/sum v) (c/dim v)))

(defn diag
  "Construct NxN diagonal matrix with elements taken from N-vector v."
  [v]
  (let [n (if (c/vect? v) (c/dim v) (count v))
        zeros (repeat n 0)
        xs (reduce #(concat %1 zeros [%2]) (take 1 v) (rest v))]
    (n/dge n n xs)))
