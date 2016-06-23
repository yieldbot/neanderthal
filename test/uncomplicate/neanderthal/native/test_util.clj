(ns uncomplicate.neanderthal.native.test-util
  (:require [uncomplicate.neanderthal.core :refer [ecount]]
            [uncomplicate.neanderthal.native :refer [dv]]
            [uncomplicate.fluokitten.core :refer [foldmap]]
            [uncomplicate.neanderthal.native.util :refer [rows->dge]]))

(def ^:dynamic *TOL* 1e-8)

(defn ecmp ^double [^double x ^double y]
  (if (<= (Math/abs (- x y)) *TOL*) 1.0 0.0))

(defn cmp [m xss]
  (let [reduce-fn (fn ^double [^double x ^double y] (+ x y))
        xss (if (coll? (first xss)) (rows->dge xss) (dv xss))
        num-matched (foldmap reduce-fn 0 ecmp m xss)]
    (= (int num-matched) (ecount xss))))
