(ns uncomplicate.neanderthal.native.util
  (:require [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.core :as c]
            [clojure.string :as str]))

(defn mtype [m]
  (cond (c/vect? m) :vector
        (c/matrix? m) :matrix
        (number? m) :scalar))

(defmulti printm mtype)

(defmethod printm :vector [v]
  (println (format "cvec[%d]" (c/dim v)))
  (println (str/join " " (map #(format "%6.2f" %) v))))

(defmethod printm :matrix [m]
  (println (format "cmat[%d,%d]" (c/mrows m) (c/ncols m)))
  (doseq [r (c/rows m)]
    (println (str/join " " (map #(format "%6.2f" %) r)))))

(defn cols->dge [cols]
  (let [m (count cols)
        n (count (first cols))]
    (assert (every? #(= (count %) n) cols) "Ragged arrays are not supported")
    (n/dge m n (apply concat cols))))

(defn rows->dge [rows]
  (let [m (count rows)
        n (count (first rows))]
    (assert (every? #(= (count %) n) rows) "Ragged arrays are not supported")
    (n/dge m n (apply mapcat list rows))))
