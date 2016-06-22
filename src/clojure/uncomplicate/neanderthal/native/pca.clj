(ns uncomplicate.neanderthal.native.pca
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.native.ops :refer :all]
            [uncomplicate.neanderthal.native.util :refer :all]
            [uncomplicate.neanderthal.native.svd :refer [svd]]
            [uncomplicate.fluokitten.core :refer [fmap]]))

;; see https://github.com/scikit-learn/scikit-learn/blob/0.17.1/sklearn/decomposition/pca.py

(defn- center
  "Subtract mean of each column."
  [X mu]
  (let [Y (c/copy X)]
    (doall (map #(sub! %1 %2) (c/cols Y) mu))
    Y))

(defn fit [X & {:keys [whiten]}]
  (let [mu (map mean (c/cols X))
        Y (center X mu)
        m (c/mrows X)
        n (c/ncols X)
        {:keys [s u vt]} (svd Y)
        ;; TODO: use sparse SVD instead of truncating the result
        vt (c/submatrix vt (min m n) n)
        explained-variance (fmap (fn ^double [^double x]
                                   (/ 1.0 (Math/sqrt (/ (* x x) m)))
                                   #_(/ (* x x) m))
                                 s)]
    {:mu mu
     :components vt
     :whiten whiten
     :explained-variance explained-variance}))

(defn transform [model X]
  (let [{:keys [mu components whiten explained-variance]} model
        Y (center X mu)
        res (c/mm Y (c/trans components))]
    (when whiten
      (doseq [row (c/rows res)]
        (mul! row explained-variance)))
    res))
