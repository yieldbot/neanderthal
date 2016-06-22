(ns uncomplicate.neanderthal.native.pca
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.native.ops :refer :all]
            [uncomplicate.neanderthal.native.util :refer :all]
            [uncomplicate.neanderthal.native.svd :refer [svd]]))

;; see https://github.com/scikit-learn/scikit-learn/blob/0.17.1/sklearn/decomposition/pca.py

(defn- center
  "Subtract mean of each column."
  [X mu]
  (let [Y (c/copy X)]
    (doall (map #(sub! %1 %2) (c/cols Y) mu))
    Y))

(defn fit [X]
  (let [mu (map mean (c/cols X))
        Y (center X mu)
        m (c/mrows X)
        n (c/ncols X)
        {:keys [s u vt]} (svd Y)
        ;; TODO: use sparse SVD instead of truncating the result
        vt (c/submatrix vt (min m n) n)]
    {:mu mu :components vt}))

(defn transform [model X]
  (let [{:keys [mu components]} model
        Y (center X mu)]
    (c/mm Y (c/trans components))))
