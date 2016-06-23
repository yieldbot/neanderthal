(ns uncomplicate.neanderthal.native.pca
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native.ops :refer [sub! div! mean]]
            [uncomplicate.neanderthal.native.util :refer :all]
            [uncomplicate.neanderthal.native.lapack :refer [svd]]
            [uncomplicate.fluokitten.core :refer [fmap]]))

;; see https://github.com/scikit-learn/scikit-learn/blob/0.17.1/sklearn/decomposition/pca.py

(defn- center
  "Subtract mean of each column."
  [X mu]
  (let [Y (c/copy X)]
    (doall (map sub! (c/cols Y) mu))
    Y))

(defn fit [X & {:keys [whiten num-components]}]
  (let [mu (map mean (c/cols X))
        Y (center X mu)
        m (c/mrows X)
        n (c/ncols X)
        num-components (min (or num-components n) m n)
        {:keys [s u vt]} (svd Y)
        s (c/subvector s 0 num-components)
        vt (c/submatrix vt num-components (c/ncols vt))
        explained-variance (fmap (fn ^double [^double x] (/ (* x x) m)) s)]
    {:mu mu
     :components vt
     :whiten whiten
     :explained-variance explained-variance}))

(defn transform [model X]
  (let [{:keys [mu components whiten explained-variance]} model
        Y (center X mu)
        res (c/mm Y (c/trans components))]
    (when whiten
      (let [ev (fmap (fn ^double [^double x] (Math/sqrt x)) explained-variance)]
        (doall (map div! (c/rows res) (repeat ev)))))
    res))
