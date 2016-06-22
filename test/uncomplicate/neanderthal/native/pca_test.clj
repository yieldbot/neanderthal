(ns uncomplicate.neanderthal.native.pca-test
  (:require [midje.sweet :refer :all]
            [uncomplicate.neanderthal.native :refer :all]
            [uncomplicate.neanderthal.native.test-util :refer :all]
            [uncomplicate.neanderthal.native.pca :refer :all]))

(facts "pca"
  (fact "fit is non-destructive"
    (let [X (dge 2 3 (range 6))]
      (fit X)
      (transform (fit X) X)
      X => (dge 2 3 (range 6))))
  (let [X (dge 2 3 (range 6))
        {:keys [mu components] :as model} (fit X)
        Y (transform model X)]
    (cmp mu [0.5 2.5 4.5]) => true
    (cmp components [[5.77350269e-01 5.77350269e-01 5.77350269e-01]
                     [-8.16496581e-01 4.08248290e-01 4.08248290e-01]]) => true
    (cmp Y [[-8.66025404e-01 -5.55111512e-17]
            [8.66025404e-01 5.55111512e-17]]) => true))
