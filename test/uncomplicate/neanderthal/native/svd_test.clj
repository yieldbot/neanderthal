(ns uncomplicate.neanderthal.native.svd-test
  (:require [midje.sweet :refer :all]
            [uncomplicate.neanderthal.native :refer :all]
            [uncomplicate.neanderthal.native.test-util :refer :all]
            [uncomplicate.neanderthal.native.svd :refer [svd]]))

(facts "svd"
  (let [A (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5])
        {:keys [s u vt]} (svd A)]
    (cmp u [[-0.70710678 0.70710678] [0.70710678 0.70710678]]) => true
    (cmp s [1.22474487e+00 5.79375858e-17]) => true
    (cmp vt [[5.77350269e-01 5.77350269e-01 5.77350269e-01]
             [-8.16496581e-01 4.08248290e-01 4.08248290e-01]
             [-6.99362418e-17 -7.07106781e-01 7.07106781e-01]]) => true)
  (fact "is non-destructive of A"
    (let [A (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5])]
      (svd A)
      A => (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5]))))
