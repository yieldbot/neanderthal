(ns uncomplicate.neanderthal.native.ops-test
  (:require [midje.sweet :refer :all]
            [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n :refer [dv]]
            [uncomplicate.neanderthal.native.ops :refer :all]
            [uncomplicate.neanderthal.native.util :refer :all]))

(facts "addition"
  (let [x (dv (range 10))
        y (dv (range 10 20))]
    (add x 1) => (dv (range 1 11))
    x => (dv (range 10))
    (add 1 x) => (dv (range 1 11))
    x => (dv (range 10))
    (add x y) => (dv (range 10 30 2))
    x => (dv (range 10))
    y => (dv (range 10 20))))

(facts "destructive addition"
  (let [x1 (dv (range 10))
        x2 (dv (range 10))
        x3 (dv (range 10))
        y (dv (range 10 20))]
    (add! x1 1) => (dv (range 1 11))
    x1 => (dv (range 1 11))
    (add! 1 x2) => (dv (range 1 11))
    x2 => (dv (range 1 11))
    (add! x3 y) => (dv (range 10 30 2))
    x3 => (dv (range 10 30 2))
    y => (dv (range 10 20))))

(facts "destructive addition"
  (let [x1 (dv (range 10))
        x2 (dv (range 10))
        x3 (dv (range 10))
        y (dv (range 10 20))]
    (add! x1 1) => (dv (range 1 11))
    x1 => (dv (range 1 11))
    (add! 1 x2) => (dv (range 1 11))
    x2 => (dv (range 1 11))
    (add! x3 y) => (dv (range 10 30 2))
    x3 => (dv (range 10 30 2))
    y => (dv (range 10 20))))

(facts "subtraction"
  (let [x (dv (range 10))
        y (dv (range 10 20))]
    (sub x 1) => (dv (range -1 9))
    x => (dv (range 10))
    (sub 1 x) => (dv (range 1 -9 -1))
    x => (dv (range 10))
    (sub x y) => (dv (repeat 10 -10))
    x => (dv (range 10))
    y => (dv (range 10 20))))

(facts "destructive subtraction"
  (let [x1 (dv (range 10))
        x2 (dv (range 10))
        x3 (dv (range 10))
        y (dv (range 10 20))]
    (sub! x1 1) => (dv (range -1 9))
    x1 => (dv (range -1 9))
    (sub! 1 x2) => (dv (range 1 -9 -1))
    x2 => (dv (range 1 -9 -1))
    (sub! x3 y) => (dv (repeat 10 -10))
    x3 => (dv (repeat 10 -10))
    y => (dv (range 10 20))))

(facts "the mean of a vector"
  (let [v (dv (range 10))]
    (mean v) => 4.5))

(facts "constructing diagonal matrices"
  (diag (range 3)) => (n/dge 3 3 [0 0 0 0 1 0 0 0 2]))
