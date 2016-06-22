(ns uncomplicate.neanderthal.native.ops-test
  (:require [midje.sweet :refer :all]
            [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n :refer [dv]]
            [uncomplicate.neanderthal.native.ops :refer :all]
            [uncomplicate.neanderthal.native.util :refer :all]
            [uncomplicate.neanderthal.native.test-util :refer :all]))

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

(facts "multiplication"
  (let [x (dv (range 10))
        y (dv (range 10 20))]
    (mul x 2) => (dv (range 0 20 2))
    x => (dv (range 10))
    (mul 2 x) => (dv (range 0 20 2))
    x => (dv (range 10))
    (mul x y) => (dv [0 11 24 39 56 75 96 119 144 171])
    x => (dv (range 10))
    y => (dv (range 10 20))))

(facts "destructive multiplication"
  (let [x1 (dv (range 10))
        x2 (dv (range 10))
        x3 (dv (range 10))
        y (dv (range 10 20))]
    (mul! x1 2) => (dv (range 0 20 2))
    x1 => (dv (range 0 20 2))
    (mul! 2 x2) => (dv (range 0 20 2))
    x2 => (dv (range 0 20 2))
    (mul! x3 y) => (dv [0 11 24 39 56 75 96 119 144 171])
    x3 => (dv [0 11 24 39 56 75 96 119 144 171])
    y => (dv (range 10 20))))

(facts "division"
  (let [x (dv (range 1 11))
        y (dv (range 10 20))]
    (div x 2) => (dv (range 1/2 11/2 1/2))
    x => (dv (range 1 11))
    (cmp (div 2 x) (dv [2 1 2/3 1/2 2/5 1/3 2/7 1/4 2/9 1/5])) => true
    x => (dv (range 1 11))
    (cmp (div x y)
         (dv [1/10 2/11 1/4 4/13 5/14 2/5 7/16 8/17 1/2 10/19])) => true
    x => (dv (range 1 11))
    y => (dv (range 10 20))))

(facts "destructive division"
  (let [x1 (dv (range 1 11))
        x2 (dv (range 1 11))
        x3 (dv (range 1 11))
        y (dv (range 10 20))]
    (div! x1 2) => (dv (range 1/2 11/2 1/2))
    x1 => (dv (range 1/2 11/2 1/2))
    (cmp (div! 2 x2) (dv [2 1 2/3 1/2 2/5 1/3 2/7 1/4 2/9 1/5])) => true
    (cmp x2 (dv [2 1 2/3 1/2 2/5 1/3 2/7 1/4 2/9 1/5])) => true
    (cmp (div! x3 y)
         (dv (dv [1/10 2/11 1/4 4/13 5/14 2/5 7/16 8/17 1/2 10/19]))) => true
    (cmp x3 (dv [1/10 2/11 1/4 4/13 5/14 2/5 7/16 8/17 1/2 10/19])) => true
    y => (dv (range 10 20))))

(facts "the mean of a vector"
  (let [v (dv (range 10))]
    (mean v) => 4.5))

(facts "constructing diagonal matrices"
  (diag (range 3)) => (n/dge 3 3 [0 0 0 0 1 0 0 0 2]))
