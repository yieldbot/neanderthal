(ns uncomplicate.neanderthal.native.lapack-test
  (:require [midje.sweet :refer :all]
            [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :refer :all]
            [uncomplicate.fluokitten.core :refer [foldmap]]
            [uncomplicate.neanderthal.native.lapack :refer :all]))

(def ^:dynamic *TOL* 1e-8)

(defn ecmp ^double [^double x ^double y]
  (if (<= (Math/abs (- x y)) *TOL*) 1.0 0.0))

(defn rows->dge [rows]
  (let [m (count rows)
        n (count (first rows))]
    (assert (every? #(= (count %) n) rows) "Ragged arrays are not supported")
    (dge m n (apply mapcat list rows))))

(defn cmp [m xss]
  (let [reduce-fn (fn ^double [^double x ^double y] (+ x y))
        xss (if (coll? (first xss)) (rows->dge xss) (dv xss))
        num-matched (foldmap reduce-fn 0 ecmp m xss)]
    (= (int num-matched) (c/ecount xss))))

;; see https://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mkl_lapack_examples/dgesdd_ex.c.htm

(facts "dgesdd"
  (let [a (dge 6 4 [ 7.52, -0.76,  5.13, -4.75,  1.33, -2.40,
                    -1.10,  0.62,  6.62,  8.52,  4.91, -6.77,
                    -7.95,  9.34, -5.66,  5.75, -5.49,  2.34,
                     1.08, -7.10,  0.87,  5.30, -3.52,  3.95])
        {:keys [s u vt]} (dgesdd \S a)]
    (binding [*TOL* 1e-2]
      (cmp s [18.37  13.63  10.85   4.49]) => true
      (cmp u [[-0.57   0.18   0.01   0.53]
              [ 0.46  -0.11  -0.72   0.42]
              [-0.45  -0.41   0.00   0.36]
              [ 0.33  -0.69   0.49   0.19]
              [-0.32  -0.31  -0.28  -0.61]
              [ 0.21   0.46   0.39   0.09]]) => true
      (cmp vt [[-0.52  -0.12   0.85  -0.03]
               [ 0.08  -0.99  -0.09  -0.01]
               [-0.28  -0.02  -0.14   0.95]
               [ 0.81   0.01   0.50   0.31]]) => true)))

;; see https://software.intel.com/sites/products/documentation/doclib/mkl_sa/11/mkl_lapack_examples/dgesvd_ex.c.htm

(facts "dgesvd"
  (let [a (dge 6 5 [8.79,  6.11, -9.15,  9.57, -3.49,  9.84,
                    9.93,  6.91, -7.93,  1.64,  4.02,  0.15,
                    9.83,  5.04,  4.86,  8.83,  9.80, -8.99,
                    5.45, -0.27,  4.85,  0.74, 10.00, -6.02,
                    3.16,  7.98,  3.01,  5.80,  4.27, -5.31])
        {:keys [s u vt]} (dgesvd \A \A a)]
    (binding [*TOL* 1e-2]
      (cmp s [27.47  22.64   8.56   5.99   2.01]) => true
      (cmp u [[-0.59   0.26   0.36   0.31   0.23  0.55]
              [-0.40   0.24  -0.22  -0.75  -0.36  0.18]
              [-0.03  -0.60  -0.45   0.23  -0.31  0.54]
              [-0.43   0.24  -0.69   0.33   0.16 -0.39]
              [-0.47  -0.35   0.39   0.16  -0.52 -0.46]
              [ 0.29   0.58  -0.02   0.38  -0.65  0.11]]) => true
      (cmp vt [[-0.25  -0.40  -0.69  -0.37  -0.41]
               [0.81    0.36  -0.25  -0.37  -0.10]
               [-0.26   0.70  -0.22   0.39  -0.49]
               [0.40   -0.45   0.25   0.43  -0.62]
               [-0.22   0.14   0.59  -0.63  -0.44]]) => true)))

(facts "svd"
  (let [A (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5])
        {:keys [s u vt]} (svd A :full-matrices true)]
    (cmp s [1.22474487e+00 5.79375858e-17]) => true
    (cmp u [[-0.70710678 0.70710678] [0.70710678 0.70710678]]) => true
    (cmp vt [[5.77350269e-01 5.77350269e-01 5.77350269e-01]
             [-8.16496581e-01 4.08248290e-01 4.08248290e-01]
             [-6.99362418e-17 -7.07106781e-01 7.07106781e-01]]) => true)
  (fact "is non-destructive of A"
    (let [A (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5])]
      (svd A)
      A => (dge 2 3 [-0.5 0.5 -0.5 0.5 -0.5 0.5]))))

(facts "lstsq"
  (let [a (dge 4 5 [ 0.12, -6.91, -3.33,  3.97,
                    -8.19,  2.22, -8.94,  3.33,
                     7.69, -5.12, -6.72, -2.74,
                    -2.26, -9.08, -4.40, -7.92,
                    -4.71,  9.96, -9.98, -3.20])
        b (dge 5 3 [ 7.30,  1.33,  2.68, -9.62,  0.00,
                     0.47,  6.58, -1.71, -0.79,  0.00,
                    -6.28, -3.42,  3.46,  0.41,  0.00])
        {:keys [x s rank]} (lstsq a b)]
    rank => 4
    (binding [*TOL* 1e-2]
      (cmp x [[-0.69  -0.24   0.06]
              [-0.80  -0.08   0.21]
              [ 0.38   0.12  -0.65]
              [ 0.29  -0.24   0.42]
              [ 0.29   0.35  -0.30]]) => true
      (cmp s [18.66  15.99  10.01   8.51]) => true)))
