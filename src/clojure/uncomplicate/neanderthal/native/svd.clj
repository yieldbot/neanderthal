(ns uncomplicate.neanderthal.native.svd
  (:import [uncomplicate.neanderthal LAPACK]
           [uncomplicate.neanderthal.protocols Block])
  (:require [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.real :refer [entry]]
            [uncomplicate.neanderthal.block :refer [buffer order]]
            [uncomplicate.neanderthal.protocols :refer [COLUMN_MAJOR]]))

;; see http://www.netlib.org/lapack/explore-html/d1/d7e/group__double_g_esing.html#ga84fdf22a62b12ff364621e4713ce02f2
;; see https://software.intel.com/en-us/node/521150#A426CAEA-88A2-46D0-80E7-C68776C6E0B2

;; low-level
(defn dgesvd [jobu jobvt m n ^Block a lda ^Block s ^Block u ldu ^Block vt ldvt
              ^Block work lwork]
  ;; matrices must be column-oriented
  (assert (every? #(= (order %) COLUMN_MAJOR) [a u vt work])
          "Matrices must have COLUMN_MAJOR order")
  (LAPACK/dgesvd_ jobu
                  jobvt
                  m
                  n
                  (buffer a)
                  lda
                  (buffer s)
                  (buffer u)
                  ldu
                  (buffer vt)
                  ldvt
                  (buffer work)
                  lwork))

;; NOTE: Modifies s, u, and vt. Destroys a.
(defn svd! [a s u vt]
  (let [jobu \A
        jobvt \A
        m (c/mrows a)
        n (c/ncols a)
        lda m
        ldu m
        ldvt n
        ;; Query and allocate the optimal workspace
        work0 (n/dv 1)
        _ (dgesvd jobu jobvt m n a lda s u ldu vt ldvt work0 -1)
        lwork (int (entry work0 0))
        work (n/dv lwork)
        ;; Compute SVD
        info (dgesvd jobu jobvt m n a lda s u ldu vt ldvt work lwork)]
    (assert (not (pos? info)) "The algorithm computing SVD failed to converge.")))

;; TODO: support sparse SVD

(defn svd [a]
  (let [m (c/mrows a)
        n (c/ncols a)
        ldu m
        ldvt n
        a (c/copy a)  ;; dgesvd is destructive in a
        s (n/dv (min m n))
        u (n/dge ldu m)
        vt (n/dge ldvt n)]
    (svd! a s u vt)
    {:s s :u u :vt vt}))
