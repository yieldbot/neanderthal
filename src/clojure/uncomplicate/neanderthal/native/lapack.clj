(ns uncomplicate.neanderthal.native.lapack
  (:import [uncomplicate.neanderthal LAPACK]
           [uncomplicate.neanderthal.protocols Block])
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.real :refer [entry]]
            [uncomplicate.neanderthal.block :refer [buffer order]]
            [uncomplicate.neanderthal.protocols :refer [COLUMN_MAJOR]]))

;;;;; Low-level LAPACK Routines ;;;;;

(defn -dgesdd
  ;; NOTE: lwork must be an explicit parameter so we can pass -1 to query for
  ;; work size.
  [jobz m n ^Block a ^Block s ^Block u ^Block vt ^Block work lwork ^Block iwork]
  (let [lda (c/mrows a)
        ldu (c/mrows u)
        ldvt (c/mrows vt)]
    (assert (#{\A \S \N \O} jobu))
    (assert (#{\A \S \N \O} jobvt))
    ;; M >= 0.
    (assert (> m 0))
    ;; N >= 0.
    (assert (> n 0))
    ;; A is DOUBLE PRECISION array, dimension (LDA,N)
    (assert (= (c/ncols a) n))
    ;; LDA >= max(1,M).
    (assert (>= lda (max 1 m)))
    ;; S is DOUBLE PRECISION array, dimension (min(M,N))
    (assert (= (c/dim s) (min m n)))
    ;; U is DOUBLE PRECISION array, dimension (LDU,UCOL)
    ;; UCOL = M if JOBZ = 'A' or JOBZ = 'O' and M < N;
    ;; UCOL = min(M,N) if JOBZ = 'S'.
    ;; if JOBU = 'N' or 'O', U is not referenced.
    (let [ucol (c/ncols u)]
      (case jobz
        \A (assert (= ucol m))
        \S (assert (= ucol (min m n)))
        nil))
    ;; LDU >= 1; if JOBZ = 'S' or 'A' or JOBZ = 'O' and M < N, LDU >= M.
    (if (or (= jobz \A) (= jobz \S) (and (= jobz \O) (< m n)))
      (assert (>= ldu m))
      (assert (>= ldu 1)))
    ;; VT is DOUBLE PRECISION array, dimension (LDVT,N)
    (assert (= (c/ncols vt) n))
    ;; LDVT >= 1; if JOBZ = 'A' or JOBZ = 'O' and M >= N, LDVT >= N;
    ;; if JOBZ = 'S', LDVT >= min(M,N).
    (case jobz
      \A (assert (= ldvt n))
      \S (assert (= ldvt (min m n)))
      \O (assert (or (< m n) (< n ldvt)))
      nil)
    ;; IWORK is INTEGER array, dimension (8*min(M,N))
    (assert (>= (c/dim iwork) (* 8 (min m n))))
    ;; matrices must be column-oriented
    (assert (every? #(= (order %) COLUMN_MAJOR) [a u vt work])
            "Matrices must have COLUMN_MAJOR order")
    (LAPACK/dgesdd_ jobz
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
                    lwork
                    (buffer iwork))))

(defn dgesdd-query [jobz m n ^Block a ^Block s ^Block u ^Block vt ^Block iwork]
  (let [work0 (n/dv 1)]
    (-dgesdd jobz m n a s u vt work0 -1 iwork)
    (int (entry work0 0))))

(defn dgesdd
  ([jobz m n ^Block a ^Block s ^Block u ^Block vt ^Block work ^Block iwork]
   (-dgesdd jobz m n a s u vt work (c/dim work) iwork)
   {:s s :u u :vt vt})
  ([jobz m n ^Block a ^Block s ^Block u ^Block vt]
   ;; IWORK is INTEGER array, dimension (8*min(M,N))
   (let [iwork (n/dv (* 8 (min m n)))
         lwork (dgesdd-query jobz m n a s u vt iwork)
         work (n/dv lwork)]
     (dgesdd jobz m n a s u vt work iwork)))
  ([jobz m n ^Block a]
   (let [s (n/dv (min m n))
         ucol (if (= jobz \S) (min m n) m)
         u (n/dge m ucol)
         ldvt (if (= jobz \S) (min m n) n)
         vt (n/dge ldvt n)]
     (dgesdd jobz m n a s u vt)))
  ([jobz ^Block a]
   (let [m (c/mrows a)
         n (c/ncols a)]
     (dgesdd jobz m n a)))
  ([^Block a]
   (dgesdd \A a)))

(defn -dgesvd
   ;; NOTE: lwork must be an explicit parameter so we can pass -1 to query for
   ;; work size.
   [jobu jobvt m n ^Block a ^Block s ^Block u ^Block vt ^Block work lwork]
   (let [lda (c/mrows a)
         ldu (c/mrows u)
         ldvt (c/mrows vt)]
     (assert (#{\A \S \O \N} jobu))
     (assert (#{\A \S \O \N} jobvt))
     ;; M >= 0.
     (assert (> m 0))
     ;; N >= 0.
     (assert (> n 0))
     ;; A is DOUBLE PRECISION array, dimension (LDA,N)
     (assert (= (c/ncols a) n))
     ;; LDA >= max(1,M).
     (assert (>= lda (max 1 m)))
     ;; S is DOUBLE PRECISION array, dimension (min(M,N))
     (assert (= (c/dim s) (min m n)))
     ;; U is DOUBLE PRECISION array, dimension (LDU,UCOL)
     ;; (LDU,M) if JOBU = 'A' or (LDU,min(M,N)) if JOBU = 'S'.
     ;; if JOBU = 'N' or 'O', U is not referenced.
     (let [ucol (c/ncols u)]
       (case jobu
         \A (assert (= ucol m))
         \S (assert (= ucol (min m n)))
         nil))
     ;; LDU >= 1; if JOBU = 'S' or 'A', LDU >= M.
     (if (#{\A \S} jobu)
       (assert (>= ldu m))
       (assert (>= ldu 1)))
     ;; VT is DOUBLE PRECISION array, dimension (LDVT,N)
     (assert (= (c/ncols vt) n))
     ;;  LDVT >= 1; if JOBVT = 'A', LDVT >= N; if JOBVT = 'S', LDVT >= min(M,N).
     (case jobvt
       \A (assert (= ldvt n))
       \S (assert (= ldvt (min m n)))
       nil)
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
                     lwork)))

(defn dgesvd-query
  [jobu jobvt m n ^Block a ^Block s ^Block u ^Block vt]
  (let [work0 (n/dv 1)]
    (-dgesvd jobu jobvt m n a s u vt work0 -1)
    (int (entry work0 0))))

(defn dgesvd
  ([jobu jobvt m n ^Block a ^Block s ^Block u ^Block vt ^Block work]
   (-dgesvd jobu jobvt m n a s u vt work (c/dim work))
   {:s s :u u :vt vt})
  ([jobu jobvt m n ^Block a ^Block s ^Block u ^Block vt]
   (let [lwork (dgesvd-query jobu jobvt m n a s u vt)
         work (n/dv lwork)]
     (dgesvd jobu jobvt m n a s u vt work)))
  ([jobu jobvt m n ^Block a]
   (let [s (n/dv (min m n))
         ucol (if (= jobu \S) (min m n) m)
         u (n/dge m ucol)
         ldvt (if (= jobvt \S) (min m n) n)
         vt (n/dge ldvt n)]
     (dgesvd jobu jobvt m n a s u vt)))
  ([jobu jobvt ^Block a]
   (let [m (c/mrows a)
         n (c/ncols a)]
     (dgesvd jobu jobvt m n a)))
  ([^Block a]
   (dgesvd \A \A a)))

;;;;; High-level API ;;;;;

(defn svd [a & {:keys [full-matrices]}]
  (let [jobz (if full-matrices \A \S)
        a (c/copy a)]  ;; operation is destructive in A
    (dgesdd jobz a)))

;; TODO: add macros for creating IFn$DD and $Fn$DDD
