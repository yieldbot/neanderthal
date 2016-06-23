(ns uncomplicate.neanderthal.native.lapack
  (:import [uncomplicate.neanderthal LAPACK]
           [uncomplicate.neanderthal.protocols Block]
           [java.nio ByteBuffer])
  (:require [uncomplicate.neanderthal.core :as c]
            [uncomplicate.neanderthal.native :as n]
            [uncomplicate.neanderthal.real :refer [entry]]
            [uncomplicate.neanderthal.block :refer [buffer order]]
            [uncomplicate.neanderthal.protocols :refer [COLUMN_MAJOR]]
            [vertigo.bytes :refer [direct-buffer]]))

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
    (assert (pos? m))
    ;; N >= 0.
    (assert (pos? n))
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
    (let [info (LAPACK/dgesdd_ jobz
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
                               (buffer iwork))]
      (cond
        (pos? info) (throw (RuntimeException.
                            "Algorithm failed to converge"))
        (neg? info) (throw (RuntimeException.
                            "Unexpected error due to invalid LAPACK argument"))
        :else info))))

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
     (assert (pos? m))
     ;; N >= 0.
     (assert (pos? n))
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
     (let [info (LAPACK/dgesvd_ jobu
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
                                lwork)]
       (cond
         (pos? info) (throw (RuntimeException.
                             "Algorithm failed to converge"))
         (neg? info) (throw (RuntimeException.
                             "Unexpected error due to invalid LAPACK argument"))
         :else info))))

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

(defn -dgelsd
  ([m n nrhs a b s rcond ^ByteBuffer rank work lwork iwork]
   ;; NOTE: rank is a bytebuffer holding a single int. is there a cleaner way to
   ;; return multiple values (in this case, rank and info)?
   (let [lda (c/mrows a)
         ldb (c/mrows b)]
     ;; M >= 0.
     (assert (pos? m 0))
     ;; N >= 0.
     (assert (pos? n 0))
     ;; NRHS >= 0.
     (assert (pos? nrhs))
     ;; A is DOUBLE PRECISION array, dimension (LDA,N)
     (assert (= (c/ncols a) n))
     ;; LDA >= max(1,M)
     (assert (>= lda (max m 1)))
     ;; B is DOUBLE PRECISION array, dimension (LDB,NRHS)
     (assert (= (c/ncols b) nrhs))
     ;; LDB >= max(1,max(M,N))
     (assert (>= ldb (max 1 m n)))
     ;; S is DOUBLE PRECISION array, dimension (min(M,N))
     (assert (= (c/dim s) (min m n)))
     (assert (every? #(= (order %) COLUMN_MAJOR) [a b])
             "Matrices must have COLUMN_MAJOR order")
     (let [info (LAPACK/dgelsd_ m
                                n
                                nrhs
                                (buffer a)
                                lda
                                (buffer b)
                                ldb
                                (buffer s)
                                rcond
                                rank
                                (buffer work)
                                lwork
                                (buffer iwork))
           rank-val (.getInt rank 0)]
       (cond
         (pos? info) (throw (RuntimeException.
                             "Algorithm failed to converge"))
         (neg? info) (throw (RuntimeException.
                             "Unexpected error due to invalid LAPACK argument"))
         :else {:info info :rank rank-val}))))
  ([m n nrhs a b s rcond work lwork iwork]
   (let [rank (direct-buffer Integer/BYTES)]
     (-dgelsd m n nrhs a b s rcond rank work lwork iwork))))

(defn dgelsd-query [m n nrhs a b s rcond iwork]
  (let [work0 (n/dv 1)]
    (-dgelsd m n nrhs a b s rcond work0 -1 iwork)
    (int (entry work0 0))))

(defn log2 [x]
  (/ (Math/log x) (Math/log 2)))

(defn dgelsd
  ([m n nrhs a b s rcond work iwork]
   (let [{:keys [rank]} (-dgelsd m n nrhs a b s rcond work (c/dim work) iwork)]
     {:rank rank :x b :s s}))
  ([m n nrhs a b s rcond]
   ;; IWORK is INTEGER array, dimension (MAX(1,LIWORK))
   ;; LIWORK >= max(1, 3 * MINMN * NLVL + 11 * MINMN),
   ;; where MINMN = MIN( M,N ).
   ;; NLVL = MAX(0, int( log_2( MINMN / (SMLSIZ+1) ) ) + 1)
   ;; and SMLSIZ = 25
   (let [minmn (min m n)
         nlvl (max 0 (inc (int (log2 (/ minmn 26)))))
         liwork (+ (* 3 minmn nlvl) (* 11 minmn))
         iwork (n/dv (max 1 liwork))
         lwork (dgelsd-query m n nrhs a b s rcond iwork)
         work (n/dv lwork)]
     (dgelsd m n nrhs a b s rcond work iwork)))
  ([a b rcond]
   (let [m (c/mrows a)
         n (c/ncols a)
         nrhs (c/ncols b)
         s (n/dv (min m n))]
     (dgelsd m n nrhs a b s rcond)))
  ([a b]
   ;; NOTE: b is destroyed!
   (let [rcond -1]  ;; use machine precision
     (dgelsd a b -1))))

;;;;; High-level API ;;;;;

(defn svd
  "Singular-value decomposition of a. Result contains {:s S :u U :vt Vt}."
  [a & {:keys [full-matrices]}]
  (let [jobz (if full-matrices \A \S)
        a (c/copy a)]  ;; operation is destructive in A
    (dgesdd jobz a)))

(defn lstsq
  "Least-squares solution to linear system AX = B. If B is a matrix, this is
  equivalent to solving for each column independently. Result contains
  {:s S :x X :rank r}, where r and S are the effective rank and singular values
  of A.

  The effective rank of A is determined by treating as zero those singular
  values which are less than rcond times the largest singular value."
  [a b]
  (let [b (c/copy b)]  ;; operation is destructive in B
    (dgelsd a b)))

;; TODO: add macros for creating IFn$DD and $Fn$DDD
