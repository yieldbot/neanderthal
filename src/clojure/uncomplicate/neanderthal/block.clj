(ns uncomplicate.neanderthal.block
  (:import [uncomplicate.neanderthal.protocols Block]))

(defn entry-type [x]
  (.entryType ^Block x))

(defn buffer [x]
  (.buffer ^Block x))

(defn offset ^long [x]
  (.offset ^Block x))

(defn stride ^long [x]
  (.stride ^Block x))

(defn order ^long [x]
  (.order ^Block x))

(defn block? [x]
  (instance? Block x))
