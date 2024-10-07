(ns franks42.sorted-map-atom
  (:refer-clojure :exclude [assoc! update! update-in! into! conj! merge! dissoc!])
  (:require
   [cheshire.core :as json]
   [clojure.pprint :as pp]
   [clojure.set]
   [clojure.string]
   [duratom.core :as dc]
   [tick.core :as t])
  (:import
   [clojure.lang MapEntry]
   [java.lang Double Long]))

;;

(defprotocol MutableAssociativeP
  (-assoc! [m kvs])
  (-dissoc! [m ks])
  (-merge! [m xs])
  ;; (conj! [m x])
  (-conj! [m x xs])
  (into! [m xs])
  (-update! [this k f args])
  (-update-in! [this ks f args])
  (clear! [m])
  ;;
  )

(deftype SortedMapAtom [atm]
  clojure.lang.IDeref
  (deref [_]
    (deref atm))
  clojure.lang.IFn
  (invoke [this k not-found] (.invoke @atm k not-found))
  (invoke [this k] (.invoke this k nil))
  clojure.lang.ILookup
  (valAt [this k not-found] (.invoke this k not-found))
  (valAt [this k] (.invoke this k nil))
  ;;
  MutableAssociativeP
  (-assoc! [this kvs]
    (when (> (count kvs) 0)
      (swap! atm (fn [m kvs] (apply assoc m kvs)) kvs))
    this)
  (-dissoc! [this ks]
    (swap! atm (fn [m ks] (apply dissoc m ks)) ks)
    this)
  (-conj! [this x xs]
    (swap! atm (fn [m x xs] (apply conj m x xs)) x xs)
    this)
  (-merge! [this xs]
    (swap! atm (fn [m xs] (apply merge (cons m xs))) xs)
    this)
  (into! [this xs]
    (swap! atm into xs)
    this)
  (-update! [this k f args]
    (swap! atm (fn [m k f args] (apply update m k f args)) k f args)
    this)
  (-update-in! [this ks f args]
    (swap! atm (fn [m ks f args] (apply update-in m ks f args)) ks f args)
    this)
  (clear! [this] (reset! atm (sorted-map)) this)
  ;;
  )

;;

(defn assoc! [m k0 v0 & kvs] (-assoc! m (into [k0 v0] kvs)))

(defn dissoc! [m k0 & ks] (-dissoc! m (cons k0 ks)))

(defn merge! [m & xs] (-merge! m xs))

(defn conj! [m x & xs] (-conj! m x xs))

(defn update! [this k f & args] (-update! this k f args))

(defn update-in! [this ks f & args] (-update-in! this ks f args))

;;

(defn sorted-map-atom
  "Factory function for a mutable sorted map backed by an atom.
   That sorted map can be durable when backed by a 'duratom'.
   The achived abstraction is a (durable) mutable sorted map that is 
   updated and mutated by well-understood associative functions, 
   like assoc!, dissoc!, conj!, merge!, into!, update!, update-in!.
   Under the cover, those functions use their immutable counter-parts
   and swap! to update the sorted-map backed by the atom, but unlike their immutable 
   counterparts, they actually update the actual sorted map backed by the atom with 
   the same transactional properties of swap!.
   Note that an immutable snapshot-copy of the value of the sorted-map 
   backed by the atom, can be obtained thru the well known deref/@, like with any atom value.
   The abstraction goal is to hide the details of the mutating interactions with the 
   sorted-map+atom behind a more familiar interface and to provide less of a 
   (mental) impedance mismatch.
   "
  [& atm+keyvals-or-keyvals]
  (let [atm? (odd? (count atm+keyvals-or-keyvals))
        atm (if atm? (first atm+keyvals-or-keyvals)
                (atom (sorted-map)))
        - (assert (and (instance? clojure.lang.IAtom atm)
                       (sorted? @atm) (map? @atm))
                  "Atom value must be sorted-map.")
        keyvals (if atm? (rest atm+keyvals-or-keyvals)
                    atm+keyvals-or-keyvals)
        sma (SortedMapAtom. atm)]
    (if (next keyvals)
      (-assoc! sma keyvals)
      sma)))
