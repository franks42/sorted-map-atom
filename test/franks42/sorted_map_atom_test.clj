(ns franks42.sorted-map-atom-test
  (:refer-clojure :exclude [assoc! update! update-in! into! conj! merge! dissoc!])
  (:require [clojure.test :refer :all]
            [babashka.fs :as fs]
            [franks42.sorted-map-atom :as sma]
            [duratom.core :as dc]))

;;

(def duratom-test-file "/var/db/duratom/sorted-map-atom-test")

;; (fs/delete-if-exists duratom-test-file)

(defn test-duratom []
  (fs/delete-if-exists duratom-test-file)
  (dc/duratom :local-file :file-path duratom-test-file :init (sorted-map)))

;;

(deftest sorted-map-atom-test
  (testing "sorted-map-atom atom constructor"
    (let [sm (sma/sorted-map-atom)]
      (is (= franks42.sorted_map_atom.SortedMapAtom (type sm)))
      (is (= clojure.lang.PersistentTreeMap (type @sm)))
      (is (= clojure.lang.Atom (type (.atm sm))))
      (is (and (instance? clojure.lang.IAtom (.atm sm)) (map? @sm) (sorted? @sm)))))
  (testing "sorted-map-atom atom constructor with initial values"
    (is (= {} @(sma/sorted-map-atom)))
    (is (= {1 1} @(sma/sorted-map-atom 1 1)))
    (is (= {1 1 2 2} @(sma/sorted-map-atom 1 1 2 2)))
    (is (= {1 1 2 2 3 3} @(sma/sorted-map-atom 1 1 2 2 3 3))))
  (testing "sorted-map-atom atom constructor with mixed key types"
    (is (= {1 1 2 2} @(sma/sorted-map-atom 1 1 2 2)))
    (is (= "class clojure.lang.Keyword cannot be cast to class java.lang.Number (clojure.lang.Keyword is in unnamed module of loader 'app'; java.lang.Number is in module java.base of loader 'bootstrap')"
           (try @(sma/sorted-map-atom :1 1 2 2)
                (catch Exception e (.getLocalizedMessage e)))))
    (is (= {:1 1 :2 2} (try @(sma/sorted-map-atom :1 1 :2 2)
                            (catch Exception e (.getLocalizedMessage e)))))
    (is (= "class java.lang.Long cannot be cast to class clojure.lang.Keyword (java.lang.Long is in module java.base of loader 'bootstrap'; clojure.lang.Keyword is in unnamed module of loader 'app')"
           (try @(sma/sorted-map-atom 1 1 :2 2)
                (catch Exception e (.getLocalizedMessage e))))))
  (testing "sorted-map-atom duratom constructor"
    (let [sm (sma/sorted-map-atom (test-duratom))]
      (is (= franks42.sorted_map_atom.SortedMapAtom (type sm)))
      (is (= clojure.lang.PersistentTreeMap (type @sm)))
      (is (= duratom.core.Duratom (type (.atm sm))))
      (is (and (instance? clojure.lang.IAtom (.atm sm)) (map? @sm) (sorted? @sm)))))
  (testing "sorted-map-atom duratom constructor with initial values - tricky!"
    (is (= {} @(sma/sorted-map-atom (test-duratom))))
    (is (= {1 1} @(sma/sorted-map-atom (test-duratom) 1 1)))
    (is (= {1 1 2 2} @(sma/sorted-map-atom (test-duratom) 1 1 2 2)))
    (is (= {1 1 2 2 3 3} @(sma/sorted-map-atom (test-duratom) 1 1 2 2 3 3))))
  ;;
  (testing "sorted-map-atom with assoc!, dissoc!, clear!"
    (let [sm (sma/sorted-map-atom)]
      (is (= {1 1} @(sma/assoc! sm 1 1)))
      (is (= {1 1 2 2 3 3} @(sma/assoc! sm 2 2 3 3)))
      (is (= {1 1 2 2 3 3 4 4 5 5 6 6} @(sma/assoc! sm 4 4 5 5 6 6)))
      (is (= {1 1 2 2 3 3 4 4 5 5} @(sma/dissoc! sm 6 6)))
      (is (= {1 1 2 2} @(sma/dissoc! sm 5 5 4 4 3 3)))
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with conj!"
    (let [sm (sma/sorted-map-atom)]
      (is (= {1 1} @(sma/conj! sm [1 1])))
      (is (= {1 1 2 2 3 3} @(sma/conj! sm [2 2] [3 3])))
      (is (= {1 1 2 2 3 3 4 4 5 5 6 6} @(sma/conj! sm [4 4] [5 5] [6 6])))
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with merge!"
    (let [sm (sma/sorted-map-atom)]
      (is (= {} @(sma/merge! sm)))
      (is (= {1 1} @(sma/merge! sm [1 1])))
      (is (= {1 1 2 2 3 3} @(sma/merge! sm [2 2] [3 3])))
      (is (= {1 1 2 2 3 3 4 4 5 5 6 6} @(sma/merge! sm [4 4] {5 5 6 6})))
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with into!"
    (let [sm (sma/sorted-map-atom)]
      (is (= {1 1} @(sma/into! sm {1 1})))
      (is (= {1 1 2 2} @(sma/into! sm [[2 2]])))
      (is (= {1 1 2 2 3 3 4 4} @(sma/into! sm [[3 3] {4 4}])))
      (is (= {1 1 2 2 3 3 4 4 5 5 6 6} @(sma/into! sm {5 5 6 6})))
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with into!"
    (let [sm (sma/sorted-map-atom)]
      (is (= {1 1} @(sma/into! sm {1 1})))
      (is (= {1 1 2 2} @(sma/into! sm [[2 2]])))
      (is (= {1 1 2 2 3 3 4 4} @(sma/into! sm [[3 3] {4 4}])))
      (is (= {1 1 2 2 3 3 4 4 5 5 6 6} @(sma/into! sm {5 5 6 6})))
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with update!"
    (let [sm (sma/sorted-map-atom 1 1 2 2 3 3)]
      (is (= {1 1, 2 3, 3 3} @(sma/update! sm 2 inc)))
      (is (= {1 1, 2 4, 3 3} @(sma/update! sm 2 + 1)))
      (is (= {1 1, 2 6, 3 3} @(sma/update! sm 2 + 1 1)))
      (is (= {1 1, 2 9, 3 3} @(sma/update! sm 2 + 1 1 1)))
      (is (= {1 1, 2 20, 3 3} @(sma/update! sm 2 + 1 1 1 1 1 1 1 1 1 1 1)))
      ;; (is (= {1 1} @(sma/update! sm 2 inc nil))) ;; exception as nil becomes 2nd arg
      ;; (is (= {1 1} @(sma/update! sm 2 inc []))) ;; exception as [] becomes 2nd arg
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with update-in!"
    (let [sm (sma/sorted-map-atom 1 1 2 {22 22} 3 3)]
      (is (= {1 1 2 {22 23} 3 3} @(sma/update-in! sm [2 22] inc)))
      (is (= {1 1, 2 {22 24}, 3 3} @(sma/update-in! sm [2 22] + 1)))
      (is (= {1 1, 2 {22 26}, 3 3} @(sma/update-in! sm [2 22] + 1 1)))
      (is (= {1 1, 2 {22 29}, 3 3} @(sma/update-in! sm [2 22] + 1 1 1)))
      (is (= {1 1, 2 {22 40}, 3 3} @(sma/update-in! sm [2 22] + 1 1 1 1 1 1 1 1 1 1 1)))
      ;; (is (= {1 1} @(sma/update-in! sm [2 22] inc nil))) ;; exception as nil becomes 2nd arg
      ;; (is (= {1 1} @(sma/update-in! sm [2 22] inc []))) ;; exception as [] becomes 2nd arg
      (is (= {} @(sma/clear! sm)))))
  (testing "sorted-map-atom with -> (threading)"
    (is (= {1 1 2 2 3 3 4 4 5 5 6 6}
           (-> (sma/sorted-map-atom 6 6)
               (sma/assoc! 1 1)
               (sma/conj! [2 2])
               (sma/merge! [3 3])
               (sma/into! {4 4 5 5})
               (deref)))))
      ;;
  ;;
  )






(comment
  ;;

  (def sma (sma/sorted-map-atom 1 11 2 22))
  @sma
  sma
  (type @sma)
  (sorted? @sma)
  (sorted? sma)

  (sma 1)
  (@sma 1)
  (sma :a)

  (sma/clear! sma)
  (sma/assoc! sma :a 1 :b 2)
  @sma
  (sma :a)
  (sma :z)
  (sma :z :oh-no)
  (:a sma)
  (:z sma)
  (:z sma :oh-no)
  (@sma :a)


  ;;
  (sma/clear! sma)

  (sma/assoc! sma 3 33)
  (sma/assoc! sma 4 44 5 55 6 66)
  sma
  @sma
  (type sma)
  (instance? clojure.lang.IAtom sma)
  (instance? clojure.lang.IAtom (.atm sma))
  (instance? sma/MutableAssociativeP sma)
  (satisfies? sma/MutableAssociativeP sma)
  (instance? clojure.lang.IDeref sma)
  (satisfies? clojure.lang.IDeref sma)



  ;;
  (def tst-atom-map (dc/duratom :local-file
                                :file-path "/var/db/duratom/tst-kv"
                                :init (sorted-map)))

  (type tst-atom-map)
  (type @tst-atom-map)
  (instance? clojure.lang.IAtom2 tst-atom-map)
  (instance? clojure.lang.IAtom tst-atom-map)

  ;;

  (def tst-aa (AssociativeAtom. tst-atom-map))
  (def tst-aa (AssociativeAtom. (atom {})))
  (type tst-aa)
  (.aa tst-aa)

  (assoc! tst-aa 1 11)
  (.aa tst-aa)
  @(.aa tst-aa)

  (defn mutable-sorted-durable-atom-map [map-name key-type])

  (dc/map->Duratom)

  (def sm (sorted-map))
  (type sm)
  (def sm1 (assoc sm :a 1))
  (type sm1)
  (assoc sm1 "a" 1)
  (def sm2 (dissoc sm1 :a))
  (type sm2)
  (assoc sm2 "a" 1)


  (extend-type)

  (def a-m (atom {}))
  @a-m
  (def a-s-m (atom (sorted-map)))
  @a-s-m
  (def a-v (atom []))
  @a-v

  (swap! a-m assoc :a 1)

  (defn a-m-assoc! [a-m k0 v0 & kvs]
    (assert (map? @a-m) (str "atom managed value is not a map but: " (type @a-m)))
    (swap! a-m (fn [m kvs] (apply assoc m kvs)) (into [k0 v0] kvs)))

  (a-m-assoc! a-m :b 2 :c 3)
  (a-m-assoc! a-v :b 2)

  (a-m-assoc! a-s-m)
  (a-m-assoc! a-s-m :b 2)
  (a-m-assoc! a-s-m :a 1)
  (a-m-assoc! a-s-m :a 11)
  (a-m-assoc! a-s-m "a" 1)

  (deref a-s-m)

  ;;
  )