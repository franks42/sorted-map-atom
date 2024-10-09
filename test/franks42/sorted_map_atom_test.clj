(ns franks42.sorted-map-atom-test
  (:refer-clojure :exclude [assoc! update! dissoc! conj! merge! into! update-in!])
  (:use [clojure.test])
  (:require
  ;;  [clojure.test :refer [deftest is testing #_thrown? #_thrown-with-msg? are]]
  ;;  [clojure.test :refer :all]
   [babashka.fs :as fs]
   [franks42.sorted-map-atom :as sma]
   [duratom.core :as dc]))

;;
;; user=> (is (thrown? ArithmeticException (/ 1 0)))
;; #<ArithmeticException java.lang.ArithmeticException: Divide by zero>

;; user=> (is (thrown-with-msg? ArithmeticException #"Divide by zero"
;;   #_=>                       (/ 1 0)))
;; #<ArithmeticException java.lang.ArithmeticException: Divide by zero>
;; user=> 
;; 

(def duratom-test-dir (or (System/getenv "TMPDIR")
                          (and (fs/directory? "/tmp") "/tmp")
                          (and (fs/directory? "/var/tmp") "/var/tmp")))

(def duratom-test-file (str duratom-test-dir "sorted-map-atom-test"))

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
