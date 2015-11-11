(ns factual-assignment.core-test
  (:require [clojure.test :refer :all]
            [factual-assignment.core :refer :all]))

(def board (atom ["FYYHNRD" "RLJCINU" "AAWAAHR" "NTKLPNE" "CILFSAP" "EOGOTPN" "HPOLAND"]))

(def char-map (atom (gen-char-map @board)))

(def words (atom ["ITALY" "HOLLAND" "POLAND" "SPAIN" "FRANCE" "JAPAN" "TOGO" "PERU"]))

(deftest a-test
  (testing "Find words in test board."
    (is (= 0 1))))

;; This works now, nice!
(map find-word @words)
