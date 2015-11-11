(ns factual-assignment.core-test
  (:require [clojure.test :refer :all]
            [factual-assignment.core :refer :all]))

(deftest a-test
  (testing "Find words in test board."
    (let [board-data ["FYYHNRD" "RLJCINU" "AAWAAHR"
                      "NTKLPNE" "CILFSAP" "EOGOTPN" "HPOLAND"]
          word-data ["ITALY" "HOLLAND" "POLAND" "SPAIN"
                     "FRANCE" "JAPAN" "TOGO" "PERU"]]
      (load-data! [board-data word-data false])
      (is (find-word "ITALY"))
      (is (find-word "FRANCE"))
      (is (find-word "POLAND"))
      (is (not (find-word "IGURU")))))

  (testing "Another test board"
    (let [board-data ["ABC" "DEF" "GHI"]
          word-data ["FED" "CAB" "GAD" "BID" "HIGH"]]
      (load-data! [board-data word-data false])
      (is (find-word "FED"))
      (is (not (find-word "CAB")))))

  (testing "A third test board with wrapping."
    (let [board-data ["ABC" "DEF" "GHI"]
          word-data ["FED" "CAB" "GAD" "BID" "HIGH"]]
      (load-data! [board-data word-data true])
      (is (find-word "FED"))
      (is (find-word "CAB"))
      (is (find-word "GAD"))
      (is (not (find-word "HIGH"))))))
