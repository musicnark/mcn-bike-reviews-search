(ns mcn.query-test
  (:require [clojure.test :refer [deftest is testing]]
            [mcn.query :as query]))

(deftest parse-number-test
  (testing "parses numeric entries"
    (is (= 8300.0 (query/parse-number "\n\t\t£8,300\n\t")))
    (is (= 17.0 (query/parse-number "17 litres")))
    (is (= 47.5 (query/parse-number "47.5 bhp")))))

(def test-bike
  {:bike-name "test-bike"
   :fuel-capacity "17 litres"
   :seat-height "775mm"
   :max-power "47 bhp"
   :used-price "\n\t£3,100 - £3,500\n\t"})

(deftest compare-field-test
  (testing "evaluates a comparison query"
    (is (true? (query/compare-field test-bike
                                   {:type "comparison"
                                    :field "fuel-capacity"
                                    :op ">"
                                    :value 15})))))

(deftest matches-test
  (testing "evaluates query nodes"
    (is (true? (query/matches? test-bike
                              {:type "comparison"
                               :field "max-power"
                               :op "="
                               :value 47})))))

(def test-bikes
  {"bike-a" {:ok {:bike-name "bike-a"
                  :fuel-capacity "17 litres"
                  :used-price "£3,000"}}
   "bike-b" {:ok {:bike-name "bike-b"
                  :fuel-capacity "3.8 litres"
                  :used-price "£2,500"}}})

(deftest query-bikes-test
  (testing "filters and sorts"
    (let [response (query/query-bikes
                    test-bikes
                    {:filter {:type "comparison"
                              :field "fuel-capacity"
                              :op "<"
                              :value 5}
                     :sort {:field "used-price"
                            :direction "asc"}
                     :limit 1})
          results (get-in response [:ok :results])]
      (is (= 1 (get-in response [:ok :count])))
      (is (= "bike-b" (:bike-name (first results)))))))
