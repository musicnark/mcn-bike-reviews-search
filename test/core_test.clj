(ns core-test
  (:require [clojure.test :refer [deftest is testing]]
            [core :as mcn]))

(deftest clean-keyword-test
  (testing "removes colons and converts to kebab-case"
    (is (= :seat-height (mcn/clean-keyword "Seat Height:")))
    (is (= :mpg (mcn/clean-keyword "MPG:")))))

(deftest clean-bike-name-test
  (testing "reformats URL string by splitting at 'bike-reviews', taking the second element, and converting to kebab-case"
    (is (= "kawasaki-kle500-2026" (mcn/clean-bike-name "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")))
    (is (= "norton" (mcn/clean-bike-name "https://www.motorcyclenews.com/bike-reviews/norton/")))
    (is (= "zontes-zt125-g1" (mcn/clean-bike-name "https://www.motorcyclenews.com/bike-reviews/zontes/zt125-g1/")))
    (is (nil? (mcn/clean-bike-name "Not-A-URL")))
    (is (nil? (mcn/clean-bike-name "")))
    (is (nil? (mcn/clean-bike-name nil)))))

(deftest parse-number-test
  (testing "parses numeric entries in bikes data to extract the raw number, with no units alongside it (e.g., '£', 'mm', 'kg', 'mph', 'out of 17', etc)"
    (is (= 8300.0 (mcn/parse-number "\n\t\t£8,300\n\t")))
    (is (= 17.0 (mcn/parse-number "17 litres")))
    (is (= 47.5 (mcn/parse-number "47.5 bhp")))
    (is (= 17 (mcn/parse-number 17)))
    (is (nil? (mcn/parse-number "hehadsd")))
    (is (nil? (mcn/parse-number nil)))))

(def test-bike
  {:bike-name "test-bike"
   :fuel-capacity "17 litres"
   :seat-height "775mm"
   :max-power "47 bhp"
   :used-price "\n\t£3,100 - £3,500\n\t"})

(deftest compare-field-test
  (testing "evaluates a comparison query against one bike map"
    (is (true? (mcn/compare-field test-bike
                                  {:type "comparison"
                                   :field "fuel-capacity"
                                   :op ">"
                                   :value 15})))
    (is (false? (mcn/compare-field test-bike
                                    {:type "comparison"
                                     :field "seat-height"
                                     :op "<"
                                     :value 700})))
    (is (false? (mcn/compare-field test-bike
                                    {:type "comparison"
                                     :field "missing-field"
                                     :op ">"
                                     :value 1})))
    (is (false? (mcn/compare-field test-bike
                                    {:type "comparison"
                                     :field "fuel-capacity"
                                     :op "contains"
                                     :value 17})))))

(deftest matches-test
  (testing "evaluates 'comparison', 'and', 'or', and 'not' query nodes"
    (is (true? (mcn/matches? test-bike
                             {:type "comparison"
                              :field "max-power"
                              :op "="
                              :value 47})))
    (is (true? (mcn/matches? test-bike
                             {:type "and"
                              :clauses [{:type "comparison"
                                         :field "fuel-capacity"
                                         :op ">"
                                         :value 15}
                                        {:type "comparison"
                                         :field "seat-height"
                                         :op "<"
                                         :value 800}]})))
    (is (true? (mcn/matches? test-bike
                             {:type "or"
                              :clauses [{:type "comparison"
                                         :field "fuel-capacity"
                                         :op "<"
                                         :value 5}
                                        {:type "comparison"
                                         :field "seat-height"
                                         :op "<"
                                         :value 800}]})))
    (is (true? (mcn/matches? test-bike
                             {:type "not"
                              :clause {:type "comparison"
                                       :field "fuel-capacity"
                                       :op "<"
                                       :value 5}})))
    (is (false? (mcn/matches? test-bike
                              {:type "unsupported"})))))

(def test-bikes
  {"bike-a" {:ok {:bike-name "bike-a"
                  :fuel-capacity "17 litres"
                  :used-price "£3,000"}}
   "bike-b" {:ok {:bike-name "bike-b"
                  :fuel-capacity "3.8 litres"
                  :used-price "£2,500"}}
   "bike-c" {:ok {:bike-name "bike-c"
                  :fuel-capacity "4 litres"
                  :used-price "£4,000"}}
   "bike-d" {:err {:type :parse-html
                   :message "labels or values weren't found in HTML response."}}})

(deftest query-bikes-test
  (testing "filters, sorts, limits, and returns an API-shaped response from the collated bike specs"
    (let [response (mcn/query-bikes
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
      (is (= 2 (get-in response [:ok :total-matches])))
      (is (= ["bike-b"] (map :id results)))
      (is (= "bike-b" (:bike-name (first results))))))
  (testing "sorts descending when requested"
    (let [response (mcn/query-bikes
                    test-bikes
                    {:filter {:type "comparison"
                              :field "fuel-capacity"
                              :op "<"
                              :value 20}
                     :sort {:field "used-price"
                            :direction "desc"}})]
      (is (= ["bike-c" "bike-a" "bike-b"]
             (map :id (get-in response [:ok :results])))))))
