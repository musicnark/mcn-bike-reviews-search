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
