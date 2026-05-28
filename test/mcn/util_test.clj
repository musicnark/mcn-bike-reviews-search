(ns mcn.util-test
  (:require [clojure.test :refer [deftest is testing]]
            [mcn.util :as util]))

(deftest clean-keyword-test
  (testing "removes colons and converts to kebab-case"
    (is (= :seat-height (util/clean-keyword "Seat Height:")))
    (is (= :mpg (util/clean-keyword "MPG:"))))
  (testing "normalises slash-delimited spec labels to EDN-safe keywords"
    (is (= :quarter-mile-acceleration
           (util/clean-keyword "1/4 Mile Acceleration:")))
    (is (= :quarter-mile-acceleration
           (util/clean-keyword "1/4 mile acceleration")))))

(deftest clean-bike-name-test
  (testing "reformats URL string"
    (is (= "kawasaki-kle500-2026" (util/clean-bike-name "https://www.motorcyclenews.com/bike-reviews/kawasaki/kle500/2026/")))
    (is (= "norton" (util/clean-bike-name "https://www.motorcyclenews.com/bike-reviews/norton/")))
    (is (nil? (util/clean-bike-name "Not-A-URL")))))
