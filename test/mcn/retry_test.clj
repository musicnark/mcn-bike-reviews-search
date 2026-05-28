(ns mcn.retry-test
  (:require [clojure.test :refer [deftest is testing]]
            [mcn.retry :as retry]))

(deftest update-bikes-map-test
  (testing "retries failed entries"
    (let [bikes {"bike-a" {:ok {:bike-name "bike-a"}}
                 :bike-1 {:err {:type :parse-html
                                :url "https://example.com/retry-me"
                                :message "parse failed"}}}
          updated (retry/update-bikes-map
                   bikes
                   10
                   (fn [url]
                     {:ok {:bike-name "retried-bike"
                           :url url}}))]
      (is (= {:ok {:bike-name "bike-a"}}
             (get updated "bike-a")))
      (is (= "retried-bike" (get-in updated ["retried-bike" :ok :bike-name]))))))
