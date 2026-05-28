(ns mcn.core-test
  (:require [clojure.test :refer [deftest is testing]]
            [mcn.core :as mcn]
            [mcn.storage :as storage]))

(defn temp-cache-path []
  (let [file (java.io.File/createTempFile "mcn-bike-cache" ".edn")]
    (.delete file)
    (.getPath file)))

(deftest get-or-fetch-bikes-map-test
  (testing "loads existing cache"
    (let [path (temp-cache-path)
          test-bikes {"bike-a" {:ok {:bike-name "bike-a"}}}]
      (try
        (storage/save-bikes-map! path test-bikes)
        (is (= {:ok test-bikes}
               (mcn/get-or-fetch-bikes-map path false (fn [] {}))))
        (finally
          (.delete (java.io.File. path)))))))
