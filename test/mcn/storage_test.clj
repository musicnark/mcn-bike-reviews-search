(ns mcn.storage-test
  (:require [clojure.test :refer [deftest is testing]]
            [mcn.storage :as storage]))

(defn temp-cache-path []
  (let [file (java.io.File/createTempFile "mcn-bike-cache" ".edn")]
    (.delete file)
    (.getPath file)))

(def test-bikes
  {"bike-a" {:ok {:bike-name "bike-a"}}})

(deftest persistent-storage-test
  (testing "saves and loads bike maps"
    (let [path (temp-cache-path)]
      (try
        (is (false? (storage/cache-exists? path)))
        (is (= {:ok {:path path
                     :count 1}}
               (storage/save-bikes-map! path test-bikes)))
        (is (true? (storage/cache-exists? path)))
        (is (= {:ok test-bikes}
               (storage/load-bikes-map path)))
        (finally
          (.delete (java.io.File. path)))))))
