(ns mcn.storage
  (:require [clojure.java.io :as io]
            [clojure.edn :as edn]))

(def default-cache-path "resources/mcn/bikes.edn")

(defn cache-exists?
  ([] (cache-exists? default-cache-path))
  ([path]
   (.exists (io/file path))))

(defn save-bikes-map!
  ([bikes]
   (save-bikes-map! default-cache-path bikes))
  ([path bikes]
   (try
     (io/make-parents path)
     (spit path (pr-str bikes))
     {:ok {:path path
           :count (count bikes)}}
     (catch Exception e
       {:err {:type :write-cache
              :path path
              :message (.getMessage e)}}))))

(defn load-bikes-map
  ([] (load-bikes-map default-cache-path))
  ([path]
   (try
     (if-not (cache-exists? path)
       {:err {:type :cache-miss
              :path path
              :message "Bike cache file does not exist."}}
       (let [data (edn/read-string (slurp path))]
         (if (map? data)
           {:ok data}
           {:err {:type :invalid-cache
                  :path path
                  :message "Bike cache file doesn't contain a map"}})))
     (catch Exception e
       {:err {:type :read-cache
              :path path
              :message (.getMessage e)}}))))
