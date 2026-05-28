(ns mcn.retry
  (:require [clojure.core.async :refer [<!!]]
            [mcn.util :as mcn-util]
            [mcn.fetch :as fetch]
            [mcn.parse :as parse]))

(def default-max-retries 3)

(defn fetch-and-parse-bike [url]
  (let [fetch-result (<!! (fetch/fetch-bikes-async url))]
    (if (mcn-util/err? fetch-result)
      fetch-result
      (parse/parse-bike fetch-result))))

(defn retry-count [result]
  (or (get-in result [:err :retry-count]) 0))

(defn retryable-error? [result max-retries]
  (and (mcn-util/err? result)
       (some? (get-in result [:err :url]))
       (< (retry-count result) max-retries)))

(defn add-retry-metadata [result url retry-count]
  (if (mcn-util/err? result)
    {:err (assoc (:err result)
                 :url (or (get-in result [:err :url]) url)
                 :retry-count retry-count)}
    result))

(defn retry-bike-entry [id result max-retries retry-bike]
  (let [url (get-in result [:err :url])
        starting-retry-count (retry-count result)]
    (loop [current-retry-count starting-retry-count]
      (let [retried (retry-bike url)
            next-retry-count (inc current-retry-count)]
        (if (or (mcn-util/ok? retried) (>= next-retry-count max-retries))
          (let [result-with-metadata (add-retry-metadata retried url next-retry-count)
                new-id (or (get-in result-with-metadata [:ok :bike-name]) id)]
            [new-id result-with-metadata])
          (recur next-retry-count))))))

(defn update-bikes-map
  ([bikes]
   (update-bikes-map bikes default-max-retries fetch-and-parse-bike))
  ([bikes max-retries]
   (update-bikes-map bikes max-retries fetch-and-parse-bike))
  ([bikes max-retries retry-bike]
   (reduce-kv
    (fn [updated id result]
      (if (retryable-error? result max-retries)
        (let [[new-id new-result] (retry-bike-entry id result max-retries retry-bike)]
          (assoc updated new-id new-result))
        (assoc updated id result)))
    {}
    bikes)))
