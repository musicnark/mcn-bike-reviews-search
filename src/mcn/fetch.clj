(ns mcn.fetch
  (:require [clj-http.client :as http]
            [clj-http.conn-mgr :as conn]
            [clojure.core.async :as async :refer [go >! chan close!]]
            [mcn.util :as mcn-util]))

(defonce connection-manager
  (delay
    (conn/make-reusable-async-conn-manager
     {:threads 50             ;; max threads for connecting
      :default-per-route 20   ;; max connections *per host*
      :timeout 10})))

(defn shutdown-connection-manager! []
  (when (realized? connection-manager)
    (conn/shutdown-manager @connection-manager)))

(defn fetch-bikes-async [url]
  (let [ch (chan)]
    (http/get url {:headers {"User-Agent" "Mozilla/5.0"}
                   :async? true
                   :connection-manager @connection-manager}
              ;; success callback
              (fn [r]
                (go
                  (>! ch {:ok {:url url
                               :response r}})
                  (close! ch)
                  (println "successfully fetched bike: " (mcn-util/clean-bike-name url))))
              ;; error callback
              (fn [e]
                (go
                  (>! ch {:err {:type :network-page
                                :url url
                                :message (.getMessage e)}})
                  (println "ERR: " e) ;; TODO redirect to logging
                  (close! ch))))
    ch))
