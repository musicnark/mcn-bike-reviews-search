(ns dev
  (:require [clj-http.client :as http]
            [net.cgrand.enlive-html :as html]
            [clj-http.util :as util]
            [clojure.string :as string]
            [clojure.data.xml :as xml]
            [mcn.core :as mcn]
            [mcn.util :as mcn-util]
            [mcn.query :as mcn-query]
            [mcn.fetch :as mcn-fetch]
            [mcn.parse :as mcn-parse]
            [mcn.storage :as mcn-storage]))

;; (add-tap (fn [x] (spit "src/log.txt" (pr-str x) :append true)))

(comment
  (def rez (mcn/get-or-fetch-bikes-map))
  (def rez (mcn/get-or-fetch-bikes-map mcn-storage/default-cache-path true))
  
  ;; example query
  (->
   (mcn-util/bind rez #(mcn/query-bikes %
                                       {:filter {:type "comparison"
                                                 :field "fuel-capacity"
                                                 :op "<"
                                                 :value 5}
                                        :sort {:field "bike-weight"
                                               :direction "asc"}
                                        :limit 10}))
   :ok
   :results))
