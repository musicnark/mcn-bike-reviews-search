(ns mcn.refresh
  (:require [mcn.core :as mcn]
            [mcn.fetch :as fetch]
            [mcn.storage :as storage]
            [mcn.util :as util]))

(defn refresh! [path]
  (mcn/get-or-fetch-bikes-map path true))

(defn -main [& [path]]
  (let [path (or path storage/default-cache-path)]
    (try
      (let [result (refresh! path)]
        (if (util/err? result)
          (throw (ex-info "Live bike data refresh failed." (:err result)))
          (println "Refreshed" (count (:ok result)) "bike reviews in" path)))
      (finally
        (fetch/shutdown-connection-manager!)))))
