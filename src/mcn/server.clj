(ns mcn.server
  (:require [ring.adapter.jetty :as jetty]
            [mcn.api :as api]
            [mcn.storage :as storage]
            [mcn.util :as util]))

(def default-port 3000)

(defonce server (atom nil))

(defn load-state []
  (let [loaded (storage/load-bikes-map)]
    (if (util/ok? loaded)
      {:bikes (:ok loaded)}
      {:startup-error (:err loaded)})))

(defn port []
  (parse-long (or (System/getenv "PORT")
                  (str default-port))))

(defn handler []
  (api/app (load-state)))

(defn start! []
  (let [app (handler)]
    (reset! server
            (jetty/run-jetty app {:port (port)
                                  :join? false}))))

(defn stop! []
  (when-let [s @server]
    (.stop s)
    (reset! server nil)))

(defn restart! []
  (stop!)
  (start!))

(defn -main [& _args]
  (start!))
