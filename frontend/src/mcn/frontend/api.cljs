(ns mcn.frontend.api)

(def api-base "http://localhost:3000/api")

(defn get-json [path]
  (-> (js/fetch (str api-base path))
      (.then #(.json %))
      (.then #(js->clj % :keywordize-keys true))))
