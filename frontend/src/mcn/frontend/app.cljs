(ns mcn.frontend.app
  (:require
   [reagent.dom.client :as rdom]
   [reagent.core :as r]
   [mcn.frontend.api :as api]))

(defonce root (atom nil))

(defonce app-state
  (r/atom {:status "loading..."}))

(defn load-random-bike! []
  (-> (api/get-json "/bikes/random")
      (.then #(swap! app-state assoc :bike {:bike-name (:bikeName %)
                                            :url (:url %)}))
      (.catch #(swap! app-state assoc :bike "API request failed"))))

(defn app []
  [:main
   [:h1 "MCN Bike Reviews Search"]
   [:p "Frontend online"]
  [:section
   [:h2 "Random Bike:"]
   [:p (-> (:bike @app-state) :bike-name)]
   [:a (-> (:bike @app-state) :url)]]])

(defn init []
  (let [el (.getElementById js/document "app")]
    (reset! root (rdom/create-root el))
    (load-random-bike!)
    (rdom/render @root [app])))
