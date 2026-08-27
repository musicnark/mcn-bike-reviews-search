(ns mcn.frontend.app
  (:require
   [reagent.dom.client :as rdom]
   [reagent.core :as r]
   [mcn.frontend.api :as api]))

(defonce root (atom nil))

(defonce app-state
  (r/atom {:status "loading..."}))

;; FIXME test
(defn load-random-bike! []
  (-> (api/get-json "/bikes/random")
      (.then #(swap! app-state assoc :bike {:bike-name (:bikeName %)
                                            :url (:url %)}))
      (.catch #(swap! app-state assoc :bike "API request failed"))))

(defn random-bike-section []
  (let [bike (:bike @app-state)]
    [:section
     [:div
      [:h2 "Random Bike:"]
      (if bike
        [:a {:href (:url bike)
             :target "_blank"}
         (:bike-name bike)]
        [:p "Loading..."])]
     [:div
      [:button {:on-click #(load-random-bike!)} "Load Random Bike"]]]))

(defn app []
  [:main
   [:h1 "MCN Bike Reviews Search"]
   [:p "Frontend online"]
   [random-bike-section]])

(defn init []
  (let [el (.getElementById js/document "app")]
    (reset! root (rdom/create-root el))
    (load-random-bike!)
    (rdom/render @root [app])))
