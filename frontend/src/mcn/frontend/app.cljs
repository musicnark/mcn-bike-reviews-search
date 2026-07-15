(ns mcn.frontend.app
  (:require
   [reagent.dom.client :as rdom]))

(defonce root (atom nil))

(defn app []
  [:div
   [:h1 "MCN Bike Reviews Search"]
   [:p "Frontend online"]])

(defn init []
  (let [el (.getElementById js/document "app")]
    (reset! root (rdom/create-root el))
    (rdom/render @root [app])))
