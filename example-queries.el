;; Old syntax (kept for posterity)
;; Best Superbikes
(query-bikes-by-tag :max-power '> 200) 

;; MCN's Favourite A2 Bikes
(query-bikes-by-tag :max-power '< 47) 

;; Best Mile-Munching Tourers
(query-bikes-by-tag :tank-range '> 240) 

;; Best Bikes for Speed-Demons
(query-bikes-by-tag :top-speed '> 160) 

;; MCN's Favourite 750cc Bikes
(query-bikes-by-tag :engine-size '= 750) 

;; Best Bikes for Shorter Riders
(query-bikes-by-tag :seat-height '< 800) 

;; Best Lightweight Bikes
(query-bikes-by-tag :bike-weight '< 150) 

;; most fuel efficient bikes
(query-bikes-by-tag :average-fuel-consumption '> 100)

;; best new bikes under £5000
(query-bikes-by-tag :new-price '<> 5000)

;; best used bikes under £2000
(query-bikes-by-tag :used-price '<> 2000)

;; ??
(query-bikes-by-tag :annual-service-cost '<> 50)

;; New syntax
;; Best fuel-efficient motorway-capable A1 bikes
(query-bikes
 '(and
	 (:average-fuel-consumption > 150)
	 (:top-speed >= 60)))
