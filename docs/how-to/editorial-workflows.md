# Accelerating content creation
Utilising a programmatic mindset, this tool promises to significantly accelerate content creation for editorial teams at MCN. Instead of relying on deep expert knowledge, or spending countless hours manually searching for each bike worth featuring in a piece of content, this tool can find them all with a single well-constructed query.

If we have an idea for a buying guide or newsletter, let's say:

> "best budget A2 bikes for shorter riders"

A query can be used to find all the relevant bikes. In this case:

```elisp
(mcn/query-bikes
 '(and
     (:used-price < 2500) ;; budget
	 (:max-power < 47)    ;; A2 bikes
	 (:max-power > 15)    ;; ^^
     (:seat-height < 800))) ;; for shorter riders

;; => ("2024-on CF-moto 450NK" ... "https://www.motorcyclenews.com/bike-reviews/...")
;;    ...
```

# Accelerating content ideation
You can also use this tool to help with inspiration for content ideas, as playing around with the different filters can yield interesting results. For example, think about what would these queries might output:

```elisp
(mcn/query-bikes
	'(and
		(:average-fuel-consumption > 150)
		(:top-speed >= 60)))
```

> "Best fuel-efficient but motorway-capable A1 bikes"?

```elisp
(mcn/query-bikes 
	'(and
		(:annual-service-cost <= 100)
		(:insurance-group < 7)
		(:annual-road-tax < 50)
		(:used-price < 2500)))
```

> "Best cheap runners"?

```elisp
(mcn/query-bikes
	'(and
		(:engine-size = 750)
		(:tank-range > 200)))
```

> "Best 750cc mile-munchers"?

```elisp
(mcn/query-bikes
	'(and
		(:used-price < 5000)
		(:used-price > 0)
		(:top-speed >= 180))
```
> "Best bargain bikes for speed-demons"?

# Integration with LLMs
To further accelerate content creation, an integration with ChatGPT is currently in development. In the meantime, you can copy and paste the output from this tool directly into your choice of LLM, and ask it to draft a piece of content with those bikes. This gives the LLM less work to do searching for the bikes, which it does far less reliably than this tool, and generates a first-draft for content rapidly.
