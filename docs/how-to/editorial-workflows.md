# Editorial Workflows
## Accelerating Content Creation
Utilising a programmatic mindset, this tool promises to significantly accelerate content ideation for editorial teams at MCN. Instead of relying on deep expert knowledge that takes years to hone, or spending countless hours manually searching for each bike worth featuring, this tool can find them all with a well constructed query.

If we have an idea for a buying guide or newsletter, let's say:

> "best budget A2 bikes for shorter riders"

A query can be used to find all the relevant bikes. In this case:

```elisp
(mcn/query-bikes
 '(and
     (:used-price < 2500)
     (:seat-height < 800)
     (:max-power > 15)
     (:max-power < 47)))
;; => ("2024-on CF-moto 450NK" ... "https://www.motorcyclenews.com/bike-reviews/...")
;;    ...
```

## Accelerating Content Ideation
You can also use this tool to help with for inspiration for content ideas, as playing around with the different filters can yield interesting results. For example, think about what would this query output:

```elisp
(query-bikes
 '(and
	 (:average-fuel-consumption > 150)
	 (:top-speed >= 60)))
```

There are many specs that go under-represented across our content that can be used to search too:

```elisp
(query-bikes
 '(and
	 (:average-fuel-consumption > 150)
	 (:top-speed >= 60)))
```
