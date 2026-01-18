# Basic Queries
A query consists of three parts:

```elisp
(query-bikes
 '(and
	 (:average-fuel-consumption > 150)
	 (:top-speed >= 60)))
```
