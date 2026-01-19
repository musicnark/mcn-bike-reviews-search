# Overview
The grammar for constructing a query is as follows:


# Examples
example-queries.el

# Bike Specs
The list of bike specs available to filter by are listed as follows:

Spec    Unit of Measurement (if filterable)
`:engine-size` - "cc"
`:engine-type` - `TBC`
`:frame-type` - `TBC`
`:fuel-capacity` - "litres"
`:seat-height` - "mm"
`:bike-weight` - "kg"
`:front-suspension` - `TBC`
`:rear-suspension` - `TBC`
`:front-brake` - `TBC`
`:rear-brake` - `TBC`
`:front-tyre-size` - `TBC`
`:rear-tyre-size` - `TBC`

`:average-fuel-consumption` - "mpg"
`:annual-road-tax` - "£"
`:annual-service-cost` - "£"
`:new-price` - "£"
`:used-price` - "£"
`:insurance-group` - "0 of 0"
`:warranty-term` - "years"

`:max-power` - "bhp"
`:max-torque` - "ft-lb"
`:top-speed` - "mph"
`:1/4-mile-acceleration` - "secs"
`:tank-range` "miles"

`TBC` denotes a spec that can't yet be filtered by this tool.

# Operators & Combinators
Numerical comparison operators are supported:

`<` - less than
`>` - greater than
`=` - equal to

`<=` - less than or equal to
`>=` - greater than or equal to

Queries can be combined with three combinators:

`and` - all filters must return true
`or` - at least one filter must return true
`not` - returns the inverse
Each spec can be filtered by 

# Expected Errors
The errors expected by the program are as follows:
- fetching a URL may return 'nil' if it encounters any error (e.g., no internet connection), but that page will be skipped and the process will continue
- parsing the page contents will return 'nil' if the sub-page was not found, but that page will be skipped and the process will continue
- when extracting specs from the HTML <table> node, the function will raise an error if it's not given a HTML <table> node, or if no <tbody> tag is found within it.
