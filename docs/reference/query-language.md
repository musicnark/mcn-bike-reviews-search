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

`TBC` denotes a spec that can't yet be filtered by.

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
