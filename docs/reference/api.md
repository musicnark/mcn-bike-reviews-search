# Query Structure
A typical query is represented in structured data. There is one field with two optional parameters; :filter, and optionally :sort, and :limit.

```clojure
{:filter {:type "comparison"
             :field "fuel-capacity"
             :op "<"
             :value 5}
    :sort {:field "bike-weight"
           :direction "desc"}
    :limit 10}
```

# Limitations
