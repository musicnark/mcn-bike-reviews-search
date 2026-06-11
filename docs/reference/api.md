# API Reference

## Preface

Some code examples in this documentation use `// ...` comments to keep long responses readable. These comments are illustrative only; make sure to remove them before copying a JSON body into a request.

## Jump To

- [Running the API](#running-the-api)
- [Response Format](#response-format)
- [Endpoints](#endpoints)
- [Search Filters](#search-filters)
- [Sorting](#sorting)
- [Limits](#limits)
- [Examples](#examples)
- [Error Types](#error-types)
- [Notes](#notes)

## Running The API

### Live Demo

The API is available to access as a live demo at https://labs.musicnark.com/mcn/api.

For example:

```sh
curl https://labs.musicnark.com/mcn/api
```

This uses `/mcn` as its base path. Therefore, the endpoint paths documented below should be requested as `/mcn/endpoint` on the live demo.

For example:

```sh
curl https://labs.musicnark.com/mcn/api/bikes/random
```

The live demo is rate limited, so run the API locally for unrestricted testing or development.

### Local

To run the API locally, you will need to have `clojure` and Java/JDK (version 11+) installed and available in your `PATH`.

Start the API locally with the built-in alias:

```sh
clojure -M:api
```

This will load the default cache, and start the server at http://localhost:3000.

You can optionally add a custom port via an environment variable:

```sh
PORT=9999 clojure -M:api
```

You can also refresh the data set before running the API with:

```sh
clojure -M:refresh
```

Refreshing overwrites the default cache, unless a custom path is specified:

```sh
clojure -M:refresh /tmp/bikes.edn
```

Refreshing requires internet access, and may take some time.

## Response Format

The response is formatted in JSON, with camelCase response keys. See [Endpoints](#endpoints) for example output per-endpoint.

The standard error shape follows this convention:

```json
{
  "error": {
    "type": "invalid-query",
    "message": "Unknown field: foo"
  }
}
```

Note that some display values reflect inconsistencies in the source dataset. While [a fix is tracked](https://github.com/musicnark/mcn-bike-reviews-search/issues/12), in the meantime users should:
- treat `"-"` as unavailable data
- trim leading and trailing whitespace from `usedPrice`
- treat malformed `insuranceGroup` values as unavailable

`"-"` values are placed first when sorting in `asc` order, so they currently need to be skipped manually. The output format is also affected, but it does not affect the accuracy of filtering, as these values are normalised internally.

## Endpoints

### GET /api

This endpoint serves as the API index. The expected response should give some basic information about the API:

```json
{
  "name": "MCN Bike Reviews Search API",
  "description": "Backend API for searching the Motorcycle News bike review archive by bike specs",
  "apiDocsLink": "https://github.com/musicnark/mcn-bike-reviews-search/blob/main/docs/reference/api.md",
  "bikeCount": 1489,
  "endpoints": {
    "health": "/api/health",
    "fields": "/api/fields",
    "bikes": "/api/bikes",
    "randomBike": "/api/bikes/random",
    "bikeSearch": "/api/bikes/search"
  }
}
```

### GET /api/health

Check the status of the bike reviews cache and server health:

If all is well:

```json
{
  "status": "ok",
  "cacheLoaded": true
}
```

Otherwise:

```json
{
  "status": "degraded",
  "cacheLoaded": false
}
```

### GET /api/fields

Generates a list of all valid fields for constructing filters within a query.

Prints in alphabetical order:

```json
{
  "fields": [
    {
      "name": "annual-road-tax",
      "type": "num",
      "unit": "GBP"
    },
    {
      "name": "annual-service-cost",
      "type": "num",
      "unit": "GBP"
    },
	// ...
    {
      "name": "warranty-term",
      "type": "num",
      "unit": "years"
    }
  ]
}
```

The type denotes which filter type is supported on that field:
- `num` values are filterable using the `comparison` type in a filter. You can use either an `int` or `float`.
- `string` values are filterable using the `contains` type in a filter. _(not yet implemented)_

Note that field names are returned kebab-case, as this is the expected input format for a filter within a query. Response keys from a processed query are returned camelCase.

### GET /api/bikes

Returns a paginated list of bike summaries.

Two parameters are supported in the HTML query string:

| Name       | Default | Max | Description      |
|------------|---------|-----|------------------|
| `page`     | 1       | n/a | Page number      |
| `per-page` | 25      | 100 | Results per page |

For example, running this command:

```sh
curl "http://localhost:3000/api/bikes?page=1&per-page=25"
```

The expected response shape would be:

```json
{
  "results": [
    {
      "bikeName": "yamaha-xjr1300-2015",
      "url": "https://..."
	  // ...
    }
  ],
  "page": 1,
  "perPage": 25,
  "count": 25,
  "total": 1489
}
```

### GET /api/bikes/:id

Get full bike details for a specific bike by adding its `:id` at the `bikes` endpoint.

For example:

```sh
curl "http://localhost:3000/api/bikes/suzuki-rv125-van-van-2003"
```

On success, the response should look like this:

```json
{
  "bikeName": "suzuki-rv125-van-van-2003",
  "engineSize": "124cc",
  "frontTyreSize": "130/80 x 18",
  "frontBrake": "220mm disc",
  "tankRange": "150 miles",
  "url": "https://www.motorcyclenews.com/bike-reviews/suzuki/rv125-van-van/2003/"
  // ...
}
```

On failure, like this:

```json
{
  "error": {
    "type": "bike-not-found",
    "message": "Bike not found: suzuki-rv125-von-von-2003"
  }
}
```

### GET /api/bikes/random

Get full bike details for a random bike in the dataset. Returns the same response shape as searching for a bike by its `:id`.

For example:

```sh
curl "http://localhost:3000/api/bikes/random"
```

### POST /api/bikes/search

Submit a query to search the bike cache, returning all bikes that match the query.

Use the following headers with your query:

```http
Content-Type: application/json
```

The request body shape looks like this:

```json
{
  "filter": {
  // ...
  },
  "sort": {
  // ...
  },
  "limit": 25
}
```

Your request body should contain at least a `filter` at minimum. You can optionally provide a field to `sort` by, and `limit` how many matching bikes are returned.

If `limit` is omitted, the API returns up to 25 matching bikes by default. If `limit` is higher than 100, the API caps it at 100.

See [Search Filters](#search-filters) for the supported filters.

See [Sorting](#sorting) for the sort syntax.

See [Limits](#limits) for more info on the limit option.

See [Examples](#examples) for some example queries.

## Search Filters

### Comparison

The comparison filter compares between `num` fields:

```json
{
  "type": "comparison",
  "field": "seat-height",
  "op": "<",
  "value": 800
}
```

```json
{
  "type": "comparison",
  "field": "fuel-capacity",
  "op": ">=",
  "value": 15.5
}
```

The supported operators are:

`<` `>` `<=` `>=` `=`

### Contains

**Not yet implemented**

### And

Chain filters together by wrapping them in `and`:

```json
{
  "type": "and",
  "clauses": []
}
```

### Or

Return a bike if one `or` more of its clauses match:

```json
{
  "type": "or",
  "clauses": []
}
```

### Not

Exclude bikes that match a filter with `not`:

```json
{
  "type": "not",
  "clause": {}
}
```

## Sorting

Sorting must be done by field:

```json
{
  "field": "used-price",
  "direction": "asc"
}
```

Sort direction can be either `asc` or `desc`:

```json
{
  "field": "average-fuel-consumption",
  "direction": "desc"
}
```

## Limits

The API applies a small set of limits to keep requests predictable and cheap to process:

| Limit                    | Value       | Applies To               | Meaning                                                             |
|--------------------------|-------------|--------------------------|---------------------------------------------------------------------|
| Default search limit     | 25          | `POST /api/bikes/search` | Used when a search request does not include `limit`                 |
| Maximum search limit     | 100         | `POST /api/bikes/search` | Larger requested limits are capped at 100                           |
| Maximum body size        | 65536 chars | `POST /api/bikes/search` | Larger JSON request bodies return `body-too-large`                  |
| Maximum query depth      | 10          | Search filters           | Heavily nested `and`, `or`, or `not` filters return `invalid-query` |
| Maximum compound clauses | 25          | `and` / `or` filters     | More than 25 clauses in one compound filter returns `invalid-query` |

For example, this request asks for 1000 results:

```json
{
  "filter": {
    "type": "comparison",
    "field": "seat-height",
    "op": "<",
    "value": 800
  },
  "limit": 1000
}
```

The API will process it as though `limit` were `100`.

## Examples

### "Budget A2 Bikes For Shorter Riders"

Find bikes under £2,500, within the A2 license category, and a seat height below 800mm:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "used-price",
        "op": "<",
        "value": 2500
      },
      {
        "type": "comparison",
        "field": "max-power",
        "op": "<",
        "value": 47
      },
      {
        "type": "comparison",
        "field": "max-power",
        "op": ">",
        "value": 15
      },
      {
        "type": "comparison",
        "field": "seat-height",
        "op": "<",
        "value": 800
      }
    ]
  },
  "sort": {
    "field": "used-price",
    "direction": "asc"
  },
  "limit": 25
}
```

### "Fuel-Efficient But Motorway-Capable A1 Bikes"

Find bikes with very high fuel economy that can still reach at least 60mph:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "average-fuel-consumption",
        "op": ">",
        "value": 150
      },
      {
        "type": "comparison",
        "field": "top-speed",
        "op": ">=",
        "value": 60
      }
    ]
  },
  "sort": {
    "field": "average-fuel-consumption",
    "direction": "desc"
  },
  "limit": 10
}
```

### "Cheap Runners"

Find bikes with low service costs, low insurance group, low road tax, and a used price under £2,500:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "annual-service-cost",
        "op": "<=",
        "value": 100
      },
      {
        "type": "comparison",
        "field": "insurance-group",
        "op": "<",
        "value": 7
      },
      {
        "type": "comparison",
        "field": "annual-road-tax",
        "op": "<",
        "value": 50
      },
      {
        "type": "comparison",
        "field": "used-price",
        "op": "<",
        "value": 2500
      }
    ]
  },
  "sort": {
    "field": "used-price",
    "direction": "asc"
  },
  "limit": 20
}
```

### "Best Mid-Capacity Mile-Munchers"

Find bikes with an engine size between 600-900cc, and a tank range over 200 miles:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "engine-size",
        "op": ">=",
        "value": 600
      },
      {
        "type": "comparison",
        "field": "engine-size",
        "op": "<=",
        "value": 900
      },
      {
        "type": "comparison",
        "field": "tank-range",
        "op": ">",
        "value": 200
      }
    ]
  },
  "sort": {
    "field": "tank-range",
    "direction": "desc"
  },
  "limit": 10
}
```

### "MCN's Favourite A-Class Bikes"

Find bikes with over 47bhp and a 5-star MCN rating:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "max-power",
        "op": ">",
        "value": 47
      },
      {
        "type": "comparison",
        "field": "mcn-rating",
        "op": "=",
        "value": 5
      }
    ]
  },
  "sort": {
    "field": "used-price",
    "direction": "asc"
  },
  "limit": 10
}
```

### "Bargain Bikes For Speed Demons"

Find bikes under £5,000 that can reach at least 180mph:

```json
{
  "filter": {
    "type": "and",
    "clauses": [
      {
        "type": "comparison",
        "field": "used-price",
        "op": "<",
        "value": 5000
      },
      {
        "type": "comparison",
        "field": "used-price",
        "op": ">",
        "value": 0
      },
      {
        "type": "comparison",
        "field": "top-speed",
        "op": ">=",
        "value": 180
      }
    ]
  },
  "sort": {
    "field": "used-price",
    "direction": "asc"
  },
  "limit": 10
}
```

## Error Types

| Type               | Status | Meaning                                                  |
|--------------------|--------|----------------------------------------------------------|
| `invalid-query`    | 400    | Query shape, field, operator, sort, or limit was invalid |
| `invalid-json`     | 400    | Request body was missing or malformed                    |
| `body-too-large`   | 413    | JSON body exceeded max size                              |
| `not-found`        | 404    | Unknown route                                            |
| `bike-not-found`   | 404    | Unknown bike ID                                          |
| `cache-not-loaded` | 503    | Bike cache failed to load                                |

## Notes

This API is read-only. Data comes from a local cached hash-map of bike review data.
