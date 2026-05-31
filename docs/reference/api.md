# Running The API

Start the API locally with the built-in alias:

```sh
clojure -M:api
```

This will start the server at:

http://localhost:3000

You can optionally add a custom port via an environment variable:

```sh
PORT=9999 clojure -M:api
```

# Response Format

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

# Endpoints

## GET /api

This endpoint serves as the API index. The expected response should give some basic information about the API:

```json
{
  "name": "MCN Bike Reviews Search API",
  "description": "Foo Bar Baz",
  "endpoints": ["/health", "/api/fields", "/api/bikes", "/api/search"]
}
```

## GET /api/health

Purpose: check cache/server health.

Responses:

```json
{
  "status": "ok",
  "cacheLoaded": true
}
```

```json
{
  "status": "degraded",
  "cacheLoaded": false
}
```

## GET /api/fields

Purpose: list valid searchable fields.

Example response:

{
  "fields": ["seat-height", "engine-size", "used-price"]
}

Note: field names are kebab-case because they are query input values.

## GET /api/bikes

Purpose: paginated bike summaries.

Query params:

┌──────────┬─────────┬─────┬──────────────────┐
│ Name     │ Default │ Max │ Description      │
├──────────┼─────────┼─────┼──────────────────┤
│ page     │       1 │ n/a │ Page number      │
│ per-page │      25 │ 100 │ Results per page │
└──────────┴─────────┴─────┴──────────────────┘

Example:

curl "http://localhost:3000/api/bikes?page=1&per-page=25"

Response shape:

{
  "results": [
    {
      "id": "yamaha-xjr1300-2015",
      "bikeName": "yamaha-xjr1300-2015",
      "mcnRating": "4",
      "url": "https://..."
    }
  ],
  "page": 1,
  "perPage": 25,
  "count": 25,
  "total": 1489
}

## GET /api/bikes/:id

To get full bike details for a specific bike, add the `:id` of a bike after `bikes` in the address.

Example:

curl "http://localhost:3000/api/bikes/suzuki-rv125-van-van-2003"

On success, the response should look like this:

```json
{
  "bikeName": "suzuki-rv125-van-van-2003",
  "engineSize": "124cc",
  "frontTyreSize": "130/80 x 18",
  "frontBrake": "220mm disc",
  "tankRange": "150 miles",
  "mcnRating": "3",
  ...
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

## POST /api/search

Purpose: query bike specs.

Headers:

Content-Type: application/json

Request body shape:

{
  "filter": {},
  "sort": {},
  "limit": 25
}

Then document filters.

## GET /api/random

Get full bike details for a random bike in the dataset. Returns the same shape as `/api/bikes/:id`

curl "http://localhost:3000/api/random"

# Search Filters

## Comparison

{
  "type": "comparison",
  "field": "seat-height",
  "op": "<",
  "value": 800
}

Operators:

< > <= >= =

## And

{
  "type": "and",
  "clauses": []
}

## Or

{
  "type": "or",
  "clauses": []
}

## Not

{
  "type": "not",
  "clause": {}
}

# Sorting

{
  "field": "used-price",
  "direction": "asc"
}

Directions:

asc desc

# Limits

Explain:

- default search limit: 25
- max search limit: 100
- max body size: 65536 chars
- max query depth: 10
- max compound clauses: 25

# Examples

Include 3-5 useful examples:

- low seat height
- high engine size sorted by price
- lightweight and powerful
- high MCN rating
- not tall bikes

# Error Types

Table:

┌──────────────────┬────────┬──────────────────────────────────────────────────────────┐
│ Type             │ Status │ Meaning                                                  │
├──────────────────┼────────┼──────────────────────────────────────────────────────────┤
│ invalid-query    │    400 │ Query shape, field, operator, sort, or limit was invalid │
│ invalid-json     │    401 │ Request body was missing or malformed                    │
│ body-too-large   │    413 │ JSON body exceeded max size                              │
│ not-found        │    404 │ Unknown route                                            │
│ bike-not-found   │    404 │ Unknown bike ID                                          │
│ cache-not-loaded │    503 │ Bike cache failed to load                                │
└──────────────────┴────────┴──────────────────────────────────────────────────────────┘

# Notes

Mention:

- API is read-only.
- Data comes from local cached bike review data.
- Search field names are kebab-case.
- Response keys are camelCase.
