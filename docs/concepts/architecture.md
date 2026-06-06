# Architecture Overview

MCN Bike Reviews Search is organised as a small data pipeline and JSON API. Each layer has a focused responsibility so data collection, querying, and HTTP delivery can be developed and tested independently.

```text
MCN sitemap
    |
    v
URL discovery (`mcn.sitemap`)
    |
    v
Concurrent HTTP fetch and HTML parse pipeline (`mcn.fetch`, `mcn.parse`, `mcn.pipeline`)
    |
    v
Local EDN cache and retry handling (`mcn.storage`, `mcn.retry`)
    |
    v
Query engine with filter, sort, and limit operations (`mcn.query`)
    |
    v
API layer with validation and JSON endpoints (`mcn.api`, `mcn.server`)
```

## Data Collection

`mcn.sitemap` parses bike review URLs from MCN's sitemap. The fetch and parse pipeline downloads review pages concurrently, extracts their specification tables, and returns the results as structured data. Failures remain explicit so they can be inspected or retried, rather than silently discarded.

## Storage

The collected review data is stored as structured data (EDN), and loaded into memory when the API starts. This keeps the current version simple and makes local demonstrations fast. A database would become useful if the project needed incremental updates, multiple writers, or larger/rapidly growing datasets.

## Query Layer

`mcn.query` evaluates nested comparison filters, supporting `and`, `or`, and `not`. It also supports numeric sorting and setting limits on result.

## API Layer

`mcn.api` exposes health, metadata, listing, lookup, random selection, and search endpoints through Ring. It validates request shape, allowed fields, operators, nesting depth, clause counts, sort options, limits, and request body size before evaluating a query.

`mcn.server` loads the cached data and serves the application through embedded Jetty.

## Testing

The test suite covers the core pipeline, storage, retry behaviour, query evaluation, API responses, pagination, and validation. Run it with:

```sh
clojure -X:test
```

## Key Considerations Overview

- **In-memory EDN rather than a database:** appropriate for a local, read-focused application; less suitable for concurrent or incremental writes.
- **REST rather than GraphQL:** keeps the current single-purpose API small and predictable.
- **core.async pipeline:** fits the producer-consumer shape of fetching and parsing many review pages.
- **Clojure rewrite after an Elisp prototype:** preserves rapid validation of the original idea while making the service easier to run independently.

See [Design Decisions](./design-decisions.md) for more detail on these.
