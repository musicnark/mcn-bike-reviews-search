# MCN API Postman Artefact

This directory contains a Postman collection for the MCN Bike Reviews Search API.
The collection is generated from `../openapi/mcn-bike-reviews-search-api.openapi.yaml`,
then manually reviewed/adjusted.

Generated from OpenAPI, then manually reviewed/adjusted.

## Coverage

The artefacts cover the strongest public API endpoints first:

- health checks
- searchable field metadata
- paginated bike review summaries
- individual bike review lookup
- random bike review lookup
- bike review search

The collection includes a concrete search request body and basic response checks
for successful status codes, JSON responses, and expected search result keys.

## Limits

These artefacts are intentionally not exhaustive. They demonstrate API
documentation structure and core request/response shapes, but do not attempt to
document every source data field, every data quality edge case, or every possible
error response.

## Validation

The OpenAPI file was validated with Redocly CLI:

```sh
npx @redocly/cli lint docs/openapi/mcn-bike-reviews-search-api.openapi.yaml
```

The Postman collection was checked as valid JSON after generation and manual
adjustment:

```sh
node -e "JSON.parse(require('fs').readFileSync('docs/postman/mcn-api.postman_collection.json', 'utf8'))"
```
