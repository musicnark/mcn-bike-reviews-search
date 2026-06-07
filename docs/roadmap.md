# Roadmap

The core backend API, nested query handling, input validation, local cache, automated test suite, and API documentation are feature-complete.

## Near-Term

- [ ] Add partial string matching for text fields
- [ ] Improve data-quality checks and reporting
- [ ] Support incremental updates when new reviews appear
- [ ] Add structured application logging
- [X] Add continuous integration for the test suite

## Product Development

- [ ] Build a browser-based interface for editorial users
- [ ] Add saved or shareable searches for recurring editorial workflows
- [ ] Evaluate PostgreSQL for incremental updates and hosted deployments
- [ ] Containerise the service for repeatable deployment

## Documentation

- [x] Add a root quickstart and portfolio overview
- [x] Document the current architecture
- [x] Document the JSON API
- [ ] Add an end-to-end editorial case study
- [X] Add deployment documentation (when a hosted version is available)

## Foundations

- [x] Discover review URLs from the MCN sitemap
- [x] Fetch and parse review pages asynchronously
- [x] Store and load the structured bike dataset
- [x] Implement comparison, nested boolean filters, sorting, and limits
- [x] Expose health, metadata, listing, lookup, random, and search endpoints
- [x] Validate public API input and return consistent errors
- [x] Cover core behaviour with automated tests
