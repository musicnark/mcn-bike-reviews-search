# Documentation Structure

This documentation follows the Diátaxis standard to meet the needs of
different readers. It includes practical workflows for editorial users,
reference material for developers, and explanations of the business problem
and technical decisions.

# Platform Evolution

The project began as an Elisp prototype because Emacs was already part of the
author's daily editorial workflow. That choice prioritised prototyping speed
and made it possible to demonstrate a useful version within 24 hours.

The current implementation is a standalone Clojure service. Moving beyond the
editor removed the main barrier to wider use and created space for a documented
JSON API, stronger validation, automated tests, and a future browser-based
interface.

The original prototype is retained in [`archive/`](../../archive/) to show how
the project evolved after validating the business need.

# Algorithms And Data Structures

At a high level, the current service:

1. Fetches and parses the MCN sitemap to discover bike review URLs.
2. Fetches review pages asynchronously.
3. Parses specification tables into structured bike records.
4. Stores successful and failed results explicitly in a map keyed by bike ID.
5. Saves the dataset to a local EDN cache.
6. Exposes query and lookup operations through a JSON API.

For a more detailed view, see the
[architecture overview](./architecture.md).

# Async
Although new async patterns offered by libraries like Manifold/Aleph are popular within the Clojure community, I decided to stick with core.async for this project. The flow of data from HTTP request to HTML parsing maps well to a producer-consumer pipeline, which core.async handles well. Manifold is best when you need a single abstraction to interface between multiple async implementations (e.g., service aggregation, Java interop), which wasn't necessary for this project.

# API
I decided to use a REST API for this project, instead of newer technologies like GraphQL. GraphQL works best when you have multiple consumers with different data needs from the same API, and that's outside the scope of this project. The existing DSL implementation allows querying for exact sub-sets of data, and can be used effectively by both developers and users.
