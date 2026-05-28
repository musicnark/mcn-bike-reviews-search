# Jump To:
-   [TL;DR](#TL;DR)
-   [See Also](#See-Also)
-   [Motivations & Background Info](#Motivations--Background-Info)
-   [The Business Pitch](#The-Business-Pitch)

# TL;DR
-   **Purpose:** Enable faster search of MCN's bike reviews, filtering by bike specs, to accelerate content ideation.
-   **Tech:** Elisp, CSV parse, URL fetch, DOM node tree traversal, hashmap storage, query language/DSL.
-   **Impact:** Reduced manual review, improved content discovery, scalable for editorial teams.

# See Also
This documentation follows the Diátaxis framework, separating explanation, how-to guides, reference material, and conceptual background.

See this README for the high-level workflow and business context.

For the pitch letter I sent to the wider business, see [the business pitch](./business-pitch.md)

To get started using the tool, see [install](./how-to/install.md).

For example queries, see [basic usage](./how-to/basic-usage.md)

For reference while using the tool, see [query language](./reference/query-language.md).

For an understanding of how and why this was made, see [problem space](./concepts/problem-space.md), [design decisions](./concepts/design-decisions.md), and [constraints](./concepts/constraints.md).

For developer reference documentation, see:
-   [`mcn/query-bikes`](../el/bike-reviews.el) – Filter and sort bike reviews programmatically. Its docstring contains all parameters and return format.
-   [`pc-extract-specs-from-table`](../el/bike-reviews.el) – Extracts bike specs from HTML tables into a structured property list (plist).


# Motivations & Background Info
This is a workflow automation tool I have made while working as a Commercial Content Writer at [Motorcycle News](https://www.motorcyclenews.com/). During a meeting, one of my colleagues mentioned in passing:

> "Wouldn't it be great to be able to search and filter bike reviews by their specs? That would make content ideation so much quicker!"

At the time, it was left as "one to sleep on". Nobody on the team had enough programming ability to make it a reality, and the business was notoriously slow with implementing suggested features. As with most other workflow automation ideas, it was left to gather dust in the wish-list.

But, instead of sleeping on it, I took it upon myself to build it. This is a SLC (Simple, Likeable, and Complete) version of the tool that was pitched to the wider business, with the support of editorial and IT teams.
