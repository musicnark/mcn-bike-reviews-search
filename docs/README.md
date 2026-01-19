# Jump To:
-   [TL;DR](#tldr)
-   [See Also](#see-also)
-   [Motivations & Background Info](#motivations--background-info)
-   [The Business Pitch](#the-business-pitch)

# TL;DR
-   **Purpose:** Enable faster search of MCN's bike reviews, filtering by bike specs, to accelerate content ideation.
-   **Tech:** Elisp, CSV parse, URL fetch, DOM node tree traversal, hashmap storage, query language/DSL.
-   **Impact:** Reduced manual review, improved content discovery, scalable for editorial teams.

# See Also
This documentation follows the Diátaxis framework, separating explanation, how-to guides, reference material, and conceptual background.

- See this README for the high-level background info and business context.
- To get started using the tool, see [install](./how-to/install.md).
- For example queries, see [basic usage](./how-to/basic-usage.md)
- For reference while using the tool, see [query language](./reference/query-language.md).
- For an understanding of how and why this was made, see [problem space](./concepts/problem-space.md), [design decisions](./concepts/design-decisions.md), and [constraints](./concepts/constraints.md).
- To learn who this tool is for, and who it's _not_ for, see [audience](./audience.md).
- For developer reference documentation, see:
  -   \`[mcn/query-bikes](./bike-reviews.el#L166)\` – Filter and sort bike reviews programmatically. Its docstring contains all parameters and return format.
  -   \`[pc-extract-specs-from-table](./bike-reviews.el#L48)\` – Extracts bike specs from HTML tables into a structured property list (plist).

# Motivations & Background Info
This is a workflow automation tool I have made while working as a Commercial Content Writer at [Motorcycle News](https://www.motorcyclenews.com/bike-reviews/). During a meeting, one of my colleagues mentioned in passing:

> "Wouldn't it be great to be able to search and filter bike reviews by their specs? That would make content ideation so much quicker!"

At the time, it was left as "one to sleep on". Nobody on the team had enough programming ability to make it a reality, and the business was notoriously slow with implementing suggested features. As with most other workflow automation ideas, it was left to gather dust in the wish-list.

But, instead of sleeping on it, I took it upon myself to build it. This is a MVP used to pitch a full version to the business.

# The Business Pitch
The Bike Reviews section is one of MCN’s highest-value digital assets, driving significant evergreen traffic and long-tail search demand, yet editorial teams still spend a significant amount of time manually searching bike reviews for the purpose of content creation. As each bike review already has a number of specs associated with them, being able to search and filter by spec would speed up the content creation process for a number of different types of content across the Bike Reviews section.

This tool enables that — fast, data-driven filtering of [MCN's Bike Reviews section](https://www.motorcyclenews.com/bike-reviews/) by any spec in the review (fuel economy, horsepower, yearly service cost, etc). It lets writers and editors instantly generate targeted content ideas by programmatically finding all bikes that meet a certain condition. 

Some example uses include finding:

-   Bikes exceeding 100mpg → “Best fuel-sippers” page
-   Bikes with seat height under 800mm → “Best bikes for shorter riders” page
-   Bikes between 15–47bhp and with an MCN score of 4/5 or higher → “Best A2 bikes” page

With small additions, the tool could also search for any data in the bike review page — collating owners reviews, processing review copy for relevant phrases ("great commuter", "solid engine", "full of character", etc). It could also form the basis of a more general-use application that publications across the business would benefit from.

We also see strong potential for an integration with a LLM (ChatGPT, Gemini, etc). By combining an AI agent with this consistent searching and filtering capability, it can generate consistent bike selections, and produce a first-draft for multiple types of content in seconds. This would accelerate content creation even further, while ensuring bike choice isn't subject to a LLM's tendency for hallucination or short-cutting.

The current implementation is written in Elisp, chosen for fast prototyping within my existing workflow. The underlying logic is portable to any general-purpose programming language.
