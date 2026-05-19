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
