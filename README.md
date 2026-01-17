# Jump to:

-   [2](#*TL;DR)
-   [3](#*See Also)
-   [4](#*Motivations & Background Info)
-   [5](#*The Business Pitch)
-   [6](#*Usage)
-   [7](#*Install)

# TL;DR

-   **Purpose:** Enable faster search of MCN\'s bike reviews, filtering
    by bike specs, to accelerate content ideation.
-   **Tech:** Elisp, CSV parse, URL fetch, DOM node tree traversal,
    hashmap storage, query language/DSL.
-   **Impact:** Reduced manual review, improved content discovery,
    scalable for editorial teams.

# See Also

See this README for high-level workflow and business context, and
example usage.

For reference-style documentation, see:

-   \`query-bikes\` -- Filter and sort bike reviews programmatically.
    Its docstring contains all parameters and return format.
-   \`pc-extract-specs-from-table\` -- Extracts bike specs from HTML
    tables into a structured property list (plist).

These functions form the core of the tool's programmatic API.

# Motivations & Background Info

This is a workflow automation tool I have made while working as a
Commercial Content Writer at [Motorcycle
News](https://www.motorcyclenews.com/bike-reviews/). During a meeting,
one of my colleagues mentioned in passing:

> \"Wouldn\'t it be great to be able to search and filter bike reviews
> by their specs? That would make content ideation so much quicker!\"

At the time, it was left as \"one to sleep on\". Nobody on the team had
enough programming ability to make it a reality, and the business was
notoriously slow with implementing suggested features. As with most
other workflow automation ideas, it was left to gather dust in the
wish-list.

But, instead of sleeping on it, I took it upon myself to build it. This
is a MVP used to pitch a full version to the business.

# The Business Pitch

The Bike Reviews section is one of MCN's highest-value digital assets,
driving significant evergreen traffic and long-tail search demand, yet
editorial teams still spend a significant amount of time manually
searching bike reviews for the purpose of content creation. As each bike
review already has a number of specs associated with them, being able to
search and filter by spec would speed up the content creation process
for a number of different types of content across the Bike Reviews
section.

This tool enables that --- fast, data-driven filtering of [MCN\'s Bike
Reviews section](https://www.motorcyclenews.com/bike-reviews/) by any
spec in the review (fuel economy, horsepower, yearly service cost, etc).
It lets writers and editors instantly generate targeted content ideas by
programmatically finding all bikes that meet a certain condition.

Some example uses include finding:

-   Bikes exceeding 100mpg → "Best fuel-sippers" page
-   Bikes with seat height under 800mm → "Best bikes for shorter riders"
    page
-   Bikes between 15--47bhp and with an MCN score of 4/5 or higher →
    "Best A2 bikes" page

With small additions, the tool could also search for any data in the
bike review page --- collating owners reviews, processing review copy
for relevant phrases (\"great commuter\", \"solid engine\", \"full of
character\", etc). It could also form the basis of a more general-use
application that publications across the business would benefit from.

We also see strong potential for an integration with a LLM (ChatGPT,
Gemini, etc). By combining an AI agent with this consistent searching
and filtering capability, it can generate consistent bike selections,
and produce a first-draft for multiple types of content in seconds. This
would accelerate content creation even further, while ensuring bike
choice isn\'t subject to a LLM\'s tendency for hallucination or
short-cutting.

The current implementation is written in Elisp, chosen for fast
prototyping within my existing workflow. The underlying logic is
portable to any general-purpose programming language.

# Usage

This tool works by:

1.  **Importing a CSV** of all bike reviews
2.  Individually **accessing web pages** for each bike review from the
    live site
3.  **Extracting the spec tables**
4.  **Storing each bike's specs in a hashmap** keyed by bike name, with
    a structured property list of all attributes.

The hashmap returned looks like this:

``` elisp
(gethash "honda msx125-grom 2014" bike-review-hashmap)
;; => (:engine-size "125cc" :engine-type "Air-cooled..." :seat-height "765mm" ...)
```

So an example query to find:

> \"best budget A2 bikes for shorter riders\"

looks like this:

``` elisp
(query-bikes
 '(and
     (:used-price < 2500)
     (:seat-height < 800)
     (:max-power > 15)
     (:max-power < 47)))
;; => ("2024-on CF-moto 450NK" ... "https://www.motorcyclenews.com/bike-reviews/...")
;;    ...
```

For the whole bike reviews section (on my M1 Macbook Air work laptop),
the initial process takes about three minutes. Once bike reviews are
stored in memory, new entries can be added quickly. It takes advantage
of a hashmap\'s fast lookup times, at the cost of about 1.3gb of memory
usage.

# Install

The supplied install script automates install for this tool plus Emacs
on MacOS and Debian/Ubuntu:

``` {.bash org-language="sh"}
git clone https://github.com/musicnark/mcn-bike-reviews-search
cd mcn-bike-reviews-search
./install.sh
```

If installing manually:

-   Clone the repo:

``` {.bash org-language="sh"}
git clone https://github.com/musicnark/mcn-bike-reviews-search
```

-   Move the elisp files to your local emacs elisp executable directory:

``` {.bash org-language="sh"}
mkdir -p ~/.emacs.d/lisp/mcn-bike-reviews-search/ && cp *.el *.csv ~/.emacs.d/elisp/mcn-bike-reviews-search/
```

-   Optionally, you can byte compile the elisp files:

``` {.bash org-language="sh"}
(Idk)
```

-   In your init.el, add the lisp directory to your Emacs path:

``` elisp
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
```

-   When you want to use the package, load it in (it takes about 3
    minutes to load it all):

``` elisp
(require 'bike-reviews)
```

-   By default, it will load a local version of the bikes hashmap. If
    you want to try the download behaviour:

``` elisp
(setq mcn/download-from-live-site t)
```

Then, you can refer to [6](#*Usage) to get started.

# [TODO]{.todo .TODO}  {#section-1}

-   [ ] fix jump-to links (convert from org to md?)
-   [ ] fix documentation
    -   [ ] explain loading the file contents (just C-c C-e? Or wrap the
        code in an init function to make require work?)
    -   [ ] byte compile or no?
-   [ ] finish install script
