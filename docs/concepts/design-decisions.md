# Documentation Structure
This documentation follows the Diátaxis standard, in order to meet the needs of many different potential users from one set of code docs. The current documentation practice equally serves non-technical editorial teams using this tool to save time, as much as IT departments who might wish to re-implement its functionality.

# Platform
I decided to write this tool for Emacs, in Emacs Lisp. With all that in mind, I decided to use the tools I'm most fluent with, to prioritise prototyping speed, and Emacs is the tool I use most in my day-to-day role as a writer.

This tool has also been designed such that, should the need arise, an expanded and more user-friendly version can be made quickly and distributed within the wider team. This is because I anticipated the business would take a long time to implement the tool in their way, but the value this tool adds is still worth having today. Emacs has a perfectly functional graphics library that's easy enough to work with, so a custom mode that loads this tool with a simple GUI would be serviceable for small-scale distribution within my team. 

Emacs was also chosen as it's a cross-platform tool with an existing open-source ecosystem of tools for editorial teams. Emacs gives easy access to common system needs, like the system clipboard, and integrates well with conversion tools to output to any format desired. Being cross platform, it also avoids doubling up on work to distribute across Windows and MacOS.

Knowing that a full rewrite would have to take place to scale to business needs, the underlying logic was kept as portable as possible. In its MVP state, there's no functionality in this tool unique to Emacs that couldn't be easily transferred to any other general purpose programming language. E.g., PHP for a WordPress plugin, Python for a script leveraging Pandas for data retrieval, JavaScript for a web browser plugin, etc.

# Algorithms & Data Structures
At a high level, this tool works by:

1.  **Importing a CSV** of all bike reviews
2.  Individually **accessing web pages** for each bike review from the live site
3.  **Extracting the spec tables**
4.  **Storing each bike’s specs in a hash map** keyed by bike name, with a structured property list of all attributes.
5. Giving the user a **query language to search and filter the hash map's contents**.

For a more granular look at the design, see the [main code](../../bike-reviews.el).

Most Elisp functions operate at C-like speed, and the language lends itself naturally to recursive tree algorithms - which were the most challenging part of this tool to implement. Elisp also has highly ergonomic string handling with the use of buffers and editor commands, which made parsing the input file quick and easy to implement.
