# Emacs
Emacs is a good platform insofar as it's cross-platform, and brings potential new users into the church. It requires learning a new tool for many target users though, which can add friction to their workflows initially - particularly in its current command-line based implementation. Emacs also doesn't integrate well with many business tools such as Monday.com, WordPress, etc., so the overall project scope is limited by the choice of basing it on Emacs. Emacs as the base is only viable for the MVP, not a full business integration.

Using Homebrew for installation on MacOS takes a long time too, as users will likely need to install Xcode developer tools.

# Design limitations
In its current state this tool does have some limitations stemming from the design:

If fetching URLs from the live site, this tool fetches them synchronously (one at a time), which is slow compared to asynchronous methods. Emacs does have asynchronous URL fetching available, but to prioritise speed of development I decided against implementing it in this tool.

The URLs are also pre-defined in a pre-formatted CSV file, which was manually downloaded from our organisation board (Monday.com) and manually formatted. Additions to the code that integrate with the Monday.com API and/or automatically format the CSV contents would be ideal, but weren't practical to achieve the pace of development I was after. A different algorithm could be used to scrape the live site, but legal implications related to web scraping would have to be considered.

As well as this, a database may be more suitable than a hash map for storing the page data, but for speed of development I also decided against this. The downloaded web pages use about 800mb of memory when fully loaded, which could noticeably slow down a laptop, even if the hash map of specs alone uses less than 5mb. Ideally, the web pages would not need to be stored locally at all.

This tool also relies on the live site not changing its formatting, which is inherently fragile. This is mitigated by my own knowledge of the site, as it hasn't changed its front-end format for as long as I've worked at MCN, but if it was changed an update to the code would be needed to retain functionality.
