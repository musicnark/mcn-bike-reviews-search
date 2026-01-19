# Limitations
If fetching URLs from the live site, this tool fetches them synchronously (one at a time), which is quite slow compared to asynchronous methods. Emacs does have asynchronous URL fetching available, but to prioritise speed of development I decided against implementing it in this tool.

As well as this, a database would be more suitable than a hash map for storing the page data, but for speed of development I also decided against this. This tool does use about 1.3gb of memory when fully loaded, which will noticeably slow down a laptop. It also relies on the live site not changing its formatting, which is inherently fragile.
