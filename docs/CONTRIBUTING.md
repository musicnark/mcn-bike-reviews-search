# Contributing guidelines
Contributions are welcome in any way, shape, or form, so long as you can justify that it adds value to _this_ project. In particular, documentation improvements are encouraged. Bug fixes or code improvements are also welcome. Any significant changes should be opened as an issue and discussed first, to avoid spending time on a change that would better serve another project. Otherwise, submit a PR for review, and thanks in advance for your contribution!

If you're not a developer, you can still contribute by posting issues when you run into problems using the tool. Be as descriptive as you can with any issues relating to usage, outlining what happened and what led up to it.

Some encouraged contributions include:
- documentation improvements (run `vale docs/` for some easy wins)
- reporting bugs and unexpected behaviour
- adding support for more systems in [install.sh](../install.sh)
- adding unit tests (for completeness)

# Setting up the project
To get started, clone the repository:

```sh
git clone https://github.com/musicnark/mcn-bike-reviews-search/
```

Setting up `Vale` is recommended for prose linting. Install `Vale` for your operating system, `cd` to the project root, and run:

```sh
vale sync
```
