# Pre-fetched data
For convenience, a local version of the data has been saved in the repository as the default source. It is up to date as of 29th May 2026.

# motorcyclenews.com
This tool can access the front-end of the live website (https://motorcyclenews.com/bike-reviews/) to fetch data from with the included alias:

```sh
clojure -M:refresh
```

You can optionally add an alternative path for the cache to be saved:

```sh
clojure -M:refresh /tmp/bikes.edn
```

Please follow all applicable laws when accessing web content in this way.
