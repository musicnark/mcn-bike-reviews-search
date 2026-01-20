# Usage Overview
This tool does not (yet) feature a graphical interface. Before continuing, be sure to channel your inner hacker.

This tool currently operates best within Emacs' terminal environments. Run `M-x eshell` or `M-x ielm`, and once you're there, type out a query and hit enter.

# Queries
The basic syntax of a query looks like this:

```elisp
(mcn/query-bikes                        ;; call the query function
 '(and                                  ;; (optional) combine filters
	 (:average-fuel-consumption > 150)  ;; filter a specific spec
	 (:top-speed >= 60)))               ;; ^^^
 ```

_Don't forget to put the single quote `'` before your filter!_

For the sake of ease of use, you don't need to put the unit of a spec (bhp/break horse power, mm/milometers, cc/cubic centimetres, etc), just the number.
 
Running the above will produce an output similar to this:

```elisp
("honda msx125-grom 2014" ... "https://www.motorcyclenews.com/bike-reviews/...")
;;    ...
```

By default, the results display from top → bottom, **low → high**.

To see all the available specs for a certain bike, you can search for it by name with the following command:

```elisp
(gethash "honda msx125-grom 2014" bike-review-hashmap)
;; => (:engine-size "125cc" :engine-type "Air-cooled..." :seat-height "765mm" ...)
```
See [Query Language](../reference/query-language.md) to reference:
- the list of [bike specs](../reference/query-language.md#bike-specs) each bike can be filtered by
- the supported [operators and combinators](../reference/query-language.md#operators--combinators)

See the original [CSV file](../../Bike_Reviews.csv) to find each bike's name.

To learn more about basic Elisp syntax, reference [Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html).
