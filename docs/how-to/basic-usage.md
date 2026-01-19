# Preface
This program is not (yet) interactive, and is designed to be called from within Emacs' terminal environments, namely `M-x eshell` or `M-x ielm`.

# Queries
See [Query Language](../reference/query-language.md) to reference:
- the list of [bike specs](../reference/query-language.md#bike-specs) each bike can be filtered by
- the supported [operators and combinators](../reference/query-language.md#operators--combinators)

The basic syntax of a query looks like this:

```elisp
(mcn/query-bikes                        ;; call the query function
 '(and                                  ;; (optional) combine filters
	 (:average-fuel-consumption > 150)  ;; filter a specific spec
	 (:top-speed >= 60)))               ;; ^^^
;; => ("honda msx125-grom 2014" ... "https://www.motorcyclenews.com/bike-reviews/...")
;;    ...
```

For the sake of ease of use, you don't need to put the unit of a spec (bhp/break horse power, mm/milometers, cc/cubic centimetres, etc), just the number.

To see all the available specs for a certain bike, you can search for it by name:

```elisp
(gethash "honda msx125-grom 2014" bike-review-hashmap)
;; => (:engine-size "125cc" :engine-type "Air-cooled..." :seat-height "765mm" ...)
```

To learn more about Elisp syntax, reference [Introduction to Programming in Emacs Lisp](https://www.gnu.org/software/emacs/manual/html_node/eintr/index.html).

previous: [sadsad]() next: [sadjasdjas]()
