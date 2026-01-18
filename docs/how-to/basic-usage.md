# Preface
This program is not (yet) interactive, and is designed to be called from within Emacs' terminal environments like `eshell` or `ielm`.

# Queries
The basic syntax of a query looks like this:

```elisp

```

To see all the available specs for a certain bike, you can search for it by name:

```elisp
(gethash "honda msx125-grom 2014" bike-review-hashmap)
;; => (:engine-size "125cc" :engine-type "Air-cooled..." :seat-height "765mm" ...)
```
