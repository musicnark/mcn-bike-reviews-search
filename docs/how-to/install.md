# Install

The recommended install method is via the supplied \`[install script](../../install.sh)`\, which automates install for this tool and Emacs on MacOS, Debian, and Ubuntu:

``` bash
git clone https://github.com/musicnark/mcn-bike-reviews-search
cd mcn-bike-reviews-search
chmod +x install.sh
./install.sh
```
Once installed, launch or restart Emacs and run `M-x mcn/bike-search-initialise` to load the data in.

Then, you can get started with some [basic usage](./basic-usage.md).

If you're unfamiliar with Emacs, now's a good time to [familiarise yourself with the basics](https://www.gnu.org/software/emacs/tour/) before continuing.

---

If installing manually, open a terminal and follow the below instructions:

-   Clone the repo:

``` bash
git clone https://github.com/musicnark/mcn-bike-reviews-search
```

-   Copy the elisp files and CSV to your Emacs config folder:

``` bash
mkdir -p ~/.emacs.d/lisp/mcn-bike-reviews-search/ && cp *.el *.csv ~/.emacs.d/elisp/mcn-bike-reviews-search/
```

-   Optionally, you can byte compile the elisp files for quicker loading:

``` bash
emacs --batch \
	-Q \
	-L ~/.emacs.d/elisp/mcn-bike-reviews-search \
	-f batch-byte-compile *.el
```

-   In your init.el, add the lisp directory to your Emacs path:

``` elisp
(add-to-list 'load-path (expand-file-name "lisp/mcn-bike-reviews-search" user-emacs-directory))
```

-   When you want to use this package, run `eshell` or `ielm` from Emacs and load it in:

``` elisp
(mcn/bike-search-initialise)
```

-   By default, it will load a local version of the bikes hashmap. If
    you want to try the download behaviour, set it with this variable:

``` elisp
(setq mcn/download-from-live-site t)
```
