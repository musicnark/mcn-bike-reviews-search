# Recommended Install
To follow the recommended install method, run the supplied [install script](../../install.sh), which automates install for this tool and Emacs on MacOS, Debian, and Ubuntu. Copy and paste the following into a terminal and hit enter:

``` bash
git clone https://github.com/musicnark/mcn-bike-reviews-search &&
cd mcn-bike-reviews-search &&
chmod +x install.sh &&
./install.sh
```
Once installed, the script will tell you where to find the Emacs app. Navigate to it and launch Emacs, then and run `M-x mcn/bike-search-initialise` to load the bike reviews data.

Then, you can get started with some [basic usage](./basic-usage.md).

On MacOS, `M-x` means pressing `Option + x`. On Windows/Linux, it means `Alt + x`. After that, you type the name of the function to run (in this case `mcn/bike-search-initialise`) and hit enter to run it.

If unfamiliar with Emacs, now's a good time to [familiarise yourself with the basics](https://www.gnu.org/software/emacs/tour/) before continuing.

---

# Manual Install
If installing manually, open a terminal and follow the below instructions:

- Install Emacs and add it to your PATH (check online for specific instructions on your operating system)

- Clone this repository:

```bash
git clone https://github.com/musicnark/mcn-bike-reviews-search
```

- Copy the elisp files and CSV to your Emacs config folder:

```bash
mkdir -p ~/.emacs.d/lisp/mcn-bike-reviews-search/ && cp *.el *.csv ~/.emacs.d/elisp/mcn-bike-reviews-search/
```

- Optionally, you can byte compile the elisp files for quicker loading:

```bash
emacs --batch \
	-Q \
	-L ~/.emacs.d/elisp/mcn-bike-reviews-search \
	-f batch-byte-compile *.el
```

- In your init.el, add the lisp directory to your Emacs path:

```elisp
(add-to-list 'load-path (expand-file-name "lisp/mcn-bike-reviews-search" user-emacs-directory))
```

- When you want to use this package, load it in:

```elisp
M-x mcn/bike-search-initialise
```

- By default, it will load a local version of the bikes hash map. If you want to try the download behaviour, set it with this variable:

```elisp
(setq mcn/download-from-live-site t)
```
