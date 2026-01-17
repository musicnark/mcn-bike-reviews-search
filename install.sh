#! /usr/bin/env sh
# User-space installation for MCN Bike Reviews Search Tool.

install_mcn_bike_reviews_search() {
	# check if running in the project directory
	pwd | grep "mcn-bike-reviews-search" || {
		echo "Please run this script from the project root, run:
cd mcn-bike-reviews-search" &&
			return 1
		}

	# extract program files to .emacs.d/lisp/
	mkdir -p ~/.emacs.d/lisp/mcn-bike-reviews-search/ &&
		cp *.el *.csv ~/.emacs.d/lisp/mcn-bike-reviews-search/ &&
		cd ~/.emacs.d/lisp/mcn-bike-reviews-search/

	# (optional) byte compile program files for faster loading time
	emacs --batch \
		  -Q \
		  -L ~/.emacs.d/elisp/mcn-bike-reviews-search \
		  -f batch-byte-compile *.el >/dev/null 2>&1 ||
		echo "Warning: byte-compilation failed, continuing."
	echo ";; Automatic configuration from mcn-bike-reviews-search install
(add-to-list 'load-path (expand-file-name \"lisp/mcn-bike-reviews-search\" user-emacs-directory))
(require 'bike-reviews)" >> ~/.emacs.d/init.el
	# provide instruction how to run it
	echo -e "\n
Install successful.
You can now launch emacs, and load this tool via:

M-x bike-search-initialise

See the README for usage.

(HINT: M-x means 'Alt + x', or 'Cmd + x'. If you're new to Emacs, you can click the tutorial on the home page to learn the basics.)
"
}

install_emacs_macos() {
	echo "Installing Emacs for MacOS.."
	/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)" &&
	mkdir -p ~/.homebrew_apps && brew install emacs --cask --Appdir ~/.homebrew_apps/ && echo "Emacs install finished successfully"
}

install_emacs_debian_ubuntu() {
	echo "Installing Emacs for Ubuntu/Debian..."
	sudo apt install emacs -y && echo "Emacs install finished successfully"
}

main() {
echo -e "Setting up MCN Bike Reviews Search Tool...\n"

which emacs >/dev/null 2>&1 ||
if [ $(uname -s) = "Darwin" ]; then
	# install for macos
	install_emacs_macos

elif grep -qiE '^ID=(ubuntu|debian)' /etc/os-release; then
	# install for Debian/Ubuntu
	install_emacs_debian_ubuntu
else
	echo "Unsupported host - cannot install Emacs automatically. Please install emacs manually, add it to your path, and run this script again."
	return 1
fi

install_mcn_bike_reviews_search
}
main
