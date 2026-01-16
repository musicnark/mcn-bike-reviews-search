#! /usr/bin/env sh
# User-space installation for MCN Bike Reviews Search Tool.

install_mcn_bike_reviews_search() {
	# check if we're in the right directory
	# extract files to/check that .emacs.d/elisp/ exists
	# provide instruction how to run it
	echo "Test~"
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
