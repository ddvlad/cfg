#!/bin/bash

# Symlink config files to specified directory.

function usage()
{
	echo -e "Usage: $0 [-d destdir] [-s]"
	echo -e "Run this script in the repo directory"
	echo -e "\tdestdir is where to put the symlinks, default is \$HOME"
	echo -e "\t-s skips existing files (default is to back them up)"
	echo -e "\t-h prints this message"
	exit 1
}

# Install specified file to the destination directory
function install_one()
{
	destfile="$1"
	target="$1"
	# All files that start with an underscore actiually start with a dot
	# in the destination directory.
	if [[ "$destfile" =~ ^_.* ]]; then
		destfile="$(echo $destfile | sed -e s/^_/\./)"
	fi

	if [[ -e "$destdir/$destfile" ]]; then
		if [[ $skip == 1 ]]; then
			echo "Skipping existing file $destdir/$destfile"
			return
		else
			mv "$destdir/$destfile" "$backupdir/"
		fi
	fi

	ln -s "$PWD/$target" "$destdir/$destfile"
}

destdir="$HOME"
skip=0

# Parse command line args
while [[ $# -gt 0 ]]; do
	case $1 in
		"-h")
			usage $@
			;;
		"-d")
			destdir="$2";
			shift 2
			;;
		"-s")
			skip=1;
			shift
			;;
		*)
			usage $@
		;;
	esac
done

# Create backup directory
if [[ $skip == 0 ]]; then
	backupdir=$(mktemp -d --tmpdir=. backupXXXXXX)
	echo "Backing up existing files to $backupdir"
fi

# Only handle files beginning with an underscore. This is a retarded
# convention and will probably change sometime.
for f in _*; do
	install_one "$f"
done

exit 0
