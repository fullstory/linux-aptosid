#!/bin/sh -e
# Takes a BK diff key in $1, for the kernel series in $2, creates a patch
# annotation for it, and saves it in the file pointed to by $3.

if [ $# -lt 3 ]; then
	echo "Passed $# arguments, need 3:"
	echo "$0 [KEY] [SERIES] [TARGET]"
	exit 1
fi

KEY=$1
MAJOR=$2
TARGET=$3
TEMP="$(tempfile)"
DATE="$(date +%Y-%m-%d)"

case "$MAJOR" in
	2.5 | 2.6 | 2.4) ;;
	*) echo "Invalid major $MAJOR. Valid entries are 2.4, 2.5, and 2.6."
	   echo "If I'm wildly out of date, update me."
	   exit 1
	;;
esac

if ! wget -q -O "$TEMP" http://linux.bkbits.net:8080/linux-$MAJOR/gnupatch@$KEY; then
	echo "Could not retrieve key $KEY." >&2
	rm -f $TEMP
	exit 1
fi

cat >$TARGET <<EOF
# origin: bk
# key: $KEY (linux-$MAJOR)
# description:
# inclusion:
# revision date: $DATE

EOF

cat >>$TARGET < $TEMP

rm -f $TEMP
