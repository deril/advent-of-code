#!/bin/sh

if [ "$1" = "-h" -o "$1" = "--help" ]; then
    cat <<EOF
This script creates the appropriate files for a new year of Advent of Code.

Just run it without any parameters.  It will use the year it thinks is next,
but it'll confirm that value before it does anything.
EOF
    exit 0
fi

cd $(dirname $(readlink -f "$0"))

if [ -n "$(git status --porcelain)" ]; then
    echo "Working directory is not clean.  Refusing to do anything!"
    exit 1
fi

# last_year=$(ls -d ???? | tail -1)
last_year=2022
new_year=$(echo $last_year + 1 | bc)

echo -n "New year is ${new_year}.  Is that correct? [Y/n] "
read confirmation
confirmation=$(echo $confirmation | tr a-z A-Z)

if [ ! \( -z "$confirmation" -o "$confirmation" = "Y" -o "$confirmation" = "YES" \) ]; then
    echo "Canceling!"
    exit 1
fi

mkdir $new_year
for day in $(seq -w 1 25); do
    cat >${new_year}/${day}.lisp <<EOF
(in-package :aoc-${new_year}-${day})

(aoc:define-day nil nil)
EOF
done

perl -pi -e "/DEFSUITE-MARKER/ and print \"(5am:def-suite :aoc-${new_year} :in :aoc-all)\n\"" advent-of-code.lisp

perl -pi -e "/YEAR-MARKER/ and s/${last_year}/${last_year} ${new_year}/" advent-of-code.asd packages.lisp

git add $new_year advent-of-code.lisp advent-of-code.asd packages.lisp
git ci -m "Add year ${new_year}"
