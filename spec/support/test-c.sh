#!/bin/bash

gcc \
  -Wall \
  -Wno-parentheses \
  -g \
  -I lib \
  -I spec \
  "$1" \
  $(echo "$1" | sed 's/spec/lib/' | sed 's/_spec//') \
  spec/support/utils.c \
  -o _test.out

echo "$1"

if [[ $OSTYPE == darwin16 ]]; then
  valgrind \
    --quiet \
    --suppressions=spec/support/valgrind/osx-10.12.5.supp \
    --error-exitcode=2 \
    --leak-check=yes \
    ./_test.out || STATUS=$?
else
  valgrind \
    --quiet \
    --error-exitcode=2 \
    --leak-check=yes \
    ./_test.out || STATUS=$?
fi

rm ./_test.out

exit $STATUS
