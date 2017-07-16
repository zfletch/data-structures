#!/bin/sh

gcc -Wall \
  -I lib \
  -I spec \
  "$1" \
  $(echo "$1" | sed 's/spec/lib/' | sed 's/_spec//') \
  spec/support/utils.c \
  -o _test.out

./_test.out || STATUS=$?
rm ./_test.out

exit $STATUS
