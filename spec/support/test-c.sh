#!/bin/sh

gcc -I lib \
  "$1" \
  $(echo "$1" | sed 's/spec/lib/' | sed 's/_spec//') \
  -o _test.out

./_test.out || STATUS=$?
rm ./_test.out

exit $STATUS
