#!/bin/bash

bundle exec rspec || STATUS=$?
yarn test || STATUS=$?
PLTCOLLECTS=":$(pwd)/lib" raco test spec/ || STATUS=$?
while read line; do
  ./spec/support/test-c.sh "$line" || STATUS=$?
done < <(find spec -name '*_spec.c')

echo $STATUS
exit $STATUS
