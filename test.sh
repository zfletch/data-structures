#!/bin/sh

bundle exec rspec || STATUS=$?
yarn test || STATUS=$?
PLTCOLLECTS=":$(pwd)/lib" raco test spec/ || STATUS=$?

echo $STATUS
exit $STATUS
