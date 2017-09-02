#!/bin/bash

case "$1" in
  ruby)
    shift
    bundle exec rspec "$@" || STATUS=$?
    ;;
  javascript)
    shift
    yarn test "$@" || STATUS=$?
    ;;
  c)
    shift

    if [ $# -eq 0 ]; then
      while read line; do
        ./spec/support/test-c.sh "$line" || STATUS=$?
      done < <(find spec -name '*_spec.c')
    else
      for line in "$@"; do
        ./spec/support/test-c.sh "$line" || STATUS=$?
      done
    fi
    ;;
  racket|scheme)
    shift
    if [ $# -eq 0 ]; then
      PLTCOLLECTS=":$(pwd)/lib" raco test spec/ || STATUS=$?
    else
      PLTCOLLECTS=":$(pwd)/lib" raco test "$@" || STATUS=$?
    fi
    ;;
  haskell)
    shift
    ./spec/support/haskell.rb "$@" || STATUS=$?
    ;;
  reset)
    echo "Deleting temporary files and directories"
    rm -r .haskell
    exit 1
    ;;
  '')
    bundle exec rspec || STATUS=$?

    yarn test || STATUS=$?

    while read line; do
      ./spec/support/test-c.sh "$line" || STATUS=$?
    done < <(find spec -name '*_spec.c')

    PLTCOLLECTS=":$(pwd)/lib" raco test spec/ || STATUS=$?

    ./spec/support/haskell.rb || STATUS=$?
    ;;
  *)
    echo "Unrecognized command: $1"
    exit 1
    ;;
esac

if [[ -z $STATUS ]]; then
  echo "Tests passed!"
else
  echo "Tests failed with status: $STATUS"
fi

exit $STATUS
