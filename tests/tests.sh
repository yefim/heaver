#!/bin/sh

echo ""

#TEST 1 
./heaver README.md

# use exit code of diff to check whether 2 files are identical
if diff --brief readme.html tests/readme.html ; then
  echo Test 1 Passed
else
  echo Test 1 Failed
fi

rm readme.html

echo ""

#TEST 2

./heaver examples/test.md

if diff --brief out.html tests/out.html ; then
  echo Test 2 Passed
else
  echo Test 2 Failed
fi

rm out.html

echo ""

./heaver examples/basic.md

if diff --brief basic.html tests/basic.html ; then
  echo Test 3 Passed
else
  echo Test 3 Failed
fi

rm basic.html

echo ""
