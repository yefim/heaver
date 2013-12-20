#!/bin/sh

echo ""

#TEST 1 
./heaver README.md
cleaver README.md

# use exit code of diff to check whether 2 files are identical
if diff --brief --ignore-blank-lines --ignore-all-space --ignore-space-change README.md.heaver README-cleaver.html ; then
  echo Test 1 Passed
else
  echo Test 1 Failed
fi

rm README.md.heaver README-cleaver.html

echo ""

#TEST 2
./heaver examples/test.md
cleaver examples/test.md

# use exit code of diff to check whether 2 files are identical
if diff --brief --ignore-blank-lines --ignore-all-space --ignore-space-change examples/test.md.heaver test-cleaver.html ; then
  echo Test 2 Passed
else
  echo Test 2 Failed
fi

rm examples/test.md.heaver test-cleaver.html

echo ""

#TEST 3
./heaver examples/basic.md
cleaver examples/basic.md

# use exit code of diff to check whether 2 files are identical
if diff --brief --ignore-blank-lines --ignore-all-space --ignore-space-change examples/basic.md.heaver basic-cleaver.html ; then
  echo Test 3 Passed
else
  echo Test 3 Failed
fi

rm examples/basic.md.heaver basic-cleaver.html

echo ""
