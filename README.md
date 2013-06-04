### Fenwick trees over semigroups      [![Build Status](https://travis-ci.org/davnils/fenwick-semi.png)](https://travis-ci.org/davnils/fenwick-semi)

This is a generalized implementation of Fenwick trees working over semigroups.
Internally the tree is stored in a explicitly recursive data structure for simplicity.
Input elements are identified by indices, which form the internal ordering of the tree.

A Fenwick tree implements prefix *sums* and element updates and these operations are only allowed *log n* time.
The semigroup constraint (no inverse for all elements, e.g. subtraction) constrains how the tree can be manipulated.
All operations are done by first performing a top-down-traversal, which records a path down to the leaf of interest.
This is followed by either accumulating along the path or rebuilding a new tree with the updated values.
