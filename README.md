Arborism
========

[![Build status][1]][2]

Arborism is an implementation of tree different algorithms based on
the framework of decomposition strategies described in:

> Dulucq and Touzet. 2003. Analysis of tree edit distance
> algorithms. In *Proceedings of the 14th Annual Symposium on
> Combinatorial Pattern Matching*. 83–95.

> Demaine, Mozes, Rossman, and Weimann. 2009. An Optimal Decomposition
> Algorithm for Tree Edit Distance. *ACM Transactions on Algorithms*,
> Vol. 6, No. 1.

The decomposition strategy used in a dynamic programming algorithm
determines how a particular problem instance will be broken up into
sub-problems. Some dynamic programming problems only admit a single
(up to symmetry) decomposition strategy (string edit distance,
matrix-chain multiplication, etc.) but others may have more
interesting structure. In the tree edit difference problem different
decomposition strategies can result in quite different tableaux
structures and, hence, asymptotic behaviour.

[1]: https://travis-ci.org/thsutton/arborism.svg?branch=master
[2]: https://travis-ci.org/thsutton/arborism
