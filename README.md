Arborism
========

[![Build status][1]][2]

Arborism is an implementation of tree different algorithms described
in the following papers.

> Zhang and Shasha. 1989. Simple fast algorithms for the editing
> distance between trees and related problems. *SIAM Journal on
> Computing*, Vol. 18, No. 6. https://doi.org/10.1137/0218082

> Dulucq and Touzet. 2003. Analysis of tree edit distance
> algorithms. In: Baeza-Yates R., Chávez E., Crochemore M. (eds)
> *Combinatorial Pattern Matching*. Lecture Notes in Computer Science,
> vol 2676. https://doi.org/10.1007/3-540-44888-8_7

> Demaine, Mozes, Rossman, and Weimann. 2009. An Optimal Decomposition
> Algorithm for Tree Edit Distance. *ACM Transactions on Algorithms*,
> Vol. 6, No. 1. https://doi.org/10.1145/1644015.1644017

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
