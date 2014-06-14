Comparable Key
==============

This package allows one to define Eq, Ord and Hashable on
the key part of a data structure. While it is good
practice to define the key part of your data type
separately, it is often convenient to work with the
aggregate object. Defining Eq, Ord and Hashable directly
on the aggregate would be misguided as one would prefer to
reserve these "proper" notions of comparison for the
aggregate object. EqByKey, OrdByKey and HashableByKey offer
convenient substitutes that can be used in their stead.
This package does not currently interact with the keys
package, but support should be added in future revisions
(contributions welcome!).

