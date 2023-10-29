# schrodinger

> Sometimes you just can't hold all of those cats in RAM, so you need to start guessing how many there are.

This project contains implementations of probabilistic data structures in scala, integrating with cats and checking their laws.

It also contains instances and tests for well known JVM probabilistic data structures, so that you can use them easily with cats, and confirm they meet the expected laws.

## Included Data Types (Simple Module)

### Minhash

A MinHash implementation is provided as `SimpleMinHash`.

Provides:
* Jaccard
* Union - Semilattice

### Theta Sketch

A Theta Sketch implementation is provided as `SimpleThetaSketch`

Provides:
* Cardinality
* Union - BoundedSemilattice
* (To be added, Intersection and Diff)

## hash4j integration

* `UltraLogLog` - `BoundedSemilattice`
* `MinHash` - `Semilattice`