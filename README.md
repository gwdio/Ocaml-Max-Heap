# Max Heap
Implementation of a Max Heap Priority Queue for a cs2800 Homework Assignment.

## Extensions beyond required specs
- Code uses a stdlib-only purely recursive (and thus stateless) implementation of a Max-Heap. (Expected implementation was with a list)
- Heap has been optimized with some bitwise operations to be extra fast.
- Max Heap has been abstracted to be generically typed.
- Implementation has been converted into a module.
- Tests shifted to QCheck to run with dune.