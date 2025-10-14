# Max Heap

Implementation of a Max Heap Priority Queue for a cs2800 Homework Assignment.

## Extensions beyond required specs

* Profiling performance, heavy optimization, and comparison against Imperative Definitions.
* Stdlib-only, purely recursive (stateless/persistent) max-heap (expected solution was a list).
* Generic key type via functor; compiler-style bit-path navigation for complete-tree indexing.
* Packaged as a module; property tests run with QCheck under dune.
* Fast path tweaks with bit operations and shape-safe heapify.

---

## Benchmarks & Profiling

### Benchmark setup

* Workload: ~200,000 `push` followed by ~200,000 `pop` per trial (≈ 400k ops).
* Trials: 5 per run; report wall-time and OCaml GC stats.
* Location: `bench/bench_heap.ml` (build/run via `dune`).
* Build: native, with debug symbols for profiling.

  ```lisp
  ; bench/dune
  (executable
   (name bench_heap)
   (modules bench_heap)
   (libraries unix max_heap)
   (modes native)
   (ocamlopt_flags (:standard -g)))
  ```
* Run:

  ```bash
  dune exec --profile release bench/bench_heap.exe
  ```

### Property-Based Testing (PBT)

* Generator biased around power-of-two sizes to stress insert/delete path bits.
* Found and minimized counterexamples leading to shape violations (e.g., mismatched left/right pathing, inconsistent insert vs. delete traversal).
* Fixes:

  * Unified **depth-driven** path walk using 1-based index bits (0→left, 1→right) for both `get_last` and `pop_last`.
  * Made `downheap` follow the same convention for insertion.
  * Replaced brittle pattern-match heapify with a general **heapify-down** (compare children, swap with larger, recurse).

### Changes measured

1. **Baseline (pre-flatten, pre-depth-walk)**

   ```
   time ≈ 0.53 s
   minor_words ≈ 136,000,000
   promoted_words ≈ 26,800,000
   major_collections ≈ 20
   ```

   ~0.75M ops/sec.

2. **Flatten node payload**
   Changed `Node (lt, (k,v), rt)` → `Node (lt, k, v, rt)` (removes a tuple allocation per node).

   ```
   time ≈ 0.26 s
   minor_words ≈ 84,500,000
   promoted_words ≈ 13,900,000
   major_collections ≈ 19–26
   ```

   ~1.55M ops/sec.
   Allocation drops: minor −38%, promoted −48%.

3. **Depth-driven insert (`downheap`)**
   New `downheap` walks bits with a countdown `i = floor_log2 sz`; same 0→L/1→R convention as delete.

   ```
   time ≈ 0.24 s
   minor_words ≈ 84,7–85,0 M
   promoted_words ≈ 13,87–13,98 M
   major_collections ≈ 19–26
   ```

   ~1.65M ops/sec.
   (No GC tuning yet.)

4. **GC tuning (8 MB vs 16 MB vs 32 MB)**
   Larger minor heaps reduced promotions/majors and improved wall time.

   ```
   8 MB:
     time ≈ avg 0.210 s, med 0.210 s
     minor_words ≈ 83.89 M
     promoted_words ≈ 4.72 M
     major_collections ≈ 6.4

   16 MB:
     time ≈ avg 0.203 s, med 0.198 s
     minor_words ≈ ~84 M (one low 75.5 M, one high 92.3 M)
     promoted_words ≈ 3.34 M
     major_collections ≈ 4.8

   32 MB:
     time ≈ avg 0.188 s, med 0.172 s
     minor_words ≈ ~84 M
     promoted_words ≈ 1.97 M
     major_collections ≈ 3.8
   ```

   ~2.12M ops/sec (avg) / ~2.33M ops/sec (med) at 32 MB.
   (Vs 8 MB: +12–22% throughput; promotions ↓ ~58%, majors ↓ from ~6.4 → ~3.8.)

5. **Recursive–Imperative head-to-head (n = 200k)**
   Array (imperative) heap compared to persistent (tree) heap on the same workload.

   ```
   Persistent (tree), 5 trials:
     time ≈ avg 0.209 s, med 0.197 s
     minor_words ≈ 83.89 M
     promoted_words ≈ 2.52 M
     major_collections ≈ 3.0

   Array (imperative), 5 trials:
     time ≈ avg 0.121 s, med 0.115 s
     minor_words ≈ ~0 (one run ~16.8 M)
     promoted_words ≈ ~0
     major_collections ≈ 0–1
   ```

   ~1.91M ops/sec (avg) persistent vs ~3.31M ops/sec (avg) array → **~1.73× faster** for the array heap.

### Interpreting the GC numbers

* `minor_words`: total words allocated in the minor heap (lower is better here).
* `promoted_words`: survivors copied to major heap (ideally lower).
* `major_collections`: full collections; more can correlate with jitter/slower runs.

The big gain came from **allocation reduction** (flattened nodes). The depth-walk `downheap` also trimmed arithmetic/branching per level.

### Optional GC tuning (bench-only)

Add at the top of `bench/bench_heap.ml` to reduce major GCs on this alloc-heavy workload:

```ocaml
let () =
  let g = Gc.get () in
  Gc.set { g with
    minor_heap_size = 16 * 1024 * 1024;  (* try 8–32MB *)
    space_overhead  = 120;
  }
```

Expected effect: fewer `major_collections`, slight wall-time drop.

---

## How to run tests

* Property tests (QCheck):

  ```bash
  dune runtest
  ```
* Crash-finder test targets complete-tree shape and pop path; emits minimized repros on failure.

---

## Takeaways

* Functional (persistent) tree heaps can be competitive with the right structure:

  * Flattening node payload and unifying index-bit traversal roughly **2× throughput** vs. the initial version.
* For further speed: GC tuning in the bench or minor micro-opts in heapify is a good target.
