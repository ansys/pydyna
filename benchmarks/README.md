# Performance Benchmarks

This directory contains performance benchmarks for PyDyna components.

## Plotting Benchmarks

### bench_plotting_bottlenecks.py
Micro-benchmarks for individual plotting functions.

Usage: `python benchmarks/bench_plotting_bottlenecks.py`

Tests: get_nid_to_index_mapping, map_facet_nid_to_index, extract_shell_facets, extract_solids

### bench_plotting_endtoend.py
End-to-end plotting benchmarks with real-world and synthetic models.

Usage: `python benchmarks/bench_plotting_endtoend.py`

Tests: Camry model (1M nodes), procedurally generated hex meshes (1K-64K elements)

## Deck Benchmarks

### bench_deck_expand.py
Benchmarks deck.expand() performance for include file processing.

Usage: `python benchmarks/bench_deck_expand.py`

Tests: Camry model expansion (6 → 363 keywords, 21 seconds)

This is currently the PRIMARY BOTTLENECK (96% of total time for plotting workflow).

## General Benchmarks

### bench_general.py
General performance tests (formerly test_perf.py).

Usage: `python benchmarks/bench_general.py <test_number> <profiler_mode>`

Tests: keyword writing, deck loading, import operations

## Optimization Status

Plotting performance (Camry: 1M nodes, 975K shells):
- Before: 2,395 ms
- After: 169 ms (14.2x faster)
- Time saved: 2.2 seconds

Key change: Eliminated separate_triangles_and_quads by having extract_shell_facets return tri/quad separately.

**Current bottleneck:** deck.expand() takes 21 seconds (96% of total time).
