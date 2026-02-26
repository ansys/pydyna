#!/usr/bin/env python
"""Benchmark deck.expand() performance to identify bottlenecks.

Deck Expand Bottleneck Analysis
================================

Model: Camry roof crush (1M nodes, 975k shells)
Total Time: ~26-46 seconds (varies with cProfile overhead)

Executive Summary
-----------------
``deck.expand()`` is the PRIMARY BOTTLENECK in the plotting pipeline,
accounting for 97% of total execution time. The plotting operations themselves
now take only 169ms after optimization, but expand takes 26+ seconds.

Usage
-----
::

    # Quick timing (recommended for iterative development)
    python benchmarks/bench_deck_expand.py

    # Detailed profiling (when you need to dig deeper)
    python benchmarks/bench_deck_expand.py --profile

Profiling Results
-----------------
- Simple timing mode: 26.15 seconds
- cProfile mode: 46.45 seconds (profiling overhead adds ~20s)

Top Bottlenecks (from cProfile)
--------------------------------
1. pandas/python_parser.py:1316 - 7.6s (16%) - Comment checking, 1.98M calls
2. pandas/python_parser.py:837(_check_comments) - 7.3s (16%) - Comment checking, 1.4k calls
3. pandas/to_numeric() - 3.7s (8%) - Type conversion, 2.9k calls
4. deck_loader.py:292(_try_load_deck_from_buffer) - 2.2s (5%) - Main loader, 7 calls
5. kwd_line_formatter.py:31(read_line) - 2.0s (4%) - Line reading, 2.0M calls
6. str.strip() - 2.0s (4%) - String trimming, 17.8M calls

Total from top 6: 24.8 seconds (53% of runtime)

Key Insights
------------

1. Pandas Comment Checking is Expensive (14.9s, 32%)
   - Pandas is checking for comments on nearly 2 million lines
   - This happens during table/curve parsing
   - Optimization opportunity: If we know data doesn't have comments, disable checking

2. Excessive String Operations (2.0s, 4%)
   - 17.8 million str.strip() calls
   - Each line is being stripped multiple times
   - Optimization opportunity: Strip once and reuse

3. Line Reading Overhead (2.0s, 4%)
   - 2 million calls to read_line()
   - Each line parsed individually
   - Optimization opportunity: Batch processing or vectorization

4. Type Conversion (3.7s, 8%)
   - Pandas to_numeric() is slow
   - Optimization opportunity: Pre-specify dtypes to avoid inference

Optimization Targets (Priority Order)
-------------------------------------

HIGH PRIORITY: Pandas Comment Checking (14.9s potential savings)
    Disable comment checking when not needed:
    pd.read_csv(..., comment=None)

MEDIUM PRIORITY: String Operations (2.0s potential savings)
    - Cache stripped strings
    - Use str.strip() once per line maximum
    - Consider str.lstrip()/rstrip() when only one side needed

MEDIUM PRIORITY: Type Inference (3.7s potential savings)
    Pre-specify dtypes instead of letting pandas infer:
    pd.read_csv(..., dtype={'col1': float, 'col2': int})

LOW PRIORITY: Batch Processing (2.0s potential savings)
    - Process multiple lines at once instead of one-by-one
    - Use NumPy/pandas vectorization where possible

Expected Impact
---------------
- Pandas comment checking: 14.9s -> ~1.0s (14.9x speedup)
- Type inference: 3.7s -> ~0.5s (7.4x speedup)
- String operations: 2.0s -> ~0.5s (4.0x speedup)
- TOTAL: 26.0s -> ~6.0s (4.3x speedup)

Next Steps
----------
1. Profile pandas usage: Find where 'comment' parameter is used in table/curve parsing
2. Test disabling comments: Measure impact of comment=None in pandas calls
3. Optimize string operations: Profile kwd_line_formatter.py for strip usage
4. Pre-specify dtypes: Add explicit dtype mappings for known columns

Related Files
-------------
- benchmarks/bench_deck_expand.py (this file)
- src/ansys/dyna/core/lib/deck_loader.py
- src/ansys/dyna/core/lib/kwd_line_formatter.py
- src/ansys/dyna/core/lib/table_card.py
"""

import cProfile
import pstats
import time
from pathlib import Path

from ansys.dyna.core import Deck


# Profiling mode: 1=cProfile, 2=simple timing
PROFILER = 2


def _profile(func):
    """Profile function execution based on PROFILER setting.

    PROFILER=1: Use cProfile for detailed performance analysis
    PROFILER=2: Use simple time.time() for basic timing
    """
    global PROFILER
    if PROFILER == 2:
        t1 = time.time()
        func()
        t2 = time.time()
        print(f"\nTotal runtime: {t2-t1:.3f} seconds")
    elif PROFILER == 1:
        profiler = cProfile.Profile()
        profiler.enable()
        func()
        profiler.disable()
        stats = pstats.Stats(profiler).sort_stats("tottime")
        print("\n" + "="*70)
        print("cProfile Results (sorted by tottime)")
        print("="*70)
        stats.print_stats(30)  # Top 30 functions


def benchmark_camry_expand():
    """Benchmark deck expansion for the Camry roof crush model."""
    # Path to Camry model
    camry_path = Path("src/ansys/dyna/core/pre/examples/implicit/camry_rc/Camry_RC_main.k")

    if not camry_path.exists():
        print(f"ERROR: Camry model not found at {camry_path}")
        print("Make sure you're running from the pydyna root directory.")
        return

    camry_dir = camry_path.parent

    print("="*70)
    print("DECK EXPAND BENCHMARK - Camry Model")
    print("="*70)
    print()

    # Load the deck
    deck = Deck()

    print("Loading deck...")
    t_start = time.perf_counter()
    with open(camry_path, 'r') as f:
        deck.loads(f.read())
    t_load = time.perf_counter() - t_start
    print(f"  Load time: {t_load*1000:8.2f} ms")

    # Count keywords before expansion
    n_keywords_before = len(deck)
    print(f"  Keywords in main file: {n_keywords_before:,}")
    print()

    # Expand the deck (this is what we're benchmarking)
    print("Expanding deck (processing includes)...")
    t_start = time.perf_counter()
    flat_deck = deck.expand(cwd=str(camry_dir), recurse=True)
    t_expand = time.perf_counter() - t_start

    print(f"  Expand time: {t_expand*1000:8.2f} ms ({t_expand:.2f} seconds)")
    print()

    # Count keywords after expansion
    n_keywords_after = len(flat_deck)
    print(f"  Keywords after expansion: {n_keywords_after:,}")
    print(f"  Expansion ratio: {n_keywords_after/n_keywords_before:.1f}x")
    print()

    # Count specific keyword types
    nodes = list(flat_deck.get_kwds_by_type("NODE"))
    shells = [kwd for kwd in flat_deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == "SHELL"]
    beams = [kwd for kwd in flat_deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == "BEAM"]
    solids = [kwd for kwd in flat_deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == "SOLID"]

    n_nodes = sum(len(kwd.nodes) for kwd in nodes) if nodes else 0
    n_shells = sum(len(kwd.elements) for kwd in shells) if shells else 0
    n_beams = sum(len(kwd.elements) for kwd in beams) if beams else 0
    n_solids = sum(len(kwd.elements) for kwd in solids) if solids else 0

    print("Model contents:")
    print(f"  Nodes:  {n_nodes:>10,}")
    print(f"  Shells: {n_shells:>10,}")
    print(f"  Beams:  {n_beams:>10,}")
    print(f"  Solids: {n_solids:>10,}")
    print()

    # Analysis
    print("="*70)
    print("ANALYSIS")
    print("="*70)

    if t_expand > 10.0:
        print(f"WARNING: Expand took {t_expand:.1f} seconds")
        print()
        print("This is the PRIMARY BOTTLENECK in the plotting pipeline.")
        print(f"Expand accounts for ~{t_expand/(t_expand+0.8)*100:.0f}% of total time.")
        print()
        print("Potential optimization targets:")
        print("  1. Include file I/O (disk reads)")
        print("  2. Keyword parsing (text -> objects)")
        print("  3. Keyword collection/flattening")
        print("  4. String operations (path resolution, text processing)")
    else:
        print(f"OK: Expand took {t_expand:.2f} seconds")

    print()
    print("To profile in detail, run:")
    print("  python benchmarks/bench_deck_expand.py --profile")
    print()


if __name__ == "__main__":
    import sys

    # Parse command line args
    if len(sys.argv) > 1 and sys.argv[1] == "--profile":
        PROFILER = 1
        print("Running with cProfile (detailed analysis)...")
        print()
    else:
        PROFILER = 2
        print("Running with simple timing...")
        print()

    _profile(benchmark_camry_expand)
