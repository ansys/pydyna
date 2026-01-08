#!/usr/bin/env python
"""Performance profiling for deck_plotter.py functions.

Each benchmark profiles a single function in production code to measure performance
and identify remaining bottlenecks for further optimization.

Run with: python benchmarks/bench_bottlenecks.py
"""

import time
import numpy as np
import pandas as pd

# Import production implementations to profile
from ansys.dyna.core.lib.deck_plotter import (
    get_nid_to_index_mapping,
    map_facet_nid_to_index,
    extract_shell_facets,
    extract_solids,
    separate_triangles_and_quads,
)


# =============================================================================
# Generate synthetic test data
# =============================================================================

def generate_nodes_df(n_nodes: int, sparse_factor: float = 1.0) -> pd.DataFrame:
    """Generate a nodes DataFrame for benchmarking.

    Parameters
    ----------
    n_nodes : int
        Number of nodes to generate
    sparse_factor : float
        Factor to increase node ID range (1.0 = dense, 10.0 = sparse IDs)
    """
    max_nid = int(n_nodes * sparse_factor)
    nids = np.sort(np.random.choice(np.arange(1, max_nid + 1), size=n_nodes, replace=False))
    x = np.random.rand(n_nodes) * 100
    y = np.random.rand(n_nodes) * 100
    z = np.random.rand(n_nodes) * 100

    return pd.DataFrame({
        "nid": nids,
        "x": x,
        "y": y,
        "z": z,
        "tc": np.zeros(n_nodes, dtype=int),
        "rc": np.zeros(n_nodes, dtype=int),
    })


def generate_shells_df(n_shells: int, max_nid: int, quad_ratio: float = 0.7) -> pd.DataFrame:
    """Generate a shells DataFrame for benchmarking.

    Parameters
    ----------
    n_shells : int
        Number of shell elements
    max_nid : int
        Maximum node ID (for generating random node references)
    quad_ratio : float
        Ratio of quad elements (rest are triangles)
    """
    n_quads = int(n_shells * quad_ratio)
    n_tris = n_shells - n_quads

    eids = np.arange(1, n_shells + 1)
    pids = np.ones(n_shells, dtype=int)

    # Generate random node IDs for each element
    n1 = np.random.randint(1, max_nid + 1, size=n_shells)
    n2 = np.random.randint(1, max_nid + 1, size=n_shells)
    n3 = np.random.randint(1, max_nid + 1, size=n_shells)
    n4 = np.zeros(n_shells, dtype=int)
    n4[:n_quads] = np.random.randint(1, max_nid + 1, size=n_quads)

    # Remaining columns for 8-node shells (zeros for our test)
    n5 = np.zeros(n_shells, dtype=int)
    n6 = np.zeros(n_shells, dtype=int)
    n7 = np.zeros(n_shells, dtype=int)
    n8 = np.zeros(n_shells, dtype=int)

    return pd.DataFrame({
        "eid": eids, "pid": pids,
        "n1": n1, "n2": n2, "n3": n3, "n4": n4,
        "n5": n5, "n6": n6, "n7": n7, "n8": n8,
    })


def generate_solids_df(n_solids: int, max_nid: int) -> pd.DataFrame:
    """Generate a solids DataFrame for benchmarking (all hex elements)."""
    eids = np.arange(1, n_solids + 1)
    pids = np.ones(n_solids, dtype=int)

    # Generate 8 node IDs per element (faster: just use random ints, not unique per element)
    # For benchmarking purposes, we don't need truly unique nodes per element
    nodes = np.random.randint(1, max_nid + 1, size=(n_solids, 8), dtype=int)

    return pd.DataFrame({
        "eid": eids, "pid": pids,
        "n1": nodes[:, 0], "n2": nodes[:, 1], "n3": nodes[:, 2], "n4": nodes[:, 3],
        "n5": nodes[:, 4], "n6": nodes[:, 5], "n7": nodes[:, 6], "n8": nodes[:, 7],
        "n9": np.zeros(n_solids, dtype=int), "n10": np.zeros(n_solids, dtype=int),
    })


def generate_flat_facets(n_elements: int, nodes_per_element: int = 4) -> np.ndarray:
    """Generate a flat facets array for benchmarking map_facet_nid_to_index."""
    # Format: [count, n1, n2, n3, (n4), count, n1, n2, n3, (n4), ...]
    stride = nodes_per_element + 1
    total_size = n_elements * stride
    facets = np.zeros(total_size, dtype=np.int32)

    for i in range(n_elements):
        base = i * stride
        facets[base] = nodes_per_element
        facets[base + 1:base + stride] = np.random.randint(1, 100000, size=nodes_per_element)

    return facets


# =============================================================================
# Benchmark harness
# =============================================================================

def time_function(func, *args, iterations: int = 3, warmup: int = 1, **kwargs):
    """Time a function with warmup iterations."""
    # Warmup
    for _ in range(warmup):
        result = func(*args, **kwargs)

    # Timed runs
    times = []
    for _ in range(iterations):
        start = time.perf_counter()
        result = func(*args, **kwargs)
        end = time.perf_counter()
        times.append(end - start)

    return min(times), result


# =============================================================================
# Benchmark functions
# =============================================================================

def print_benchmark_header(name: str, data_size: str):
    """Print formatted benchmark header."""
    print(f"\n{'='*70}")
    print(f"BENCHMARK: {name}")
    print(f"Data size: {data_size}")
    print(f"{'='*70}")


def run_benchmark_1_nid_mapping():
    """Benchmark 1: get_nid_to_index_mapping"""
    print_benchmark_header("get_nid_to_index_mapping", "50K, 200K, 500K nodes")

    for n_nodes in [50_000, 200_000, 500_000]:
        nodes_df = generate_nodes_df(n_nodes)
        time_prod, mapping_prod = time_function(get_nid_to_index_mapping, nodes_df)

        print(f"\n  {n_nodes:,} nodes:")
        print(f"    Time: {time_prod*1000:8.2f} ms")
        print(f"    Bottleneck: {'NONE' if time_prod < 0.01 else 'MINOR' if time_prod < 0.1 else 'SIGNIFICANT'}")


def run_benchmark_2_map_facet():
    """Benchmark 2: map_facet_nid_to_index"""
    print_benchmark_header("map_facet_nid_to_index", "50K, 200K, 500K elements")

    for n_elements in [50_000, 200_000, 500_000]:
        max_nid = 500_000
        mapping_array = np.arange(max_nid + 1, dtype=np.int32) - 1
        mapping_array[0] = -1

        facets = generate_flat_facets(n_elements, nodes_per_element=4)
        time_prod, result_prod = time_function(map_facet_nid_to_index, facets, mapping_array)

        print(f"\n  {n_elements:,} quad elements ({len(facets):,} array entries):")
        print(f"    Time: {time_prod*1000:8.2f} ms")
        print(f"    Bottleneck: {'NONE' if time_prod < 0.01 else 'MINOR' if time_prod < 0.1 else 'SIGNIFICANT'}")


def run_benchmark_3_shell_facets():
    """Benchmark 3: extract_shell_facets"""
    print_benchmark_header("extract_shell_facets", "50K, 200K, 500K shells")

    for n_shells in [50_000, 200_000, 500_000]:
        max_nid = 500_000
        shells_df = generate_shells_df(n_shells, max_nid)
        mapping_array = np.arange(max_nid + 1, dtype=np.int32) - 1
        mapping_array[0] = -1

        time_prod, result_prod = time_function(extract_shell_facets, shells_df, mapping_array)
        facets_prod, eids_prod, pids_prod = result_prod

        print(f"\n  {n_shells:,} shells:")
        print(f"    Time: {time_prod*1000:8.2f} ms")
        print(f"    Bottleneck: {'NONE' if time_prod < 0.05 else 'MINOR' if time_prod < 0.5 else 'SIGNIFICANT'}")


def run_benchmark_4_solids():
    """Benchmark 4: extract_solids"""
    print_benchmark_header("extract_solids", "10K, 50K, 100K hex elements")

    for n_solids in [10_000, 50_000, 100_000]:
        max_nid = 500_000
        solids_df = generate_solids_df(n_solids, max_nid)
        mapping_array = np.arange(max_nid + 1, dtype=np.int32) - 1
        mapping_array[0] = -1

        time_prod, result_prod = time_function(extract_solids, solids_df, mapping_array)
        hex_count_prod = len(result_prod[8][1]) if result_prod.get(8) and len(result_prod[8][1]) > 0 else 0

        print(f"\n  {n_solids:,} solids:")
        print(f"    Time: {time_prod*1000:8.2f} ms")
        print(f"    Bottleneck: {'NONE' if time_prod < 0.01 else 'MINOR' if time_prod < 0.1 else 'SIGNIFICANT'}")


def run_benchmark_5_separate_tris_quads():
    """Benchmark 5: separate_triangles_and_quads"""
    print_benchmark_header("separate_triangles_and_quads", "50K, 200K, 500K elements")

    for n_elements in [50_000, 200_000, 500_000]:
        # Generate mixed facets (70% quads, 30% tris)
        n_tris = int(n_elements * 0.3)
        n_quads = n_elements - n_tris

        # Build facet array
        tri_size = n_tris * 4  # [3, n1, n2, n3]
        quad_size = n_quads * 5  # [4, n1, n2, n3, n4]
        total_size = tri_size + quad_size

        facets = np.zeros(total_size, dtype=np.int32)
        eids = np.arange(1, n_elements + 1, dtype=np.int32)
        pids = np.ones(n_elements, dtype=np.int32)

        # Fill triangles first, then quads
        pos = 0
        for i in range(n_tris):
            facets[pos] = 3
            facets[pos+1:pos+4] = np.random.randint(0, 100000, size=3)
            pos += 4
        for i in range(n_quads):
            facets[pos] = 4
            facets[pos+1:pos+5] = np.random.randint(0, 100000, size=4)
            pos += 5

        time_prod, result_prod = time_function(separate_triangles_and_quads, facets, eids, pids)

        print(f"\n  {n_elements:,} elements ({n_tris:,} tris, {n_quads:,} quads):")
        print(f"    Time: {time_prod*1000:8.2f} ms")
        print(f"    Bottleneck: {'NONE' if time_prod < 0.05 else 'MINOR' if time_prod < 0.5 else 'SIGNIFICANT'}")
def main():
    """Run all performance profiling benchmarks."""
    print("\n" + "="*70)
    print("DECK PLOTTER PERFORMANCE PROFILING")
    print("="*70)
    print("\nProfiling production code to identify remaining bottlenecks.")
    print("Target: <10ms for small, <100ms for medium, <1s for large models.\n")

    run_benchmark_1_nid_mapping()
    run_benchmark_2_map_facet()
    run_benchmark_3_shell_facets()
    run_benchmark_4_solids()
    run_benchmark_5_separate_tris_quads()

    print("\n" + "="*70)
    print("PROFILING COMPLETE")
    print("="*70)
    print("\nNext steps:")
    print("  - Review 'Bottleneck' indicators above")
    print("  - SIGNIFICANT bottlenecks need optimization")
    print("  - When optimizing, add comparison benchmarks to measure impact")
    print("="*70 + "\n")


if __name__ == "__main__":
    main()
