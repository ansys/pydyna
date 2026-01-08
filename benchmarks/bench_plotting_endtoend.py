"""
Performance benchmarks for deck plotting.

Tests plotting performance with:
1. Real-world model (Camry)
2. Procedurally generated solid meshes of varying sizes
"""
import time
import numpy as np
import pandas as pd
from pathlib import Path

from ansys.dyna.core import Deck
from ansys.dyna.core.lib.deck_plotter import get_polydata


def benchmark_function(func, name, *args, **kwargs):
    """Benchmark a function and print timing results."""
    print(f"\n{'='*60}")
    print(f"Benchmarking: {name}")
    print(f"{'='*60}")
    
    start = time.perf_counter()
    result = func(*args, **kwargs)
    elapsed = time.perf_counter() - start
    
    print(f"Time: {elapsed:.3f}s")
    
    if hasattr(result, 'n_cells'):
        print(f"Cells: {result.n_cells:,}")
    if hasattr(result, 'n_points'):
        print(f"Points: {result.n_points:,}")
    if hasattr(result, 'GetActualMemorySize'):
        mem_kb = result.GetActualMemorySize()
        print(f"Memory: {mem_kb/1024:.2f} MB")
    
    return result, elapsed


def create_hex_mesh(nx, ny, nz):
    """
    Create a structured hex mesh for benchmarking.
    
    Parameters
    ----------
    nx, ny, nz : int
        Number of elements in each direction
        
    Returns
    -------
    deck : Deck
        Deck with nodes and solid elements
    """
    deck = Deck()
    
    # Build keyword string
    kwd_str = "*KEYWORD\n*NODE\n"
    
    # Create nodes
    node_id = 1
    for k in range(nz + 1):
        for j in range(ny + 1):
            for i in range(nx + 1):
                x, y, z = float(i), float(j), float(k)
                kwd_str += f"{node_id:8d}{x:16.8f}{y:16.8f}{z:16.8f}       0       0\n"
                node_id += 1
    
    # Create solid elements
    kwd_str += "*ELEMENT_SOLID\n"
    elem_id = 1
    
    def get_node_id(i, j, k):
        """Get node ID from grid coordinates."""
        return 1 + i + j * (nx + 1) + k * (nx + 1) * (ny + 1)
    
    for k in range(nz):
        for j in range(ny):
            for i in range(nx):
                # Hex element with 8 nodes
                n1 = get_node_id(i, j, k)
                n2 = get_node_id(i+1, j, k)
                n3 = get_node_id(i+1, j+1, k)
                n4 = get_node_id(i, j+1, k)
                n5 = get_node_id(i, j, k+1)
                n6 = get_node_id(i+1, j, k+1)
                n7 = get_node_id(i+1, j+1, k+1)
                n8 = get_node_id(i, j+1, k+1)
                
                kwd_str += f"{elem_id:8d}       1{n1:8d}{n2:8d}{n3:8d}{n4:8d}{n5:8d}{n6:8d}{n7:8d}{n8:8d}\n"
                elem_id += 1
    
    kwd_str += "*END\n"
    
    # Load into deck
    deck.loads(kwd_str)
    
    return deck


def benchmark_camry():
    """Benchmark with the Camry model."""
    camry_path = Path(__file__).parent / "src/ansys/dyna/core/pre/examples/implicit/camry_rc/Camry_RC_main.k"
    
    if not camry_path.exists():
        print(f"Camry model not found at {camry_path}")
        return None, 0
    
    print(f"Loading Camry model from: {camry_path}")
    deck = Deck()
    
    start_load = time.perf_counter()
    with open(camry_path, 'r') as f:
        deck.loads(f.read())
    load_time = time.perf_counter() - start_load
    print(f"Load time: {load_time:.3f}s")
    
    # Expand to get all includes
    start_expand = time.perf_counter()
    try:
        flat_deck = deck.expand(cwd=camry_path.parent, recurse=True)
        expand_time = time.perf_counter() - start_expand
        print(f"Expand time: {expand_time:.3f}s")
    except Exception as e:
        print(f"Warning: Could not expand deck: {e}")
        flat_deck = deck
        expand_time = 0
    
    # Count elements
    nodes = [kwd for kwd in flat_deck.get_kwds_by_type("NODE")]
    shells = [kwd for kwd in flat_deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == "SHELL"]
    solids = [kwd for kwd in flat_deck.get_kwds_by_type("ELEMENT") if kwd.subkeyword == "SOLID"]
    
    n_nodes = sum(len(kwd.nodes) for kwd in nodes)
    n_shells = sum(len(kwd.elements) for kwd in shells)
    n_solids = sum(len(kwd.elements) for kwd in solids)
    
    print(f"Nodes: {n_nodes:,}, Shells: {n_shells:,}, Solids: {n_solids:,}")
    
    # Benchmark plotting
    result, elapsed = benchmark_function(get_polydata, "Camry Model", flat_deck)
    
    return result, elapsed


def benchmark_procedural_meshes():
    """Benchmark with procedurally generated meshes of various sizes."""
    results = []
    
    # Test different mesh sizes
    sizes = [
        (10, 10, 10, "Small (1K elements)"),
        (20, 20, 20, "Medium (8K elements)"),
        (30, 30, 30, "Large (27K elements)"),
        (40, 40, 40, "X-Large (64K elements)"),
    ]
    
    for nx, ny, nz, label in sizes:
        n_elems = nx * ny * nz
        n_nodes = (nx + 1) * (ny + 1) * (nz + 1)
        
        print(f"\n{'='*60}")
        print(f"Creating {label}: {n_elems:,} hex elements, {n_nodes:,} nodes")
        print(f"{'='*60}")
        
        start = time.perf_counter()
        deck = create_hex_mesh(nx, ny, nz)
        create_time = time.perf_counter() - start
        print(f"Mesh creation time: {create_time:.3f}s")
        
        # Benchmark plotting WITHOUT extract_surface
        result_full, elapsed_full = benchmark_function(
            get_polydata, f"Plot {label} (full)", deck, extract_surface=False
        )
        
        # Benchmark plotting WITH extract_surface
        result_surf, elapsed_surf = benchmark_function(
            get_polydata, f"Plot {label} (surface)", deck, extract_surface=True
        )
        
        speedup = elapsed_full / elapsed_surf if elapsed_surf > 0 else 0
        reduction = (1 - result_surf.n_cells / result_full.n_cells) * 100 if result_full.n_cells > 0 else 0
        
        print(f"\nSpeedup: {speedup:.2f}x faster with extract_surface()")
        print(f"Cell reduction: {reduction:.1f}% ({result_full.n_cells:,} -> {result_surf.n_cells:,})")
        
        results.append({
            'label': label,
            'n_elements': n_elems,
            'n_nodes': n_nodes,
            'create_time': create_time,
            'plot_time_full': elapsed_full,
            'plot_time_surf': elapsed_surf,
            'speedup': speedup,
            'cells_full': result_full.n_cells,
            'cells_surf': result_surf.n_cells,
            'reduction_pct': reduction,
            'points': result_full.n_points
        })
    
    return results


def print_summary(results):
    """Print summary table of results."""
    if not results:
        return
    
    print(f"\n{'='*100}")
    print("PERFORMANCE SUMMARY")
    print(f"{'='*100}")
    print(f"{'Mesh Size':<20} {'Elements':>12} {'Full(s)':>10} {'Surface(s)':>12} {'Speedup':>10} {'Cells':>15} {'Reduction':>12}")
    print(f"{'-'*100}")
    
    for r in results:
        speedup_str = f"{r['speedup']:.2f}x"
        cells_str = f"{r['cells_full']:,} -> {r['cells_surf']:,}"
        reduction_str = f"{r['reduction_pct']:.1f}%"
        print(f"{r['label']:<20} {r['n_elements']:>12,} {r['plot_time_full']:>10.3f} "
              f"{r['plot_time_surf']:>12.3f} {speedup_str:>10} {cells_str:>15} {reduction_str:>12}")
    
    print(f"{'='*100}\n")
    
    # Print interpretation
    avg_speedup = np.mean([r['speedup'] for r in results])
    avg_reduction = np.mean([r['reduction_pct'] for r in results])
    
    print(f"Average speedup: {avg_speedup:.2f}x")
    print(f"Average cell reduction: {avg_reduction:.1f}%")
    print(f"\nInterpretation: extract_surface() removes interior cells while preserving visual")
    print(f"appearance, resulting in significant performance gains for solid meshes.")


if __name__ == "__main__":
    print("PyDyna Plotting Performance Benchmark")
    print("=" * 80)
    
    # Test with Camry model
    print("\n1. REAL-WORLD MODEL TEST")
    try:
        camry_result, camry_time = benchmark_camry()
    except Exception as e:
        print(f"Camry benchmark failed: {e}")
        import traceback
        traceback.print_exc()
        camry_result, camry_time = None, 0
    
    # Test with procedural meshes
    print("\n\n2. PROCEDURAL MESH TESTS")
    procedural_results = benchmark_procedural_meshes()
    
    # Print summary
    print_summary(procedural_results)
    
    print("\nBenchmark complete!")
