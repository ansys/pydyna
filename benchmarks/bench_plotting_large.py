"""Quick test with a very large mesh."""
import time
from bench_plotting import create_hex_mesh
from ansys.dyna.core.lib.deck_plotter import get_polydata

print("Testing with XXL mesh (100x100x100 = 1M elements)...")
print("This will take a while...")

nx, ny, nz = 100, 100, 100
n_elems = nx * ny * nz

start = time.perf_counter()
deck = create_hex_mesh(nx, ny, nz)
create_time = time.perf_counter() - start
print(f"\nMesh created: {n_elems:,} elements in {create_time:.1f}s")

# Test WITHOUT extract_surface
print("\nTesting WITHOUT extract_surface...")
start = time.perf_counter()
result_full = get_polydata(deck, extract_surface=False)
time_full = time.perf_counter() - start
print(f"Time: {time_full:.1f}s, Cells: {result_full.n_cells:,}, Points: {result_full.n_points:,}")

# Test WITH extract_surface
print("\nTesting WITH extract_surface...")
start = time.perf_counter()
result_surf = get_polydata(deck, extract_surface=True)
time_surf = time.perf_counter() - start
print(f"Time: {time_surf:.1f}s, Cells: {result_surf.n_cells:,}, Points: {result_surf.n_points:,}")

speedup = time_full / time_surf
reduction = (1 - result_surf.n_cells / result_full.n_cells) * 100

print(f"\n{'='*60}")
print(f"SPEEDUP: {speedup:.2f}x faster")
print(f"CELL REDUCTION: {reduction:.1f}% ({result_full.n_cells:,} -> {result_surf.n_cells:,})")
print(f"{'='*60}")
