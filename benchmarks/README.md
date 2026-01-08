# Performance Benchmarks

This directory contains performance benchmarks for PyDyna components.

## Plotting Benchmarks

### `bench_plotting.py`
Comprehensive benchmark for the deck plotting system. Tests both real-world models (Camry) and procedurally generated meshes.

**Usage:**
```bash
python benchmarks/bench_plotting.py
```

**What it tests:**
- Real-world model (Camry roof crush) with ~1M nodes and ~975K shells
- Procedurally generated hex meshes of varying sizes (1K to 64K elements)
- Compares performance with and without `extract_surface()` optimization
- Reports speedup, cell reduction, and memory usage

### `bench_plotting_large.py`
Tests with very large meshes (1M elements) to demonstrate scaling behavior.

**Usage:**
```bash
python benchmarks/bench_plotting_large.py
```

**Warning:** This creates a 1M element mesh which takes significant time (~13 minutes for mesh creation).

## Results Interpretation

The `extract_surface()` optimization in `get_polydata()`:
- Removes interior solid cells (not visible anyway)
- Reduces cell count by 40-94% depending on mesh topology
- Provides 1.05-1.13x speedup for grid creation
- **Major benefit**: Dramatically faster rendering/interaction in visualization tools
- **Major benefit**: Significant memory reduction for large solid meshes

For shell-only models (like Camry), no performance impact as optimization only applies to solids.
