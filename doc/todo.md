Plan: Autodoc for generated keywords

## Performance Optimization Plan

**Goal**: Optimize PyDyna documentation build to handle 3,214+ auto-generated keyword classes efficiently.

**Problem**: Full build with auto-keywords takes 30+ minutes and uses excessive RAM. This makes iteration too slow for optimization work.

**Strategy**: Use representative subset for rapid iteration, optimize on subset, then validate on full build.

### Learnings

- **AutoAPI caching**: Must `rm -rf doc/source/api` before subset builds, otherwise Sphinx uses cached docs from previous full build.
- **Subset selection**: boundary (168), contact (155), control (216) = 539 keywords provides good representation (~17% of total).
- **Clean vs incremental**: Incremental builds (no `rm -rf source/api _build`) are 18% faster (154.57s → 126.66s).
- **Parallelization impact**: No scaling observed by increasing SPHINXJOBS from 1 to 4 to 8

### Steps

1. ✅ **Generate auto keywords for a specific subset using the domain feature from the codegen CLI** (e.g. boundary/*, contact/*, and control/*).
   - Implementation: Added `--subset` CLI option to `codegen/generate.py` accepting comma-delimited domains
   - Usage: `python codegen/generate.py --subset "boundary,contact,control"`
   - Result: Generates 536 classes in 3 domains

2. ✅ **Add timing instrumentation** to `doc/make.bat` and `doc/Makefile` with timestamps before/after each major phase, and modify `conf.py` to add Sphinx event handlers (`autodoc-process-docstring`, `source-read`, etc.) that log phase durations to `doc/_build/timing.log`.
   - Implementation: Added `setup()` function in `conf.py` with event handlers for builder-inited, env-get-outdated, env-before-read-docs, doctree-resolved, build-finished
   - Output: Logs to `doc/_build/timing.log` with phase breakdown and total document count

3. ✅ **Establish baseline metrics** by running 3 timed builds with the subset: (a) current configuration, (b) with `SPHINXJOBS=4`, (c) with `SPHINXJOBS=8`, measuring total time, memory usage, and time per file to identify optimal parallelization.
   - **Baseline metrics (1,461 documents, boundary/contact/control subset)**:
     - Clean build (rm -rf source/api _build): ~154s total (41s read-docs, 113s process-doctrees)
     - SPHINXJOBS=1: 128.94s total (36.05s read, 92.26s process) - FASTEST
     - SPHINXJOBS=4: 147.34s total (40.46s read, 106.38s process)
     - SPHINXJOBS=8: 134.88s total (35.74s read, 98.66s process)
     - SPHINXJOBS=auto (default): 154.57s total (41.02s read, 113.07s process)
   - **Finding**: Serial processing (SPHINXJOBS=1) is fastest for this subset size, likely due to overhead outweighing parallelization benefits

4. ✅ **Profile subset build** using `python -m cProfile -o doc/_build/build.prof` wrapper and `snakeviz` or `pyinstrument` to visualize hotspots, focusing on autodoc import time, docstring processing, and HTML generation phases.
   - **Key findings**:
     - **Wall-clock time**: 7m22s (442 seconds total)
     - **Sphinx-reported time**: 140s (37s read-docs, 102s process-doctrees)
     - **Hidden AutoAPI overhead**: ~302 seconds (68% of total time!) before Sphinx even initializes
     - **AutoAPI phases**: "Reading files" → "Mapping Data" → "Rendering Data" all happen pre-init
     - **Intersphinx**: 6 inventories loaded at startup (python, numpy, matplotlib, imageio, pandas, pytest)
     - **CPU vs I/O**: pyinstrument showed 1070s wall-clock vs 198s CPU time - heavily I/O-bound
   - **Optimization opportunities**:
     1. AutoAPI "Reading files" and "Mapping Data" - 5+ minutes of parsing Python files
     2. Intersphinx inventory loading - network I/O at startup
     3. Autosummary generation running before AutoAPI
     4. Process-doctrees phase (102s) - Jinja2 template compilation, numpydoc processing

5. **Measure, evaluate, and optimize AutoAPI** - The critical path (68% of build time is AutoAPI overhead)

   **Phase A: Measure AutoAPI baseline**
   1. Add detailed timing to AutoAPI phases (Reading files, Mapping Data, Rendering Data)
   2. Measure time per file, identify slowest files to parse
   3. Profile AST parsing with cProfile targeting AutoAPI/astroid modules
   4. Baseline: 539 files in ~302s = 0.56s/file average

   **Phase B: Quick wins (target: 15-25% improvement)**
   1. **Intersphinx caching** (~5-10s):
      - Pre-download 6 inventories to `doc/_build/intersphinx_cache/`
      - Configure `intersphinx_cache_limit = -1`
   2. **Optimize AutoAPI options** (~30-60s):
      - Add `autoapi_options = ['members', 'show-inheritance']`
      - Test `autoapi_python_class_content = 'class'`
      - Test `autoapi_member_order = 'bysource'`
   3. Document `SPHINXJOBS=1` as optimal (no time savings, just documentation)

   **Phase C: Incremental builds (target: 40-60% savings on rebuilds)**
   1. Enable `autoapi_keep_files = True` for caching generated RST files
   2. Test incremental build after no-op (expect 5x+ speedup: 7.5min → <2min)
   3. Test incremental build after single keyword change
   4. Document when clean builds needed (config changes)

   **Phase D: Deep optimization (target: 20-30% improvement)**
   1. Simplify AutoAPI templates in `doc/source/autoapi/`
   2. Investigate astroid caching options
   3. Test if reducing type annotations helps parsing
   4. Consider optimizing keyword class structure (codegen changes)

   **Phase E: Alternative approaches (if insufficient)**
   1. Pre-built documentation cache (commit AutoAPI RST files)
   2. Selective documentation (important vs reference-only classes)
   3. Split documentation builds (manual vs auto-keywords)

   **Success metrics**:
   - Subset build: <3 min (currently 7.5 min) = 60% improvement
   - Full build: <10 min (currently 30+ min) = 67% improvement
   - Incremental (no changes): <1 min = 85% improvement

6. **Validate on full build** by running optimized configuration against all 3,214 auto-keywords, comparing total time and memory usage to establish that improvements scale, then document optimal build settings in `AGENTS.md` and CI workflows.

