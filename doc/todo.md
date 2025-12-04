Plan: Accelerate PyDyna Documentation Build (Revised)
Focus on three high-impact optimizations that work within the existing CI architecture: mock heavy dependencies, parallelize keyword documentation by category, and tune autodoc configuration for speed.

Steps
Mock heavy dependencies in conf.py - Add autodoc_mock_imports = ['pandas', 'numpy', 'pyarrow', 'transformations', 'pyvista', 'ansys.dpf.core'] to prevent expensive module initialization during autodoc introspection without breaking import resolution.

Split monolithic autosummary into category files - Modify codegen/keyword_generation/create_autokeyword_rst.py to generate one RST file per category (airbag.rst, contact.rst, mat.rst, etc.) instead of single 22K-line index.rst. Update main index to use toctree for categories, enabling Sphinx -j auto to parallelize across ~51 files.

Optimize autodoc settings in conf.py - Set autodoc_typehints = 'signature', autodoc_member_order = 'bysource', and autodoc_default_options = {'exclude-members': '__weakref__,__dict__,__module__', 'inherited-members': False} to minimize per-class processing overhead.

Add build timing instrumentation - Insert time wrapper in Makefile and workflows to measure before/after speedup and identify remaining bottlenecks.

Further Considerations
Parallel autodoc processing - Custom Sphinx extension for true parallel autodoc? Standard -j auto only parallelizes pages, not individual automodule directives within a page.