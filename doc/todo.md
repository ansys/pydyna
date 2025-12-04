Plan: Accelerate PyDyna Documentation Build (Revised)

Steps
Add build timing instrumentation - NOT DONE. Insert time wrapper in Makefile and workflows to measure before/after speedup and identify remaining bottlenecks.

Parallel autodoc processing - Custom Sphinx extension for true parallel autodoc? Standard -j auto only parallelizes pages, not individual automodule directives within a page.
