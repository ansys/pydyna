This is where pydyna solver lives

   - [ ]  Locate all code, documentation, and tests related to the solver module.
   - [ ]  Add a deprecation warning in the solver module’s code (e.g., warnings.warn("solver is deprecated", DeprecationWarning)).
   - [ ]  Update all docstrings and module-level documentation in solver to indicate deprecation.
   - [ ]  Mark the solver module as deprecated in the main README and any user-facing documentation.
   - [ ]  Update or annotate any examples or tutorials that use solver to warn users of deprecation.
   - [ ]  Update tests that import or use solver to expect deprecation warnings.
   - [ ]  Add a deprecation notice to CHANGELOG.md and, if applicable, to release notes.
   - [ ]  Communicate a timeline for removal and suggest alternatives or migration paths if available.
   - [ ]  Run the test suite and verify that no critical functionality outside solver is broken.
   - [ ]  Review and merge the deprecation changes.