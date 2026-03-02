# Testing guide for agents

`run` tests use a special marker to constrain them to environments where ls-dyna is available

codegen tests should not be collected by default, these are tests used by the codegen validation system and should *not* be used when testing pydyna. codegen validation