#!/bin/bash
# Clean documentation build artifacts without confirmation prompts
rm -rf doc/_build 2>/dev/null
echo "Cleaned doc/_build"
