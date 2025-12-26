"""Profile the Sphinx documentation build."""

import os
import sys
from pathlib import Path
from pyinstrument import Profiler

# Set environment variables before importing Sphinx
os.environ['BUILD_EXAMPLES'] = 'false'
os.environ['BUILD_AUTOKEYWORDS_API'] = 'true'
os.environ['SPHINXJOBS'] = '1'

# Add source directory to path
doc_dir = Path(__file__).parent
source_dir = doc_dir / 'source'
build_dir = doc_dir / '_build'

# Start profiling
profiler = Profiler()
profiler.start()

# Import and run Sphinx
from sphinx.cmd.build import main as sphinx_main

sys.argv = [
    'sphinx-build',
    '-M', 'html',
    str(source_dir),
    str(build_dir),
    '-j', '1'
]

try:
    sphinx_main(sys.argv[1:])
finally:
    # Stop profiling and save results
    profiler.stop()
    
    # Print to console
    print("\n" + "=" * 80)
    print("PROFILING RESULTS")
    print("=" * 80)
    profiler.print(show_all=False)
    
    # Save HTML report
    html_output = build_dir / 'profile.html'
    with open(html_output, 'w') as f:
        f.write(profiler.output_html())
    print(f"\nHTML profile saved to: {html_output}")
