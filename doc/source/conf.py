#import ansys.dyna.solver as solver


import os
import sys

from ansys_sphinx_theme import pyansys_logo_black

sys.path.insert(0,os.path.abspath('./../../ansys/dyna/pre'))
sys.path.insert(0,os.path.abspath('./../../ansys/dyna'))

# Project information
project = 'pyansys_library_'
copyright = '(c) 2021 ANSYS, Inc. All rights reserved'
author = 'ANSYS Inc.'
#release = version = solver.__version__

# optionally use the default pyansys logo
html_logo = 'https://docs.pyansys.com/_static/pyansys-logo-black-cropped.png'

html_theme = 'pyansys_sphinx_theme'

# specify the location of your github repo
html_theme_options = {
    "github_url": "https://github.com/pyansys/pyDynaSolver",
    "show_prev_next": False
}

# Sphinx extensions
extensions = [
    'sphinx.ext.autodoc',
    'sphinx_gallery.gen_gallery',
    'numpydoc',
    'sphinx.ext.autosummary',
]

sphinx_gallery_conf = {
            # convert rst to md for ipynb
            "pypandoc": True,
            # path to your examples scripts
            "examples_dirs": ["../../examples/"],
            # path where to save gallery generated examples
            "gallery_dirs": ["examples"],
            # Patter to search for examples files
            "filename_pattern": r"/pre/.py",
            # Patter to omit some files
            "ignore_pattern": r"/*_data.py",
            # Remove the "Download all examples" button from the top level gallery
            "download_all_examples": False,
            # Sort gallery examples by file name instead of number of lines (default)
            #"within_subsection_order": FileNameSortKey,
            # directory where function granular galleries are stored
            "backreferences_dir": None,
            # Modules for which function level galleries are created.  In
            "doc_module": "ansys-mapdl-core",
            "image_scrapers": ("pyvista", "matplotlib"),
            "thumbnail_size": (350, 350),
            # 'first_notebook_cell': ("%matplotlib inline\n"
            #                         "from pyvista import set_plot_theme\n"
            #                         "set_plot_theme('document')"),
        }

# Numpydoc config
numpydoc_show_class_members = False  # we take care of autosummary on our own

# Favicon
html_favicon = "favicon.png"

# The suffix(es) of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# -- Options for HTML output -------------------------------------------------
html_short_title = html_title = "PyAEDT"
html_show_sourcelink = True
html_theme = "ansys_sphinx_theme"
html_logo = pyansys_logo_black

html_theme_options = {
    "github_url": "https://github.com/pyansys/pyaedt",
    "show_prev_next": False,
    "show_breadcrumbs": True,
    "additional_breadcrumbs": [
        ("PyAnsys", "https://docs.pyansys.com/"),
    ],
}

# static path
html_static_path = ['_static']

# We have our own custom templates
templates_path = ['_templates']
