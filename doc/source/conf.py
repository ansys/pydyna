from ansys.DynaSolver import DynaSolver

# Project information
project = 'pyansys_library_'
copyright = '(c) 2021 ANSYS, Inc. All rights reserved'
author = 'ANSYS Inc.'
release = version = DynaSolver.__version__

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
    'numpydoc',
    'sphinx.ext.autosummary',
]

# Numpydoc config
numpydoc_show_class_members = False  # we take care of autosummary on our own

# The suffix(es) of source filenames.
source_suffix = '.rst'

# The master toctree document.
master_doc = 'index'

# static path
html_static_path = ['_static']

# We have our own custom templates
templates_path = ['_templates']
