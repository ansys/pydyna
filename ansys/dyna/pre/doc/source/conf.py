# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
import pathlib
#sys.path.insert(0, os.path.dirname(os.path.abspath('..')))
sys.path.insert(0, 'D:\\pyDyna\\ansys\\dyna')
print("D:\\pyDyna\\ansys\\dyna\\pre")
sys.path.insert(0, 'D:\\pyDyna\\ansys\\dyna\\pre\\doc\\Resources')
local_path = os.path.dirname(os.path.realpath(__file__))
module_path = pathlib.Path(local_path)
root_path = module_path.parent.parent
sys.path.append(os.path.abspath(os.path.join(local_path)))
sys.path.append(os.path.join(root_path))

# -- Project information -----------------------------------------------------

project = 'pyDyna-pre'
copyright = '2022, Ansys.inc'
author = 'Ansys'

# The full version, including alpha/beta/rc tags
release = 'MIT License'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = [
    "sphinx.ext.autodoc",
]

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = []


# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = 'alabaster'

# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = ['_static']