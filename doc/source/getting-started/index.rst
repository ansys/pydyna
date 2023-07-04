Getting started
===============

************
Installation
************

Python module
~~~~~~~~~~~~~

The ``ansys.dyna.core`` package currently supports Python 3.8 through
Python 3.10 on Windows, Mac OS, and Linux.

Install the latest release from 
`PyPI <pydyna_pypi_>`_ with:

.. code:: console

   pip install ansys-dyna-core

Alternatively, install the latest from 
`PyDyna GitHub <pydyna_issues_>`_ via:

.. code:: console
   
   pip install git+https://github.com/pyansys/pydyna.git
   

For a local *development* version, install with:

.. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna
   pip install -e .

This allows you to install the ``ansys-dyna-core`` module
and modify it locally and have the changes reflected in your setup
after restarting the Python kernel.

Offline installation
~~~~~~~~~~~~~~~~~~~~
If you lack an internet connection on your install machine, the recommended way
of installing PyDyna is downloading the wheelhouse archive from the 
`Releases Page <pydyna_releases_>`_ for your corresponding
machine architecture.

Each wheelhouse archive contains all the Python wheels necessary to install
PyDyna from scratch on Windows and Linux for Python 3.8 through 3.11. You can install
this on an isolated system with a fresh Python or on a virtual environment.

For example, on Linux with Python 3.8, unzip it and install it with the following:

.. code:: console

   unzip ansys-dyna-core-v0.3.1-wheelhouse-ubuntu-latest-3.8.zip wheelhouse
   pip install ansys-dyna-core -f wheelhouse --no-index --upgrade --ignore-installed

If you're on Windows with Python 3.8, unzip to a ``wheelhouse`` directory and
install using the preceding command.

Consider installing using a `virtual environment <using_venv_>`_.

.. include:: ../../../docker/pre/README.rst
.. include:: ../../../docker/solver/README.rst

.. LINKS
.. _pydyna_pypi: https://pypi.org/projects/ansys-dyna-core/
.. _pydyna_releases: https://github.com/ansys/pydyna/releases
.. _pydyna_issues: https://github.com/ansys/pydyna/issues
.. _using_venv: https://docs.python.org/3/library/venv.html