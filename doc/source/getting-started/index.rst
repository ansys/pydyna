Getting started
===============
To run PyDYNA, you must have an installation of Ansys LS-DYNA.
PyDYNA supports LS-DYNA 2023 R2 and later.

For information on getting a licensed copy of LS-DYNA, see
the `Ansys LS-DYNA <https://www.ansys.com/products/structures/ansys-ls-dyna>`_
page on the Ansys website.

Installation
------------

The ``ansys.dyna.core`` package supports Python 3.8 through
Python 3.11 on Windows, Linux, and MacOS.

You should consider installing PyDYNA in a virtual environment.
For more information, see Python's
`venv -- Creation of virtual environments <https://docs.python.org/3/library/venv.html>`_.

Install the latest release from `PyPI <pydyna_pypi_>`_ with this
command:

.. code:: console

   pip install ansys-dyna-core

Alternatively, install the latest release from the
`GitHub repository <pydyna_repo_>`_ with this command:

.. code:: console
   
   pip install git+https://github.com/pyansys/pydyna.git
   
If you plan on doing local *development* of PyDYNA with Git,
install the latest ``ansys-dyna.core`` package with these
commands:

.. code:: console

   git clone https://github.com/pyansys/pydyna.git
   cd pydyna
   pip install -e .

The preceding commands clone and install the package, allowing you to modify it
locally. After restarting the Python kernel, your changes are reflected in your
Python setup.

Offline installation
~~~~~~~~~~~~~~~~~~~~
If you lack an internet connection on your installation machine (or you do not have access
to the private Ansys PyPI packages repository), you should install PyDYNA by downloading
the wheelhouse archive for your corresponding machine architecture from the
`Releases Page <https://github.com/pyansys/pydyna/releases>`_ .

Each wheelhouse archive contains all the Python wheels necessary to install
PyDYNA from scratch on Windows and Linux for Python 3.8 through 3.11. You can install
PyDYNA on an isolated system with a fresh Python installation or on a virtual environment.

For example, on Linux with Python 3.8, unzip the wheelhouse archive and install PyDYNA
with these commands:

.. code:: console

   unzip ansys-dyna-core-v0.3.1-wheelhouse-ubuntu-latest-3.8.zip wheelhouse
   pip install ansys-dyna-core -f wheelhouse --no-index --upgrade --ignore-installed

If you're on Windows with Python 3.8, unzip the wheelhouse archive to a ``wheelhouse``
directory and install PyDYNA using the preceding command.

.. include:: ../../../docker/pre/README.rst
.. include:: ../../../docker/solver/README.rst

.. LINKS
.. _pydyna_pypi: https://pypi.org/projects/ansys-dyna-core/
.. _pydyna_repo: https://github.com/ansys/pydyna/
.. _pydyna_releases: https://github.com/ansys/pydyna/releases
.. _pydyna_issues: https://github.com/ansys/pydyna/issues
