Getting started
===============
To run PyDYNA, you must have an installation of Ansys LS-DYNA.
PyDYNA supports LS-DYNA 2023 R2 and later.

For information on getting a licensed copy of LS-DYNA, see
the `Ansys LS-DYNA <https://www.ansys.com/products/structures/ansys-ls-dyna>`_
page on the Ansys website.

Installation
============
To use PyDYNA, you must install Docker images for the ``pre`` and ``solver``
services and the package.

Install Docker image for the ``pre`` service
--------------------------------------------
To launch the ``pre`` service locally, you must have Docker installed
on your machine.

.. caution::

   The ``pre`` service is available only as a Linux Docker image. 
   Make sure that your Docker engine is configured to run Linux Docker images.

For information on installing the Docker container for the ``pre`` service,
see the ``README.rst`` file in the repository's ``docker/pre`` directory.

Install Docker image for the ``solver`` service
-----------------------------------------------
For information on installing the Docker container for the ``solver`` service,
see the ``README.rst`` file in the repository's ``docker/solver`` directory.

Install the package
-------------------
The ``ansys.dyna.core`` package supports Python 3.8 through
Python 3.11 on Windows, Linux, and MacOS.

You should consider installing PyDYNA in a virtual environment.
For more information, see Python's
`venv -- Creation of virtual environments <https://docs.python.org/3/library/venv.html>`_.

PyDYNA has three installation modes: user, developer, and offline.

Install in user mode
~~~~~~~~~~~~~~~~~~~~

Before installing PyDYNA in user mode, make sure you have the latest version of
`pip`_ with this command:

.. code:: bash

   python -m pip install -U pip

Then, install PyDYNA with this command:

.. code:: bash

   python -m pip install ansys-dyna-core

.. caution::

    PyDYNA is currently hosted in a private PyPI repository. You must provide the index
    URL to the private PyPI repository: ``https://pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/``.

    If access to this package registry is needed, email `pyansys.core@ansys.com <mailto:pyansys.core@ansys.com>`_
    to request access. The PyAnsys team can provide you with a read-only token.
    
	Once you have the token, run this command, replacing ``${PRIVATE_PYPI_ACCESS_TOKEN}`` with the
	read-only token:

    .. code:: bash

        pip install ansys-dyna-core --index-url=https://${PRIVATE_PYPI_ACCESS_TOKEN}@pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/

Install in developer mode
~~~~~~~~~~~~~~~~~~~~~~~~~

Installing PyDYNA in developer mode allows you to modify the source and enhance it.

.. note::
   
    Before contributing to the project, ensure that you are thoroughly familiar
    with the `PyAnsys Developer's Guide`_.

Start by cloning and installing the repository with these commands:

.. code::

   git clone https://github.com/pyansys/pyDyna
   cd pyDyna
   pip install -e .

Install in offline mode
~~~~~~~~~~~~~~~~~~~~~~~

If you lack an internet connection on your installation machine (or you do not have access
to the private Ansys PyPI packages repository), you should install PyDYNA by downloading
the wheelhouse archive for your corresponding machine architecture from the
`Releases Page <https://github.com/pyansys/pydyna/releases>`_.

Each wheelhouse archive contains all the Python wheels necessary to install
PyDYNA from scratch on Windows and Linux for Python 3.8 through 3.11. You can install
PyDYNA on an isolated system with a fresh Python installation or on a virtual environment.

For example, on Linux with Python 3.8, unzip the wheelhouse archive and install PyDYNA
with these commands:

.. code:: bash

    unzip ansys-dyna-core-v0.3.5-wheelhouse-Linux-3.8.zip -d wheelhouse
    pip install ansys-dyna-core -f wheelhouse --no-index --upgrade --ignore-installed

If you're on Windows with Python 3.8, unzip the wheelhouse archive to a ``wheelhouse``
directory and install PyDYNA using the preceding command.


.. LINKS
.. _pydyna_pypi: https://pypi.org/projects/ansys-dyna-core/
.. _pydyna_repo: https://github.com/ansys/pydyna/
.. _pydyna_releases: https://github.com/ansys/pydyna/releases
.. _pydyna_issues: https://github.com/ansys/pydyna/issues
