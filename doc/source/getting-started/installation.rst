Installation
============

PyDYNA consists of two modules: ``ansys.dyna.core.run``, and ``ansys.dyna.core.keywords``.

Install the client
------------------
The ``ansys.dyna.core`` package supports Python 3.9 through
Python 3.12 on Windows, Linux, and MacOS.

You should consider installing PyDYNA in a virtual environment.
For more information, see Python's
`venv -- Creation of virtual environments <https://docs.python.org/3/library/venv.html>`_.

PyDYNA has three installation modes: user, developer, and offline.

Install in user mode
~~~~~~~~~~~~~~~~~~~~

Before installing PyDYNA in user mode, make sure you have the latest version of
`pip <https://pip.pypa.io/en/stable/installation/>`_ with this command:

.. code:: bash

   python -m pip install -U pip

Then, install PyDYNA with this command:

.. code:: bash

   python -m pip install ansys-dyna-core


Install in developer mode
~~~~~~~~~~~~~~~~~~~~~~~~~

Installing PyDYNA in developer mode allows you to modify the source and enhance it.

.. note::

    Before contributing to the project, ensure that you are thoroughly familiar
    with the `PyAnsys Developer's Guide <https://dev.docs.pyansys.com/index.html>`_.

Start by cloning and installing the repository with these commands:

.. code::

   git clone https://github.com/ansys/pydyna
   cd pyDyna
   pip install -e .

Install in offline mode
~~~~~~~~~~~~~~~~~~~~~~~

If you lack an internet connection on your installation machine, you should install
PyDYNA by downloading the wheelhouse archive for your corresponding machine
architecture from the `Releases Page <https://github.com/ansys/pydyna/releases>`_.

Each wheelhouse archive contains all the Python wheels necessary to install
PyDYNA from scratch on Windows and Linux for Python 3.9 through 3.12. You can install
PyDYNA on an isolated system with a fresh Python installation or on a virtual environment.

For example, on Linux with Python 3.9, unzip the wheelhouse archive and install PyDYNA
with these commands:

.. code:: bash

    unzip ansys-dyna-core-v0.3.dev0-wheelhouse-Linux-3.9.zip -d wheelhouse
    pip install ansys-dyna-core -f wheelhouse --no-index --upgrade --ignore-installed

If you're on Windows with Python 3.9, unzip the wheelhouse archive to a ``wheelhouse``
directory and install PyDYNA using the preceding command.