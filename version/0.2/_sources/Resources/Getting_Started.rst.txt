Getting started
===============

Installation
~~~~~~~~~~~~

This package is not yet available on the public PyPI, but you can still install
it using ``pip`` from our private PyPI repository.

The following on Windows:

.. code::

   set PYANSYS_PYPI_PRIVATE_PAT=<REDACTED>
   set INDEX_URL=https://%PYANSYS_PYPI_PRIVATE_PAT%@pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/
   python -m pip install ansys-dyna-core --index-url %INDEX_URL%

And if you are running Linux:

.. code::

   PYANSYS_PYPI_PRIVATE_PAT=<REDACTED>
   export INDEX_URL='https://$PYANSYS_PYPI_PRIVATE_PAT@pkgs.dev.azure.com/pyansys/_packaging/pyansys/pypi/simple/'
   python -m pip install ansys-dyna-core --index-url $INDEX_URL

Email your friendly PyAnsys team member for the ``PYANSYS_PYPI_PRIVATE_PAT``
at `pyansys.core@ansys.com <mailto:pyansys.core@ansys.com>`_ or send us a message via Teams.

**Installing from git**

If you have ``git`` installed and want the bleeding edge version:

.. code::

   pip install -U git+https://github.com/pyansys/pyDyna@main

You need to be logged into GitHub locally and be a member of the `PyAnsys Organization <https://github.com/pyansys>`_.

Alternatively, if you need to modify the repository locally (or want to
do local development), you can clone it and install it in "development" mode with:

.. code::

   git clone https://github.com/pyansys/pyDyna
   cd pyDyna
   pip install -e .

Note the ``-e`` flag, which denotes that you are in development mode.
You can make changes in the local ``pyDyna`` and have them reflected
in your local install of PyDyna.
