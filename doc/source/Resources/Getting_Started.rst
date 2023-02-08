Getting started
===============

Installation
~~~~~~~~~~~~

This package is not yet available on the public PyPI, but you can still install it using ``pip`` with the following:

.. code::

   pip install -U git+https://github.com/pyansys/pyDyna@main

You'll need to be logged into GitHub locally and a member of the `PyAnsys Organization <https://github.com/pyansys>`_.

Alternatively, if you need to modify the respository locally (or want to do local development), you can clone it and install it in "development" mode with:

.. code::

   git clone https://github.com/pyansys/pyDyna
   cd pyDyna
   pip install -e .

Note the ``-e`` flag, which denotes that you're in development mode. You can make changes in the local ``pyDyna`` directory and those will be reflected within your Python kernel after you restart it.
