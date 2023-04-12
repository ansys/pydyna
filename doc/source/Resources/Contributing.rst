Contributing
============

Overall guidance on contributing to a PyAnsys repository appears in the
`Contributing <https://dev.docs.pyansys.com/overview/contributing.html>`_ topic
in the *PyAnsys Developer's Guide*. Ensure that you are thoroughly familiar
with it and all `Guidelines and Best Practices <https://dev.docs.pyansys.com/guidelines/index.html>`_
before attempting to contribute to PyDyna.

The following contribution information is specific to PyDyna.

Clone the repository
--------------------

To clone and install the latest PyDyna release in development mode, run:

.. code::

    git clone https://github.com/pyansys/PyDyna
    cd PyDyna
    python -m pip install --upgrade pip
    pip install -e .


Post issues
-----------

Use the `PyDyna Issues <https://github.com/pyansys/PyDyna/issues>`_
page to submit questions, report bugs, and request new features. When possible, you
should use these issue templates:

* Bug report template
* Feature request template

If your issue does not fit into these categories, create your own issue.

To reach the PyAnsys team, email `pyansys.core@ansys.com <pyansys.core@ansys.com>`_.

View documentation
------------------

Documentation for the latest stable release of PyDyna is hosted at
`PyDyna Documentation <https://dyna.docs.pyansys.com>`_.

Documentation for the latest development version, which tracks the
``main`` branch, is hosted at `Development PyDyna Documentation <https://dyna.docs.pyansys.com/dev/>`_.
This version is automatically kept up to date via GitHub actions.

Code style
----------

PyDyna follows the PEP8 standard as outlined in the `PyAnsys Development Guide
<https://dev.docs.pyansys.com>`_ and implements style checking using
`pre-commit <https://pre-commit.com/>`_.

To ensure your code meets minimum code styling standards, run::

  pip install pre-commit
  pre-commit run --all-files

You can also install this as a pre-commit hook by running::

  pre-commit install

This way, it's not possible for you to push code that fails the style checks. For example::

  $ pre-commit install
  $ git commit -am "added my cool feature"
  black....................................................................Passed
  blacken-docs.............................................................Passed
  codespell................................................................Passed
  flake8...................................................................Passed
  isort....................................................................Passed
  check for merge conflicts................................................Passed
  debug statements (python)................................................Passed
  Validate GitHub Workflows................................................Passed