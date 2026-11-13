Contribute
==========

Overall guidance on contributing to a PyAnsys library appears in the
`Contributing <dev_guide_contributing_>`_ topic
in the *PyAnsys Developer's Guide*. Ensure that you are thoroughly familiar
with this guide before attempting to contribute to PyDYNA.

The following contribution information is specific to PyDYNA.

Clone the repository
--------------------

To clone and install the latest PyDYNA release in development mode, run these commands:

.. code::

    git clone https://github.com/pyansys/pydyna
    cd pydyna
    python -m pip install --upgrade pip
    pip install -e .


Post issues
-----------

Use the `PyDYNA Issues <https://github.com/pyansys/pydyna/issues>`_
page to report bugs and request new features. When possible,
use these issue templates:

* Bug, problem, error: For filing a bug report
* Documentation issue: For requesting modifications to the documentation
* Adding an example: For proposing a new example
* New feature: For requesting enhancements to the code

If your issue does not fit into one of these template categories, you can click
the link for opening a blank issue.

To reach the project support team, email `pyansys.core@ansys.com <pyansys.core@ansys.com>`_.

View documentation
------------------

Documentation for the latest stable release of PyDYNA is hosted at
`PyDYNA documentation <https://dyna.docs.pyansys.com>`_.

In the upper right corner of the documentation’s title bar, there is an option
for switching from viewing the documentation for the latest stable release to
viewing the documentation for the development version or previously released versions.

Adhere to code style
--------------------

PyDYNA follows the PEP8 standard as outlined in the `PyAnsys Developer's Guide
<https://dev.docs.pyansys.com>`_ and implements style checking using
`pre-commit <https://pre-commit.com/>`_.

To ensure your code meets minimum code styling standards, run these commands::

  pip install pre-commit
  pre-commit run --all-files

You can also install this as a pre-commit hook by running this command::

  pre-commit install

This way, it is not possible for you to push code that fails the style checks::

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
