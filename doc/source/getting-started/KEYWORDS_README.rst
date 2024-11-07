.. _getting_started_pydyna_keywords:

Getting started
===============

Overview
--------
The `keywords`` subpackage of PyDyna project provides Python libraries to build a DYNA keyword deck.

Usage
-----
Here's an example of how you use DYNA-Lib to render Python code:

.. code:: pycon

   >>> from ansys.dyna.core.keywords import keywords
   >>> shell = keywords.SectionTShell()
   >>> shell
   *SECTION_TSHELL
   $#   secid    elform      shrf       nip     propt        qr     icomp    tshear
                      1       1.0         2       1.0         0         0         0


See also
--------

- `API reference <api_reference_pydyna_keywords_>`_: Provides API member descriptions and usage examples.
- `Examples <examples_pydyna_keywords_>`_: Provides examples showing end-to-end workflows for using PyDyna -
   write a deck using the ``keywords`` subpackage and run the solver using the ``run`` subpackage.

.. LINKS AND REFERENCES
.. _api_reference_pydyna_keywords_: https://dyna.docs.pyansys.com/version/dev/keyword_class_documentation.html
.. _examples_pydyna_keywords_: https://dyna.docs.pyansys.com/version/dev/keyword_examples/index.html
