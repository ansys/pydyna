.. _getting_started_pydyna_keywords:

Getting started
===============

Overview
--------
The `keywords`` module of PyDyna provides Python libraries to build an Ansys LS-DYNA keyword deck.

Usage
-----
Here's an example of how you can generate a `*SECTION_TSHELL`` keyword:

.. code:: pycon

   >>> from ansys.dyna.core.keywords import keywords
   >>> shell = keywords.SectionTShell()
   >>> shell
   *SECTION_TSHELL
   $#   secid    elform      shrf       nip     propt        qr     icomp    tshear
                      1       1.0         2       1.0         0         0         0


Examples
--------
Examples showing end-to-end workflows for using PyDyna -
write a deck using the ``keywords`` module and run the solver using the ``run`` module.

#. ``Buckling_Beer_Can``
#. ``John_Reid_Pendulum``
#. ``John_Reid_Pipe``
#. ``Taylor_Bar``

.. - `API reference <api_reference_pydyna_keywords_>`_: Provides API member descriptions and usage examples.
.. - `Examples <examples_pydyna_keywords_>`_: Provides examples showing end-to-end workflows for using PyDyna -
   write a deck using the ``keywords`` module and run the solver using the ``run`` module.

.. LINKS AND REFERENCES
.. .. _api_reference_pydyna_keywords_: https://dyna.docs.pyansys.com/version/dev/keyword_class_documentation.html
.. .. _examples_pydyna_keywords_: https://dyna.docs.pyansys.com/version/dev/keyword_examples/index.html
