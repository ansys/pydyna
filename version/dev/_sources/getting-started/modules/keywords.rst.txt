.. _getting_started_pydyna_keywords:

Keywords
=========

The ``keywords`` module can be used to interact with LS-DYNA keywords.

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


Multi-row keywords
------------------

Some LS-DYNA keywords accept multiple data rows under a single keyword header.
PyDyna reads all rows into a ``pandas.DataFrame`` accessible via the ``.elements``
(or equivalent) attribute.

For example, ``*ELEMENT_MASS_PART`` and ``*ELEMENT_MASS_PART_SET`` can list
several part IDs in one block:

.. code:: pycon

   >>> from ansys.dyna.core import Deck
   >>> deck = Deck()
   >>> deck.loads("""*KEYWORD
   ... *ELEMENT_MASS_PART
   ...    101           500.0             0.0       0
   ...    102           200.0             0.0       0
   ... *END""")
   >>> kw = for elem in deck.get_kwds_by_full_type("ELEMENT", "MASS_PART"):
   >>> print(kw.elements)
      pid  addmass  finmass  lcid
   0  101    500.0      0.0     0
   1  102    200.0      0.0     0

All rows are preserved in the DataFrame and round-trip correctly through
:meth:`ansys.dyna.core.Deck.write()`.

.. code:: pycon

   >>> output = deck.write()
   >>> print(output)
   *KEYWORD
   *ELEMENT_MASS_PART
   $#     pid         addmass         finmass      lcid
        101         500.000           0.000         0
        102         200.000           0.000         0
   *END


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
