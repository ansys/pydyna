





:class:`InitialStrainShellCardSet`
==================================


.. py:class:: initial_strain_shell.InitialStrainShellCardSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.cards.Cards`


   
   CardSet.
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStrainShellCardSet

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Shell element ID.
          * - :py:attr:`~nplane`
            - Get or set the Number of in#plane integration points being output.
          * - :py:attr:`~nthick`
            - Get or set the Number of integration points through the thickness.
          * - :py:attr:`~large`
            - Get or set the Large format flag:
          * - :py:attr:`~ilocal`
            - Get or set the Flag for coordinate system of strain components:
          * - :py:attr:`~strains`
            - Get the table of strains.
          * - :py:attr:`~parent`
            - Get the parent keyword.







Import detail
-------------

.. code-block:: python

    from initial_strain_shell import InitialStrainShellCardSet

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Shell element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: Optional[int]


   
   Get or set the Number of in#plane integration points being output.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthick
   :type: Optional[int]


   
   Get or set the Number of integration points through the thickness.
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Large format flag:
   EQ.0:   off
   EQ.1 : on.Each strain field is twice as long for higher precision.
















   ..
       !! processed by numpydoc !!

.. py:property:: ilocal
   :type: int


   
   Get or set the Flag for coordinate system of strain components:
   EQ.0:   global,
   EQ.1 : local(not supported).
















   ..
       !! processed by numpydoc !!

.. py:property:: strains
   :type: pandas.DataFrame


   
   Get the table of strains.
















   ..
       !! processed by numpydoc !!

.. py:property:: parent
   :type: ansys.dyna.core.lib.keyword_base.KeywordBase


   
   Get the parent keyword.
















   ..
       !! processed by numpydoc !!






