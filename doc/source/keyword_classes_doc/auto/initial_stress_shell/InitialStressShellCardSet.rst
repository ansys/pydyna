





:class:`InitialStressShellCardSet`
==================================


.. py:class:: initial_stress_shell.InitialStressShellCardSet(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.cards.Cards`


   
   CardSet.
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: InitialStressShellCardSet

Overview
--------

.. tab-set::



   .. tab-item:: Methods

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~add_set`
            - Adds a set to the list of sets.


   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~eid`
            - Get or set the Shell element ID.
          * - :py:attr:`~nplane`
            - Get or set the Number of in plane integration points being output.
          * - :py:attr:`~nthick`
            - Get or set the Number of through thickness integration points.
          * - :py:attr:`~nhisv`
            - Get or set the Number of additional history variables.
          * - :py:attr:`~ntensr`
            - Get or set the Number of components of tensor data taken from the element history variables.
          * - :py:attr:`~large`
            - Get or set the Format size (0:off or 1:on).
          * - :py:attr:`~nthint`
            - Get or set the Number of thermal integration points.
          * - :py:attr:`~nthhsv`
            - Get or set the Number of thermal history variables per thermal integration point..
          * - :py:attr:`~sets`
            - Gets the list of sets.
          * - :py:attr:`~parent`
            - Get the parent keyword.







Import detail
-------------

.. code-block:: python

    from initial_stress_shell import InitialStressShellCardSet

Property detail
---------------

.. py:property:: eid
   :type: Optional[int]


   
   Get or set the Shell element ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: nplane
   :type: int


   
   Get or set the Number of in plane integration points being output.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthick
   :type: int


   
   Get or set the Number of through thickness integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: nhisv
   :type: int


   
   Get or set the Number of additional history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: ntensr
   :type: int


   
   Get or set the Number of components of tensor data taken from the element history variables.
















   ..
       !! processed by numpydoc !!

.. py:property:: large
   :type: int


   
   Get or set the Format size (0:off or 1:on).
















   ..
       !! processed by numpydoc !!

.. py:property:: nthint
   :type: int


   
   Get or set the Number of thermal integration points.
















   ..
       !! processed by numpydoc !!

.. py:property:: nthhsv
   :type: int


   
   Get or set the Number of thermal history variables per thermal integration point..
















   ..
       !! processed by numpydoc !!

.. py:property:: sets
   :type: List[InitialStressShellThicknessLargeCardSet]


   
   Gets the list of sets.
















   ..
       !! processed by numpydoc !!

.. py:property:: parent
   :type: ansys.dyna.core.lib.keyword_base.KeywordBase


   
   Get the parent keyword.
















   ..
       !! processed by numpydoc !!




Method detail
-------------

.. py:method:: add_set(**kwargs)

   
   Adds a set to the list of sets.
















   ..
       !! processed by numpydoc !!




