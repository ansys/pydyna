





:class:`SetSolidGenerateIncrement`
==================================


.. py:class:: set_solid_generate_increment.SetSolidGenerateIncrement(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA SET_SOLID_GENERATE_INCREMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: SetSolidGenerateIncrement

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~sid`
            - Get or set the Solid element set ID. All solid sets should have a unique set ID.
          * - :py:attr:`~solver`
            - Get or set the EQ.MECH: mechanics.
          * - :py:attr:`~bbeg`
            - Get or set the First solid element ID in block.
          * - :py:attr:`~bend`
            - Get or set the Last solid element ID in block.
          * - :py:attr:`~incr`
            - Get or set the Solid ID increment. Solid IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
          * - :py:attr:`~title`
            - Get or set the Additional title line


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 
          * - :py:attr:`~option_specs`
            - Get the card format type.






Import detail
-------------

.. code-block:: python

    from set_solid_generate_increment import SetSolidGenerateIncrement

Property detail
---------------

.. py:property:: sid
   :type: Optional[int]


   
   Get or set the Solid element set ID. All solid sets should have a unique set ID.
















   ..
       !! processed by numpydoc !!

.. py:property:: solver
   :type: str


   
   Get or set the EQ.MECH: mechanics.
   EQ.CESE: CE/SE compressible fluid flow solver.
   EQ.ICFD: Incompressible fluid flow solver.
















   ..
       !! processed by numpydoc !!

.. py:property:: bbeg
   :type: Optional[int]


   
   Get or set the First solid element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: bend
   :type: Optional[int]


   
   Get or set the Last solid element ID in block.
















   ..
       !! processed by numpydoc !!

.. py:property:: incr
   :type: Optional[int]


   
   Get or set the Solid ID increment. Solid IDs BBEG, BBEG + INCR, BBEG + 2*INCR, and so on through BEND are added to the set.
















   ..
       !! processed by numpydoc !!

.. py:property:: title
   :type: Optional[str]


   
   Get or set the Additional title line
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'SET'


.. py:attribute:: subkeyword
   :value: 'SOLID_GENERATE_INCREMENT'


.. py:attribute:: option_specs

   
   Get the card format type.
















   ..
       !! processed by numpydoc !!





