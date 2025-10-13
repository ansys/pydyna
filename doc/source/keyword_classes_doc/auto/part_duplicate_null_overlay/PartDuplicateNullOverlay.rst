





:class:`PartDuplicateNullOverlay`
=================================


.. py:class:: part_duplicate_null_overlay.PartDuplicateNullOverlay(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA PART_DUPLICATE_NULL_OVERLAY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: PartDuplicateNullOverlay

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~ptype`
            - Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
          * - :py:attr:`~typeid`
            - Get or set the ID of part or part set to be duplicated.
          * - :py:attr:`~idpoff`
            - Get or set the ID offset of newly created parts
          * - :py:attr:`~ideoff`
            - Get or set the ID offset of newly created elements.
          * - :py:attr:`~density`
            - Get or set the Density.
          * - :py:attr:`~e`
            - Get or set the Youngs modulus
          * - :py:attr:`~pr`
            - Get or set the Poissons ratio.


   .. tab-item:: Attributes

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~keyword`
            - 
          * - :py:attr:`~subkeyword`
            - 






Import detail
-------------

.. code-block:: python

    from part_duplicate_null_overlay import PartDuplicateNullOverlay

Property detail
---------------

.. py:property:: ptype
   :type: str


   
   Get or set the Set to "PART" to duplicate a single part or "PSET" to duplicate a part set.
















   ..
       !! processed by numpydoc !!

.. py:property:: typeid
   :type: Optional[int]


   
   Get or set the ID of part or part set to be duplicated.
















   ..
       !! processed by numpydoc !!

.. py:property:: idpoff
   :type: int


   
   Get or set the ID offset of newly created parts
















   ..
       !! processed by numpydoc !!

.. py:property:: ideoff
   :type: int


   
   Get or set the ID offset of newly created elements.
















   ..
       !! processed by numpydoc !!

.. py:property:: density
   :type: float


   
   Get or set the Density.
















   ..
       !! processed by numpydoc !!

.. py:property:: e
   :type: float


   
   Get or set the Youngs modulus
















   ..
       !! processed by numpydoc !!

.. py:property:: pr
   :type: float


   
   Get or set the Poissons ratio.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'PART'


.. py:attribute:: subkeyword
   :value: 'DUPLICATE_NULL_OVERLAY'






