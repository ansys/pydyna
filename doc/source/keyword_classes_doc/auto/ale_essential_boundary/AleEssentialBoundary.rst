





:class:`AleEssentialBoundary`
=============================


.. py:class:: ale_essential_boundary.AleEssentialBoundary(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA ALE_ESSENTIAL_BOUNDARY keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: AleEssentialBoundary

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~id`
            - Get or set the Set ID defining a part, part set or segment set ID of the ALE mesh boundary.
          * - :py:attr:`~idtype`
            - Get or set the Type of set ID:
          * - :py:attr:`~ictype`
            - Get or set the Constraint type:
          * - :py:attr:`~iexcl`
            - Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.


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

    from ale_essential_boundary import AleEssentialBoundary

Property detail
---------------

.. py:property:: id
   :type: Optional[int]


   
   Get or set the Set ID defining a part, part set or segment set ID of the ALE mesh boundary.
















   ..
       !! processed by numpydoc !!

.. py:property:: idtype
   :type: int


   
   Get or set the Type of set ID:
   EQ.0: part set ID (PSID).
   EQ.1: part ID (PID).
   EQ.2: segment set ID (SGSID).
















   ..
       !! processed by numpydoc !!

.. py:property:: ictype
   :type: int


   
   Get or set the Constraint type:
   EQ.1: No flow through all directions.
   EQ.2: No flow through normal direction. (slip condition)
















   ..
       !! processed by numpydoc !!

.. py:property:: iexcl
   :type: Optional[int]


   
   Get or set the Segment Set ID to be excluded from applying ALE essential boundary condition. For example, inlet/outlet segments.
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'ALE'


.. py:attribute:: subkeyword
   :value: 'ESSENTIAL_BOUNDARY'






