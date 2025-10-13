





:class:`CeseBoundarySegment`
============================


.. py:class:: cese_boundary_segment.CeseBoundarySegment(**kwargs)

   Bases: :py:obj:`ansys.dyna.core.lib.keyword_base.KeywordBase`


   
   DYNA CESE_BOUNDARY_SEGMENT keyword
















   ..
       !! processed by numpydoc !!


.. py:currentmodule:: CeseBoundarySegment

Overview
--------

.. tab-set::




   .. tab-item:: Properties

      .. list-table::
          :header-rows: 0
          :widths: auto

          * - :py:attr:`~n1`
            - Get or set the Node IDs defining a segment.
          * - :py:attr:`~n2_`
            - Get or set the Node IDs defining a segment.
          * - :py:attr:`~n3`
            - Get or set the Node IDs defining a segment.
          * - :py:attr:`~n4`
            - Get or set the Node IDs defining a segment.
          * - :py:attr:`~dof_`
            - Get or set the Node IDs defining a segment.
          * - :py:attr:`~lcid`
            - Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
          * - :py:attr:`~sf`
            - Get or set the Load curve scale factor.  (default=1.0).


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

    from cese_boundary_segment import CeseBoundarySegment

Property detail
---------------

.. py:property:: n1
   :type: Optional[int]


   
   Get or set the Node IDs defining a segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n2_
   :type: Optional[int]


   
   Get or set the Node IDs defining a segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n3
   :type: Optional[int]


   
   Get or set the Node IDs defining a segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: n4
   :type: Optional[int]


   
   Get or set the Node IDs defining a segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: dof_
   :type: Optional[int]


   
   Get or set the Node IDs defining a segment.
















   ..
       !! processed by numpydoc !!

.. py:property:: lcid
   :type: Optional[int]


   
   Get or set the Load curve ID to describe the variable value versus time, see *DEFINE_ CURVE.
















   ..
       !! processed by numpydoc !!

.. py:property:: sf
   :type: float


   
   Get or set the Load curve scale factor.  (default=1.0).
















   ..
       !! processed by numpydoc !!



Attribute detail
----------------

.. py:attribute:: keyword
   :value: 'CESE'


.. py:attribute:: subkeyword
   :value: 'BOUNDARY_SEGMENT'






